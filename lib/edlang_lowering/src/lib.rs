use std::collections::HashMap;

use ast::ModuleStatement;
use common::{BodyBuilder, BuildCtx, IdGenerator};
use edlang_ast as ast;
use edlang_ir as ir;
use ir::{
    BasicBlock, Body, ConstData, ConstKind, DefId, Local, LocalKind, Operand, Place, ProgramBody,
    RValue, Statement, StatementKind, Terminator, TypeInfo, TypeKind,
};

mod common;
mod prepass;

pub fn lower_modules(modules: &[ast::Module]) -> ProgramBody {
    let mut ctx = BuildCtx::default();

    // resolve symbols
    for module in modules {
        ctx = prepass::prepass_module(ctx, module);
    }

    // resolve imports
    for module in modules {
        ctx = prepass::prepass_imports(ctx, module);
    }

    for mod_def in modules {
        let id = *ctx
            .body
            .top_level_module_names
            .get(&mod_def.name.name)
            .expect("module should exist");

        ctx = lower_module(ctx, mod_def, id);
    }

    ctx.body
}

fn lower_module(mut ctx: BuildCtx, module: &ast::Module, id: DefId) -> BuildCtx {
    let body = ctx.body.modules.get(&id).unwrap();

    // fill fn sigs
    for content in &module.contents {
        if let ModuleStatement::Function(fn_def) = content {
            let fn_id = *body.symbols.functions.get(&fn_def.name.name).unwrap();

            let mut args = Vec::new();
            let ret_type;

            for arg in &fn_def.params {
                let ty = lower_type(&ctx, &arg.arg_type);
                args.push(ty);
            }

            if let Some(ty) = &fn_def.return_type {
                ret_type = lower_type(&ctx, ty);
            } else {
                ret_type = TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Unit,
                };
            }

            ctx.body.function_signatures.insert(fn_id, (args, ret_type));
        }
    }

    for content in &module.contents {
        match content {
            ModuleStatement::Constant(_) => todo!(),
            ModuleStatement::Function(fn_def) => {
                ctx = lower_function(ctx, fn_def, id);
            }
            ModuleStatement::Struct(_) => todo!(),
            // ModuleStatement::Type(_) => todo!(),
            ModuleStatement::Module(_mod_def) => {}
        }
    }

    ctx
}

fn lower_function(ctx: BuildCtx, func: &ast::Function, module_id: DefId) -> BuildCtx {
    let mut builder = BodyBuilder {
        body: Body {
            blocks: Default::default(),
            locals: Default::default(),
            name: func.name.name.clone(),
            def_id: {
                let body = ctx.body.modules.get(&module_id).unwrap();
                *body.symbols.functions.get(&func.name.name).unwrap()
            },
            is_pub: func.is_public,
            is_extern: func.is_extern,
            fn_span: func.span,
        },
        local_module: module_id,
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ctx,
    };

    let fn_id = builder.body.def_id;

    let (args_ty, ret_ty) = builder
        .ctx
        .body
        .function_signatures
        .get(&fn_id)
        .unwrap()
        .clone();

    // store args ret

    builder.ret_local = builder.body.locals.len();
    builder.body.locals.push(Local::new(
        None,
        LocalKind::ReturnPointer,
        ret_ty.clone(),
        None,
        false,
    ));

    for (arg, ty) in func.params.iter().zip(args_ty) {
        builder
            .name_to_local
            .insert(arg.name.name.clone(), builder.body.locals.len());
        builder.body.locals.push(Local::new(
            Some(arg.name.span),
            LocalKind::Arg,
            ty,
            Some(arg.name.name.clone()),
            false,
        ));
    }

    // Get all user defined locals
    for stmt in &func.body.body {
        if let ast::Statement::Let(info) = stmt {
            let ty = lower_type(&builder.ctx, &info.r#type);
            builder
                .name_to_local
                .insert(info.name.name.clone(), builder.body.locals.len());
            builder.body.locals.push(Local::new(
                Some(info.name.span),
                LocalKind::Temp,
                ty,
                Some(info.name.name.clone()),
                info.is_mut,
            ));
        }
    }

    for stmt in &func.body.body {
        lower_statement(&mut builder, stmt, &ret_ty);
    }

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.def_id);
    ctx.body.functions.insert(body.def_id, body);
    ctx
}

fn lower_statement(builder: &mut BodyBuilder, info: &ast::Statement, ret_type: &TypeInfo) {
    match info {
        ast::Statement::Let(info) => lower_let(builder, info),
        ast::Statement::Assign(info) => lower_assign(builder, info),
        ast::Statement::For(_) => todo!(),
        ast::Statement::While(_) => todo!(),
        ast::Statement::If(_) => todo!(),
        ast::Statement::Return(info) => lower_return(builder, info, ret_type),
        ast::Statement::FnCall(info) => {
            lower_fn_call(builder, info);
        }
    }
}

fn lower_let(builder: &mut BodyBuilder, info: &ast::LetStmt) {
    let ty = lower_type(&builder.ctx, &info.r#type);
    let rvalue = lower_expr(builder, &info.value, Some(&ty));
    let local_idx = builder.name_to_local.get(&info.name.name).copied().unwrap();
    builder.statements.push(Statement {
        span: Some(info.name.span),
        kind: StatementKind::StorageLive(local_idx),
    });
    builder.statements.push(Statement {
        span: Some(info.name.span),
        kind: StatementKind::Assign(
            Place {
                local: local_idx,
                projection: Default::default(),
            },
            rvalue,
        ),
    });
}

fn lower_assign(builder: &mut BodyBuilder, info: &ast::AssignStmt) {
    let local = *builder.name_to_local.get(&info.name.first.name).unwrap();
    let ty = builder.body.locals[local].ty.clone();
    let rvalue = lower_expr(builder, &info.value, Some(&ty));
    let place = lower_path(builder, &info.name);

    builder.statements.push(Statement {
        span: Some(info.name.first.span),
        kind: StatementKind::Assign(place, rvalue),
    })
}

fn lower_expr(
    builder: &mut BodyBuilder,
    info: &ast::Expression,
    type_hint: Option<&TypeInfo>,
) -> ir::RValue {
    match info {
        ast::Expression::Value(info) => ir::RValue::Use(lower_value(builder, info, type_hint)),
        ast::Expression::FnCall(info) => ir::RValue::Use(lower_fn_call(builder, info)),
        ast::Expression::Unary(_, _) => todo!(),
        ast::Expression::Binary(lhs, op, rhs) => {
            lower_binary_expr(builder, lhs, op, rhs, type_hint)
        }
    }
}

fn lower_binary_expr(
    builder: &mut BodyBuilder,
    lhs: &ast::Expression,
    op: &ast::BinaryOp,
    rhs: &ast::Expression,
    type_hint: Option<&TypeInfo>,
) -> ir::RValue {
    let expr_type = type_hint.expect("type hint needed");
    let lhs = lower_expr(builder, lhs, type_hint);
    let rhs = lower_expr(builder, rhs, type_hint);

    let local_ty = expr_type;
    let lhs_local = builder.add_local(Local::temp(local_ty.clone()));
    let rhs_local = builder.add_local(Local::temp(local_ty.clone()));
    let lhs_place = Place {
        local: lhs_local,
        projection: Default::default(),
    };
    let rhs_place = Place {
        local: lhs_local,
        projection: Default::default(),
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::StorageLive(lhs_local),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(lhs_place.clone(), lhs),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::StorageLive(rhs_local),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(rhs_place.clone(), rhs),
    });

    let lhs = Operand::Move(lhs_place);
    let rhs = Operand::Move(rhs_place);

    match op {
        ast::BinaryOp::Arith(op, _) => match op {
            ast::ArithOp::Add => ir::RValue::BinOp(ir::BinOp::Add, lhs, rhs),
            ast::ArithOp::Sub => ir::RValue::BinOp(ir::BinOp::Sub, lhs, rhs),
            ast::ArithOp::Mul => ir::RValue::BinOp(ir::BinOp::Mul, lhs, rhs),
            ast::ArithOp::Div => ir::RValue::BinOp(ir::BinOp::Div, lhs, rhs),
            ast::ArithOp::Mod => ir::RValue::BinOp(ir::BinOp::Rem, lhs, rhs),
        },
        ast::BinaryOp::Logic(op, _) => match op {
            ast::LogicOp::And => ir::RValue::LogicOp(ir::LogicalOp::And, lhs, rhs),
            ast::LogicOp::Or => ir::RValue::LogicOp(ir::LogicalOp::Or, lhs, rhs),
        },
        ast::BinaryOp::Compare(op, _) => match op {
            ast::CmpOp::Eq => ir::RValue::BinOp(ir::BinOp::Eq, lhs, rhs),
            ast::CmpOp::NotEq => ir::RValue::BinOp(ir::BinOp::Ne, lhs, rhs),
            ast::CmpOp::Lt => ir::RValue::BinOp(ir::BinOp::Lt, lhs, rhs),
            ast::CmpOp::LtEq => ir::RValue::BinOp(ir::BinOp::Le, lhs, rhs),
            ast::CmpOp::Gt => ir::RValue::BinOp(ir::BinOp::Gt, lhs, rhs),
            ast::CmpOp::GtEq => ir::RValue::BinOp(ir::BinOp::Ge, lhs, rhs),
        },
        ast::BinaryOp::Bitwise(op, _) => match op {
            ast::BitwiseOp::And => ir::RValue::BinOp(ir::BinOp::BitAnd, lhs, rhs),
            ast::BitwiseOp::Or => ir::RValue::BinOp(ir::BinOp::BitOr, lhs, rhs),
            ast::BitwiseOp::Xor => ir::RValue::BinOp(ir::BinOp::BitXor, lhs, rhs),
        },
    }
}

fn lower_fn_call(builder: &mut BodyBuilder, info: &ast::FnCallExpr) -> Operand {
    let fn_id = {
        let mod_body = builder.get_module_body();

        if let Some(id) = mod_body.symbols.functions.get(&info.name.name) {
            *id
        } else {
            *mod_body
                .imports
                .get(&info.name.name)
                .expect("function call not found")
        }
    };
    let (args_ty, ret_ty) = {
        if let Some(x) = builder.ctx.body.function_signatures.get(&fn_id).cloned() {
            x
        } else {
            let (args, ret) = builder
                .ctx
                .unresolved_function_signatures
                .get(&fn_id)
                .unwrap();

            let args: Vec<_> = args.iter().map(|x| lower_type(&builder.ctx, x)).collect();
            let ret = ret
                .as_ref()
                .map(|x| lower_type(&builder.ctx, x))
                .unwrap_or(TypeInfo {
                    span: None,
                    kind: TypeKind::Unit,
                });
            builder
                .ctx
                .body
                .function_signatures
                .insert(fn_id, (args.clone(), ret.clone()));
            (args, ret)
        }
    };

    let mut args = Vec::new();

    for (arg, arg_ty) in info.params.iter().zip(args_ty) {
        let rvalue = lower_expr(builder, arg, Some(&arg_ty));
        args.push(rvalue);
    }

    let dest_local = builder.add_local(Local::temp(ret_ty));

    let dest_place = Place {
        local: dest_local,
        projection: Default::default(),
    };

    let target_block = builder.body.blocks.len() + 1;

    // todo: check if function is diverging such as exit().
    let kind = Terminator::Call {
        func: fn_id,
        args,
        destination: dest_place.clone(),
        target: Some(target_block),
    };

    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: kind,
    });

    Operand::Move(dest_place)
}

fn lower_value(
    builder: &mut BodyBuilder,
    info: &ast::ValueExpr,
    type_hint: Option<&TypeInfo>,
) -> Operand {
    match info {
        ast::ValueExpr::Bool { value, span } => ir::Operand::Constant(ir::ConstData {
            span: Some(*span),
            type_info: ir::TypeInfo {
                span: None,
                kind: ir::TypeKind::Bool,
            },
            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::Bool(*value))),
        }),
        ast::ValueExpr::Char { value, span } => ir::Operand::Constant(ir::ConstData {
            span: Some(*span),
            type_info: ir::TypeInfo {
                span: None,
                kind: ir::TypeKind::Char,
            },
            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::U32((*value) as u32))),
        }),
        ast::ValueExpr::Int { value, span } => {
            let (ty, val, type_span) = match type_hint {
                Some(type_hint) => match &type_hint.kind {
                    ir::TypeKind::Int(int_type) => match int_type {
                        ir::IntTy::I128 => (
                            ir::TypeKind::Int(ir::IntTy::I128),
                            ir::ConstValue::I128((*value) as i128),
                            type_hint.span,
                        ),
                        ir::IntTy::I64 => (
                            ir::TypeKind::Int(ir::IntTy::I64),
                            ir::ConstValue::I64((*value) as i64),
                            type_hint.span,
                        ),
                        ir::IntTy::I32 => (
                            ir::TypeKind::Int(ir::IntTy::I32),
                            ir::ConstValue::I32((*value) as i32),
                            type_hint.span,
                        ),
                        ir::IntTy::I16 => (
                            ir::TypeKind::Int(ir::IntTy::I16),
                            ir::ConstValue::I16((*value) as i16),
                            type_hint.span,
                        ),
                        ir::IntTy::I8 => (
                            ir::TypeKind::Int(ir::IntTy::I8),
                            ir::ConstValue::I8((*value) as i8),
                            type_hint.span,
                        ),
                        ir::IntTy::Isize => todo!(),
                    },
                    ir::TypeKind::Uint(int_type) => match int_type {
                        ir::UintTy::U128 => (
                            ir::TypeKind::Uint(ir::UintTy::U128),
                            ir::ConstValue::U128(*value),
                            type_hint.span,
                        ),
                        ir::UintTy::U64 => (
                            ir::TypeKind::Uint(ir::UintTy::U64),
                            ir::ConstValue::U64((*value) as u64),
                            type_hint.span,
                        ),
                        ir::UintTy::U32 => (
                            ir::TypeKind::Uint(ir::UintTy::U32),
                            ir::ConstValue::U32((*value) as u32),
                            type_hint.span,
                        ),
                        ir::UintTy::U16 => (
                            ir::TypeKind::Uint(ir::UintTy::U16),
                            ir::ConstValue::U16((*value) as u16),
                            type_hint.span,
                        ),
                        ir::UintTy::U8 => (
                            ir::TypeKind::Uint(ir::UintTy::U8),
                            ir::ConstValue::U8((*value) as u8),
                            type_hint.span,
                        ),
                        _ => todo!(),
                    },
                    _ => unreachable!(),
                },
                None => todo!(),
            };

            ir::Operand::Constant(ir::ConstData {
                span: Some(*span),
                type_info: ir::TypeInfo {
                    span: type_span,
                    kind: ty,
                },
                kind: ir::ConstKind::Value(ir::ValueTree::Leaf(val)),
            })
        }
        ast::ValueExpr::Float { value, span } => match type_hint {
            Some(type_hint) => match &type_hint.kind {
                TypeKind::Float(float_ty) => match float_ty {
                    ir::FloatTy::F32 => ir::Operand::Constant(ir::ConstData {
                        span: Some(*span),
                        type_info: ir::TypeInfo {
                            span: type_hint.span,
                            kind: ir::TypeKind::Float(ir::FloatTy::F32),
                        },
                        kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::F32(
                            value.parse().unwrap(),
                        ))),
                    }),
                    ir::FloatTy::F64 => ir::Operand::Constant(ir::ConstData {
                        span: Some(*span),
                        type_info: ir::TypeInfo {
                            span: type_hint.span,
                            kind: ir::TypeKind::Float(ir::FloatTy::F64),
                        },
                        kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::F64(
                            value.parse().unwrap(),
                        ))),
                    }),
                },
                _ => unreachable!(),
            },
            None => todo!(),
        },
        ast::ValueExpr::Str { value: _, span: _ } => todo!(),
        ast::ValueExpr::Path(info) => {
            // add deref info to path
            Operand::Move(lower_path(builder, info))
        }
    }
}

fn lower_return(builder: &mut BodyBuilder, info: &ast::ReturnStmt, return_type: &TypeInfo) {
    if let Some(value_expr) = &info.value {
        let value = lower_expr(builder, value_expr, Some(return_type));
        builder.statements.push(Statement {
            span: None,
            kind: StatementKind::Assign(
                Place {
                    local: builder.ret_local,
                    projection: Default::default(),
                },
                value,
            ),
        });
    }

    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: Terminator::Return,
    });
}

fn lower_path(builder: &mut BodyBuilder, info: &ast::PathExpr) -> ir::Place {
    let local = *builder
        .name_to_local
        .get(&info.first.name)
        .expect("local not found");

    Place {
        local,
        projection: Default::default(), // todo, field array deref
    }
}

pub fn lower_type(_ctx: &BuildCtx, t: &ast::Type) -> ir::TypeInfo {
    match t.name.name.as_str() {
        "()" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Unit,
        },
        "u8" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U8),
        },
        "u16" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U16),
        },
        "u32" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U32),
        },
        "u64" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U64),
        },
        "u128" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U128),
        },
        "i8" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I8),
        },
        "i16" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I16),
        },
        "i32" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I32),
        },
        "i64" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I64),
        },
        "i128" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I128),
        },
        _ => todo!(),
    }
}
