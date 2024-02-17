use std::collections::HashMap;

use ast::{BinaryOp, ModuleStatement, Span, WhileStmt};
use common::{BodyBuilder, BuildCtx};
use edlang_ast as ast;
use edlang_ir as ir;
use ir::{
    BasicBlock, Body, DefId, Local, LocalKind, Operand, Place, PlaceElem, ProgramBody, RValue,
    Statement, StatementKind, SwitchTarget, Terminator, TypeInfo, TypeKind,
};
use tracing::trace;

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
            is_pub: func.is_public || func.name.name == "main",
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
        lower_statement(&mut builder, stmt, &ret_ty.kind);
    }

    if !builder.statements.is_empty() {
        let statements = std::mem::take(&mut builder.statements);
        builder.body.blocks.push(BasicBlock {
            statements: statements.into(),
            terminator: Terminator::Return,
            terminator_span: None,
        });
    }

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.def_id);
    ctx.body.functions.insert(body.def_id, body);
    ctx
}

fn lower_statement(builder: &mut BodyBuilder, info: &ast::Statement, ret_type: &TypeKind) {
    match info {
        ast::Statement::Let(info) => lower_let(builder, info),
        ast::Statement::Assign(info) => lower_assign(builder, info),
        ast::Statement::For(_) => todo!(),
        ast::Statement::While(info) => lower_while(builder, info, ret_type),
        ast::Statement::If(info) => lower_if_stmt(builder, info, ret_type),
        ast::Statement::Return(info) => lower_return(builder, info, ret_type),
        ast::Statement::FnCall(info) => {
            lower_fn_call(builder, info);
        }
    }
}

fn lower_while(builder: &mut BodyBuilder, info: &WhileStmt, ret_type: &TypeKind) {
    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: Terminator::Target(builder.body.blocks.len() + 1),
        terminator_span: Some(info.block.span),
    });

    let (discriminator, discriminator_type) = lower_expr(builder, &info.condition, None);

    let local = builder.add_temp_local(TypeKind::Bool);
    let place = Place {
        local,
        projection: Default::default(),
    };

    builder.statements.push(Statement {
        span: Some(info.span),
        kind: StatementKind::Assign(place.clone(), discriminator),
    });

    // keep idx to change terminator
    let check_block_idx = builder.body.blocks.len();

    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: Terminator::Unreachable,
        terminator_span: Some(info.block.span),
    });

    // keep idx for switch targets
    let first_then_block_idx = builder.body.blocks.len();

    for stmt in &info.block.body {
        lower_statement(builder, stmt, ret_type);
    }

    // keet idx to change terminator if there is no return
    let last_then_block_idx = if !matches!(
        builder.body.blocks.last().unwrap().terminator,
        Terminator::Return
    ) {
        builder.body.blocks.len();
        let statements = std::mem::take(&mut builder.statements);
        let idx = builder.body.blocks.len();
        builder.body.blocks.push(BasicBlock {
            statements: statements.into(),
            terminator: Terminator::Unreachable,
            terminator_span: Some(Span::new(info.block.span.hi, info.block.span.hi)),
        });
        Some(idx)
    } else {
        None
    };

    let otherwise_block_idx = builder.body.blocks.len();

    let targets = SwitchTarget {
        values: vec![discriminator_type.get_falsy_value()],
        targets: vec![otherwise_block_idx, first_then_block_idx],
    };

    let kind = Terminator::SwitchInt {
        discriminator: Operand::Move(place),
        targets,
    };
    builder.body.blocks[check_block_idx].terminator = kind;

    if let Some(last_then_block_idx) = last_then_block_idx {
        builder.body.blocks[last_then_block_idx].terminator = Terminator::Target(check_block_idx);
    }
}

fn lower_if_stmt(builder: &mut BodyBuilder, info: &ast::IfStmt, ret_type: &TypeKind) {
    let cond_ty = find_expr_type(builder, &info.condition).expect("coouldnt find cond type");
    let (condition, condition_ty) = lower_expr(builder, &info.condition, Some(&cond_ty));

    let local = builder.add_temp_local(TypeKind::Bool);
    let place = Place {
        local,
        projection: vec![].into(),
    };

    builder.statements.push(Statement {
        span: Some(info.span),
        kind: StatementKind::Assign(place.clone(), condition),
    });

    // keep idx to change terminator
    let current_block_idx = builder.body.blocks.len();

    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: Terminator::Unreachable,
        terminator_span: Some(info.span),
    });

    // keep idx for switch targets
    let first_then_block_idx = builder.body.blocks.len();

    for stmt in &info.then_block.body {
        lower_statement(builder, stmt, ret_type);
    }

    // keet idx to change terminator
    let last_then_block_idx = if !matches!(
        builder.body.blocks.last().unwrap().terminator,
        Terminator::Return
    ) {
        let idx = builder.body.blocks.len();
        let statements = std::mem::take(&mut builder.statements);
        builder.body.blocks.push(BasicBlock {
            statements: statements.into(),
            terminator: Terminator::Unreachable,
            terminator_span: Some(Span::new(info.then_block.span.hi, info.then_block.span.hi)),
        });
        Some(idx)
    } else {
        None
    };

    let first_else_block_idx = builder.body.blocks.len();

    if let Some(contents) = &info.else_block {
        for stmt in &contents.body {
            lower_statement(builder, stmt, ret_type);
        }
    }

    let last_else_block_idx = if !matches!(
        builder.body.blocks.last().unwrap().terminator,
        Terminator::Return
    ) {
        let idx = builder.body.blocks.len();
        let statements = std::mem::take(&mut builder.statements);
        builder.body.blocks.push(BasicBlock {
            statements: statements.into(),
            terminator: Terminator::Unreachable,
            terminator_span: info
                .else_block
                .as_ref()
                .map(|x| Span::new(x.span.hi, x.span.hi)),
        });
        Some(idx)
    } else {
        None
    };

    let targets = SwitchTarget {
        values: vec![condition_ty.get_falsy_value()],
        targets: vec![first_else_block_idx, first_then_block_idx],
    };

    let kind = Terminator::SwitchInt {
        discriminator: Operand::Move(place),
        targets,
    };
    builder.body.blocks[current_block_idx].terminator = kind;

    let next_block_idx = builder.body.blocks.len();
    if let Some(idx) = last_then_block_idx {
        builder.body.blocks[idx].terminator = Terminator::Target(next_block_idx);
    }

    if let Some(idx) = last_else_block_idx {
        builder.body.blocks[idx].terminator = Terminator::Target(next_block_idx);
    }
}

fn lower_let(builder: &mut BodyBuilder, info: &ast::LetStmt) {
    let ty = lower_type(&builder.ctx, &info.r#type);
    let (rvalue, _ty) = lower_expr(builder, &info.value, Some(&ty.kind));
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
    let (mut place, mut ty) = lower_path(builder, &info.name);

    if let Some(PlaceElem::Deref) = place.projection.last() {
        match &ty {
            TypeKind::Ptr(inner) => {
                ty = inner.kind.clone();
            }
            TypeKind::Ref(is_mut, inner) => {
                if !is_mut {
                    panic!("trying to mutate non mut ref");
                }
                ty = inner.kind.clone();
            }
            _ => unreachable!(),
        }
    }

    for _ in 0..info.deref_times {
        match &ty {
            TypeKind::Ptr(inner) => {
                ty = inner.kind.clone();
            }
            TypeKind::Ref(is_mut, inner) => {
                if !is_mut {
                    panic!("trying to mutate non mut ref");
                }
                ty = inner.kind.clone();
            }
            _ => unreachable!(),
        }
        place.projection.push(PlaceElem::Deref);
    }

    let (rvalue, _ty) = lower_expr(builder, &info.value, Some(&ty));

    builder.statements.push(Statement {
        span: Some(info.name.first.span),
        kind: StatementKind::Assign(place, rvalue),
    })
}

fn find_expr_type(builder: &mut BodyBuilder, info: &ast::Expression) -> Option<TypeKind> {
    Some(match info {
        ast::Expression::Value(x) => match x {
            ast::ValueExpr::Bool { .. } => TypeKind::Bool,
            ast::ValueExpr::Char { .. } => TypeKind::Char,
            ast::ValueExpr::Int { .. } => return None,
            ast::ValueExpr::Float { .. } => return None,
            ast::ValueExpr::Str { .. } => todo!(),
            ast::ValueExpr::Path(path) => {
                // todo: handle full path
                builder.get_local(&path.first.name)?.ty.kind.clone()
            }
        },
        ast::Expression::FnCall(info) => {
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

            builder
                .ctx
                .body
                .function_signatures
                .get(&fn_id)?
                .1
                .kind
                .clone()
        }
        ast::Expression::Unary(_, info) => find_expr_type(builder, info)?,
        ast::Expression::Binary(lhs, op, rhs) => {
            if matches!(op, BinaryOp::Logic(_, _)) {
                TypeKind::Bool
            } else {
                find_expr_type(builder, lhs).or(find_expr_type(builder, rhs))?
            }
        }
        ast::Expression::Deref(_) => todo!(),
        ast::Expression::AsRef(_, _) => todo!(),
    })
}

fn lower_expr(
    builder: &mut BodyBuilder,
    info: &ast::Expression,
    type_hint: Option<&TypeKind>,
) -> (ir::RValue, TypeKind) {
    match info {
        ast::Expression::Value(info) => {
            let (value, ty) = lower_value(builder, info, type_hint);
            (ir::RValue::Use(value), ty)
        }
        ast::Expression::FnCall(info) => {
            let (value, ty) = lower_fn_call(builder, info);
            (ir::RValue::Use(value), ty)
        }
        ast::Expression::Unary(_, _) => todo!(),
        ast::Expression::Binary(lhs, op, rhs) => {
            lower_binary_expr(builder, lhs, op, rhs, type_hint)
        }
        ast::Expression::Deref(_) => todo!(),
        ast::Expression::AsRef(inner, mutable) => {
            let type_hint = match type_hint {
                Some(inner) => match inner {
                    TypeKind::Ref(_, inner) => Some(&inner.kind),
                    _ => unreachable!(),
                },
                None => None,
            };
            let (mut value, ty) = lower_expr(builder, inner, type_hint);

            // check if its a use directly, to avoid a temporary.
            value = match value {
                RValue::Use(op) => RValue::Ref(*mutable, op),
                value => {
                    let inner_local = builder.add_local(Local::temp(ty.clone()));
                    let inner_place = Place {
                        local: inner_local,
                        projection: Default::default(),
                    };

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::StorageLive(inner_local),
                    });

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::Assign(inner_place.clone(), value),
                    });
                    RValue::Ref(*mutable, Operand::Move(inner_place))
                }
            };

            let ty = TypeKind::Ref(
                *mutable,
                Box::new(TypeInfo {
                    span: None,
                    kind: ty,
                }),
            );

            (value, ty)
        }
    }
}

fn lower_binary_expr(
    builder: &mut BodyBuilder,
    lhs: &ast::Expression,
    op: &ast::BinaryOp,
    rhs: &ast::Expression,
    type_hint: Option<&TypeKind>,
) -> (ir::RValue, TypeKind) {
    trace!("lowering binary op: {:?}", op);

    let (lhs, lhs_ty) = if type_hint.is_none() {
        let ty = find_expr_type(builder, lhs)
            .unwrap_or_else(|| find_expr_type(builder, rhs).expect("cant find type"));
        lower_expr(builder, lhs, Some(&ty))
    } else {
        lower_expr(builder, lhs, type_hint)
    };
    let (rhs, rhs_ty) = if type_hint.is_none() {
        let ty = find_expr_type(builder, rhs).unwrap_or(lhs_ty.clone());
        lower_expr(builder, rhs, Some(&ty))
    } else {
        lower_expr(builder, rhs, type_hint)
    };

    let lhs = match lhs {
        RValue::Use(op) => op,
        lhs => {
            let lhs_local = builder.add_local(Local::temp(lhs_ty.clone()));
            let lhs_place = Place {
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
            Operand::Move(lhs_place)
        }
    };

    let rhs = match rhs {
        RValue::Use(op) => op,
        rhs => {
            let rhs_local = builder.add_local(Local::temp(rhs_ty.clone()));
            let rhs_place = Place {
                local: rhs_local,
                projection: Default::default(),
            };

            builder.statements.push(Statement {
                span: None,
                kind: StatementKind::StorageLive(rhs_local),
            });

            builder.statements.push(Statement {
                span: None,
                kind: StatementKind::Assign(rhs_place.clone(), rhs),
            });
            Operand::Move(rhs_place)
        }
    };

    match op {
        ast::BinaryOp::Arith(op, _) => (
            match op {
                ast::ArithOp::Add => ir::RValue::BinOp(ir::BinOp::Add, lhs, rhs),
                ast::ArithOp::Sub => ir::RValue::BinOp(ir::BinOp::Sub, lhs, rhs),
                ast::ArithOp::Mul => ir::RValue::BinOp(ir::BinOp::Mul, lhs, rhs),
                ast::ArithOp::Div => ir::RValue::BinOp(ir::BinOp::Div, lhs, rhs),
                ast::ArithOp::Mod => ir::RValue::BinOp(ir::BinOp::Rem, lhs, rhs),
            },
            lhs_ty,
        ),
        ast::BinaryOp::Logic(op, _) => (
            match op {
                ast::LogicOp::And => ir::RValue::LogicOp(ir::LogicalOp::And, lhs, rhs),
                ast::LogicOp::Or => ir::RValue::LogicOp(ir::LogicalOp::Or, lhs, rhs),
            },
            TypeKind::Bool,
        ),
        ast::BinaryOp::Compare(op, _) => (
            match op {
                ast::CmpOp::Eq => ir::RValue::BinOp(ir::BinOp::Eq, lhs, rhs),
                ast::CmpOp::NotEq => ir::RValue::BinOp(ir::BinOp::Ne, lhs, rhs),
                ast::CmpOp::Lt => ir::RValue::BinOp(ir::BinOp::Lt, lhs, rhs),
                ast::CmpOp::LtEq => ir::RValue::BinOp(ir::BinOp::Le, lhs, rhs),
                ast::CmpOp::Gt => ir::RValue::BinOp(ir::BinOp::Gt, lhs, rhs),
                ast::CmpOp::GtEq => ir::RValue::BinOp(ir::BinOp::Ge, lhs, rhs),
            },
            TypeKind::Bool,
        ),
        ast::BinaryOp::Bitwise(op, _) => (
            match op {
                ast::BitwiseOp::And => ir::RValue::BinOp(ir::BinOp::BitAnd, lhs, rhs),
                ast::BitwiseOp::Or => ir::RValue::BinOp(ir::BinOp::BitOr, lhs, rhs),
                ast::BitwiseOp::Xor => ir::RValue::BinOp(ir::BinOp::BitXor, lhs, rhs),
            },
            lhs_ty,
        ),
    }
}

fn lower_fn_call(builder: &mut BodyBuilder, info: &ast::FnCallExpr) -> (Operand, TypeKind) {
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
        let (rvalue, _rvalue_ty) = lower_expr(builder, arg, Some(&arg_ty.kind));
        args.push(rvalue);
    }

    let dest_local = builder.add_local(Local::temp(ret_ty.kind.clone()));

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
        terminator_span: Some(info.span),
    });

    (Operand::Move(dest_place), ret_ty.kind.clone())
}

fn lower_value(
    builder: &mut BodyBuilder,
    info: &ast::ValueExpr,
    type_hint: Option<&TypeKind>,
) -> (Operand, TypeKind) {
    match info {
        ast::ValueExpr::Bool { value, span } => (
            ir::Operand::Constant(ir::ConstData {
                span: Some(*span),
                type_info: ir::TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
                kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::Bool(*value))),
            }),
            TypeKind::Bool,
        ),
        ast::ValueExpr::Char { value, span } => (
            ir::Operand::Constant(ir::ConstData {
                span: Some(*span),
                type_info: ir::TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Char,
                },
                kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::U32(
                    (*value) as u32,
                ))),
            }),
            TypeKind::Char,
        ),
        ast::ValueExpr::Int { value, span } => {
            let (ty, val) = match type_hint {
                Some(type_hint) => match &type_hint {
                    ir::TypeKind::Int(int_type) => match int_type {
                        ir::IntTy::I128 => (
                            ir::TypeKind::Int(ir::IntTy::I128),
                            ir::ConstValue::I128((*value) as i128),
                        ),
                        ir::IntTy::I64 => (
                            ir::TypeKind::Int(ir::IntTy::I64),
                            ir::ConstValue::I64((*value) as i64),
                        ),
                        ir::IntTy::I32 => (
                            ir::TypeKind::Int(ir::IntTy::I32),
                            ir::ConstValue::I32((*value) as i32),
                        ),
                        ir::IntTy::I16 => (
                            ir::TypeKind::Int(ir::IntTy::I16),
                            ir::ConstValue::I16((*value) as i16),
                        ),
                        ir::IntTy::I8 => (
                            ir::TypeKind::Int(ir::IntTy::I8),
                            ir::ConstValue::I8((*value) as i8),
                        ),
                        ir::IntTy::Isize => todo!(),
                    },
                    ir::TypeKind::Uint(int_type) => match int_type {
                        ir::UintTy::U128 => (
                            ir::TypeKind::Uint(ir::UintTy::U128),
                            ir::ConstValue::U128(*value),
                        ),
                        ir::UintTy::U64 => (
                            ir::TypeKind::Uint(ir::UintTy::U64),
                            ir::ConstValue::U64((*value) as u64),
                        ),
                        ir::UintTy::U32 => (
                            ir::TypeKind::Uint(ir::UintTy::U32),
                            ir::ConstValue::U32((*value) as u32),
                        ),
                        ir::UintTy::U16 => (
                            ir::TypeKind::Uint(ir::UintTy::U16),
                            ir::ConstValue::U16((*value) as u16),
                        ),
                        ir::UintTy::U8 => (
                            ir::TypeKind::Uint(ir::UintTy::U8),
                            ir::ConstValue::U8((*value) as u8),
                        ),
                        _ => todo!(),
                    },
                    _ => unreachable!(),
                },
                None => todo!(),
            };

            (
                ir::Operand::Constant(ir::ConstData {
                    span: Some(*span),
                    type_info: ir::TypeInfo {
                        span: None,
                        kind: ty.clone(),
                    },
                    kind: ir::ConstKind::Value(ir::ValueTree::Leaf(val)),
                }),
                ty,
            )
        }
        ast::ValueExpr::Float { value, span } => match type_hint {
            Some(type_hint) => match &type_hint {
                TypeKind::Float(float_ty) => match float_ty {
                    ir::FloatTy::F32 => (
                        ir::Operand::Constant(ir::ConstData {
                            span: Some(*span),
                            type_info: ir::TypeInfo {
                                span: None,
                                kind: ir::TypeKind::Float(ir::FloatTy::F32),
                            },
                            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::F32(
                                value.parse().unwrap(),
                            ))),
                        }),
                        type_hint.clone(),
                    ),
                    ir::FloatTy::F64 => (
                        ir::Operand::Constant(ir::ConstData {
                            span: Some(*span),
                            type_info: ir::TypeInfo {
                                span: None,
                                kind: ir::TypeKind::Float(ir::FloatTy::F64),
                            },
                            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::F64(
                                value.parse().unwrap(),
                            ))),
                        }),
                        type_hint.clone(),
                    ),
                },
                _ => unreachable!(),
            },
            None => todo!(),
        },
        ast::ValueExpr::Str { value: _, span: _ } => todo!(),
        ast::ValueExpr::Path(info) => {
            // add deref info to path
            let (place, ty) = lower_path(builder, info);
            (Operand::Move(place), ty)
        }
    }
}

fn lower_return(builder: &mut BodyBuilder, info: &ast::ReturnStmt, return_type: &TypeKind) {
    if let Some(value_expr) = &info.value {
        let (value, _ty) = lower_expr(builder, value_expr, Some(return_type));
        builder.statements.push(Statement {
            span: Some(info.span),
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
        terminator_span: Some(info.span),
    });
}

fn lower_path(builder: &mut BodyBuilder, info: &ast::PathExpr) -> (ir::Place, TypeKind) {
    let local = *builder
        .name_to_local
        .get(&info.first.name)
        .expect("local not found");
    let ty = builder.body.locals[local].ty.kind.clone();

    let projection = Vec::new();

    for extra in &info.extra {
        match extra {
            ast::PathSegment::Field(_) => todo!(),
            ast::PathSegment::Index { .. } => todo!(),
        }
    }

    (
        Place {
            local,
            projection: projection.into(), // todo, field array deref
        },
        ty,
    )
}

#[allow(clippy::only_used_in_recursion)]
pub fn lower_type(ctx: &BuildCtx, t: &ast::Type) -> ir::TypeInfo {
    let inner_ty = match t.name.name.as_str() {
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
        "char" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Char,
        },
        "bool" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Bool,
        },
        "ptr" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Ptr(Box::new(lower_type(ctx, t.generics.first().unwrap()))),
        },
        x => todo!("{:?}", x),
    };

    match t.is_ref {
        Some(x) => ir::TypeInfo {
            span: Some(t.span),
            kind: TypeKind::Ref(
                match x {
                    ast::RefType::Not => false,
                    ast::RefType::Mut => true,
                },
                Box::new(inner_ty),
            ),
        },
        None => inner_ty,
    }
}
