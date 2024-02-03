use std::collections::HashMap;

use common::{BodyBuilder, BuildCtx, IdGenerator, ModuleCtx};
use edlang_ast as ast;
use edlang_ir as ir;
use ir::{ConstData, ConstKind, Local, Operand, Place, Statement, Terminator, TypeInfo};

mod common;

pub fn lower_modules(modules: &[ast::Module]) -> Vec<ir::ModuleBody> {
    let mut ctx = BuildCtx::default();

    for m in modules {
        ctx.module_name_to_id
            .insert(m.name.name.clone(), ctx.module_id_counter);
        ctx.module_id_counter += 1;
    }

    let mut lowered_modules = Vec::with_capacity(modules.len());

    // todo: maybe should do a prepass here populating all symbols

    for module in modules {
        let ir;
        (ctx, ir) = lower_module(ctx, module);
        lowered_modules.push(ir);
    }

    lowered_modules
}

fn lower_module(mut ctx: BuildCtx, module: &ast::Module) -> (BuildCtx, ir::ModuleBody) {
    let mut body = ir::ModuleBody {
        module_id: ctx.module_id_counter,
        functions: Default::default(),
        modules: Default::default(),
        span: module.span,
    };
    ctx.module_id_counter += 1;

    let mut module_ctx = ModuleCtx {
        id: body.module_id,
        gen: IdGenerator::new(body.module_id),
        ..Default::default()
    };

    for stmt in &module.contents {
        match stmt {
            ast::ModuleStatement::Function(func) => {
                let next_id = module_ctx.gen.next_defid();
                module_ctx
                    .func_name_to_id
                    .insert(func.name.name.clone(), next_id);

                let mut args = Vec::new();
                let ret_type;

                if let Some(ret) = func.return_type.as_ref() {
                    ret_type = lower_type(&mut ctx, ret);
                } else {
                    ret_type = TypeInfo {
                        span: None,
                        kind: ir::TypeKind::Unit,
                    };
                }

                for arg in &func.params {
                    let ty = lower_type(&mut ctx, &arg.arg_type);
                    args.push(ty);
                }

                module_ctx.functions.insert(next_id, (args, ret_type));
            }
            ast::ModuleStatement::Constant(_) => todo!(),
            ast::ModuleStatement::Struct(_) => todo!(),
            ast::ModuleStatement::Module(_) => todo!(),
        }
    }

    ctx.module_name_to_id
        .insert(module.name.name.clone(), body.module_id);
    ctx.modules.insert(body.module_id, module_ctx);

    for stmt in &module.contents {
        match stmt {
            ast::ModuleStatement::Function(func) => {
                let (res, new_ctx) = lower_function(ctx, func, body.module_id);
                body.functions.insert(res.def_id, res);
                ctx = new_ctx;
            }
            ast::ModuleStatement::Constant(_) => todo!(),
            ast::ModuleStatement::Struct(_) => todo!(),
            ast::ModuleStatement::Module(_) => todo!(),
        }
    }

    (ctx, body)
}

fn lower_function(
    mut ctx: BuildCtx,
    func: &ast::Function,
    module_id: usize,
) -> (ir::Body, BuildCtx) {
    let def_id = *ctx
        .modules
        .get(&module_id)
        .unwrap()
        .func_name_to_id
        .get(&func.name.name)
        .unwrap();

    let body = ir::Body {
        def_id,
        ret_type: func.return_type.as_ref().map(|x| lower_type(&mut ctx, x)),
        locals: Default::default(),
        blocks: Default::default(),
        fn_span: func.span,
        is_pub: func.is_public,
        is_extern: func.is_extern,
    };

    let mut builder = BodyBuilder {
        body,
        statements: Vec::new(),
        locals: HashMap::new(),
        ret_local: None,
        ctx,
        local_module: module_id,
    };

    // store args ret

    if let Some(ret_type) = func.return_type.as_ref() {
        let ty = lower_type(&mut builder.ctx, ret_type);

        let local = Local {
            mutable: false,
            span: None,
            ty,
            kind: ir::LocalKind::ReturnPointer,
        };

        builder.ret_local = Some(builder.body.locals.len());
        builder.body.locals.push(local);
    }

    for arg in &func.params {
        let ty = lower_type(&mut builder.ctx, &arg.arg_type);
        let local = Local {
            mutable: false,
            span: Some(arg.span),
            ty,
            kind: ir::LocalKind::Arg,
        };
        builder
            .locals
            .insert(arg.name.name.clone(), builder.locals.len());
        builder.body.locals.push(local);
    }

    for stmt in &func.body.body {
        match stmt {
            ast::Statement::Let(info) => lower_let(&mut builder, info),
            ast::Statement::Assign(info) => lower_assign(&mut builder, info),
            ast::Statement::For(_) => todo!(),
            ast::Statement::While(_) => todo!(),
            ast::Statement::If(_) => todo!(),
            ast::Statement::Return(info) => lower_return(&mut builder, info),
            ast::Statement::FnCall(_) => todo!(),
        }
    }

    (builder.body, builder.ctx)
}

fn lower_let(builder: &mut BodyBuilder, info: &ast::LetStmt) {
    let ty = lower_type(&mut builder.ctx, &info.r#type);
    let rvalue = lower_expr(builder, &info.value, Some(&ty));

    let local = ir::Local {
        mutable: info.is_mut,
        span: Some(info.span),
        ty: lower_type(&mut builder.ctx, &info.r#type),
        kind: ir::LocalKind::Temp,
    };

    let id = builder.body.locals.len();
    builder.locals.insert(info.name.name.clone(), id);
    builder.body.locals.push(local);

    builder.statements.push(ir::Statement {
        span: Some(info.span),
        kind: ir::StatementKind::StorageLive(id),
    });
    builder.statements.push(ir::Statement {
        span: Some(info.span),
        kind: ir::StatementKind::Assign(
            Place {
                local: id,
                projection: Default::default(),
            },
            rvalue,
        ),
    });
}

fn lower_assign(builder: &mut BodyBuilder, info: &ast::AssignStmt) {
    let local = *builder.locals.get(&info.name.first.name).unwrap();
    let ty = builder.body.locals[local].ty.clone();
    let rvalue = lower_expr(builder, &info.value, Some(&ty));
    let place = lower_path(builder, &info.name);

    builder.statements.push(Statement {
        span: Some(info.span),
        kind: ir::StatementKind::Assign(place, rvalue),
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
        ast::Expression::Binary(_, _, _) => todo!(),
    }
}

fn lower_fn_call(builder: &mut BodyBuilder, info: &ast::FnCallExpr) -> ir::Operand {
    let (arg_types, ret_type) = builder.get_fn_by_name(&info.name.name).unwrap().clone();

    let mut args = Vec::new();

    let target_local = builder.add_local(Local {
        mutable: false,
        span: None,
        ty: ret_type,
        kind: ir::LocalKind::Temp,
    });

    let dest_place = Place {
        local: target_local,
        projection: Default::default(),
    };

    for (expr, ty) in info.params.iter().zip(arg_types) {
        let rvalue = lower_expr(builder, expr, Some(&ty));

        let local = builder.add_local(Local {
            mutable: false,
            span: None,
            ty,
            kind: ir::LocalKind::Temp,
        });

        let place = Place {
            local,
            projection: Default::default(),
        };

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::StorageLive(local),
        });

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::Assign(place.clone(), rvalue),
        });

        args.push(Operand::Move(place))
    }

    builder.statements.push(Statement {
        span: None,
        kind: ir::StatementKind::StorageLive(target_local),
    });

    let fn_id = *builder
        .get_current_module()
        .func_name_to_id
        .get(&info.name.name)
        .unwrap();

    let next_block = builder.body.blocks.len() + 1;

    let terminator = Terminator::Call {
        func: Operand::Constant(ConstData {
            span: Some(info.span),
            type_info: TypeInfo {
                span: None,
                kind: ir::TypeKind::FnDef(fn_id, vec![]),
            },
            kind: ConstKind::ZeroSized,
        }),
        args,
        dest: dest_place.clone(),
        target: Some(next_block),
    };

    let statements = std::mem::take(&mut builder.statements);

    builder.body.blocks.push(ir::BasicBlock {
        id: builder.body.blocks.len(),
        statements: statements.into(),
        terminator,
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
            let (ty, val) = match type_hint {
                Some(type_hint) => match &type_hint.kind {
                    ir::TypeKind::Int(type_hint) => match type_hint {
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
                    ir::TypeKind::Uint(type_hint) => match type_hint {
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

            ir::Operand::Constant(ir::ConstData {
                span: Some(*span),
                type_info: ir::TypeInfo {
                    span: None,
                    kind: ty,
                },
                kind: ir::ConstKind::Value(ir::ValueTree::Leaf(val)),
            })
        }
        ast::ValueExpr::Float { value, span } => todo!(),
        ast::ValueExpr::Str { value, span } => todo!(),
        ast::ValueExpr::Path(info) => {
            // add deref info to path
            Operand::Move(lower_path(builder, info))
        }
    }
}

fn lower_return(builder: &mut BodyBuilder, info: &ast::ReturnStmt) {
    let ret_type = builder.body.ret_type.clone();
    if let Some(value) = &info.value {
        let rvalue = lower_expr(builder, value, ret_type.as_ref());
        let ret_local = builder.ret_local.unwrap();

        builder.statements.push(Statement {
            span: Some(info.span),
            kind: ir::StatementKind::Assign(
                Place {
                    local: ret_local,
                    projection: Default::default(),
                },
                rvalue,
            ),
        })
    }

    let statements = std::mem::take(&mut builder.statements);

    builder.body.blocks.push(ir::BasicBlock {
        id: builder.body.blocks.len(),
        statements: statements.into(),
        terminator: ir::Terminator::Return,
    });
}

fn lower_path(builder: &mut BodyBuilder, info: &ast::PathExpr) -> ir::Place {
    let local = *builder
        .locals
        .get(&info.first.name)
        .expect("local not found");

    Place {
        local,
        projection: Default::default(), // todo, field array deref
    }
}

pub fn lower_type(ctx: &mut BuildCtx, t: &ast::Type) -> ir::TypeInfo {
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
