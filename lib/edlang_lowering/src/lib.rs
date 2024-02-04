use std::collections::HashMap;

use common::{BodyBuilder, BuildCtx, IdGenerator, ModuleCtx};
use edlang_ast as ast;
use edlang_ir as ir;
use ir::{ConstData, ConstKind, DefId, Local, Operand, Place, Statement, Terminator, TypeInfo};

mod common;

pub fn lower_modules(
    modules: &[ast::Module],
) -> (HashMap<DefId, String>, HashMap<DefId, ir::ModuleBody>) {
    let mut ctx = BuildCtx::default();

    for m in modules {
        let module_id = ctx.gen.module_defid();
        ctx.module_name_to_id
            .insert(m.name.name.clone(), ctx.gen.module_defid());
        ctx.symbol_names.insert(module_id, m.name.name.clone());

        ctx.modules.insert(
            module_id,
            ModuleCtx {
                id: module_id,
                gen: IdGenerator::new(module_id.module_id),
                ..Default::default()
            },
        );

        ctx.gen.next_module_defid();
    }

    let mut lowered_modules = HashMap::with_capacity(modules.len());

    // todo: maybe should do a prepass here populating all symbols

    for module in modules {
        let ir;
        (ctx, ir) = lower_module(ctx, module);
        lowered_modules.insert(ir.module_id, ir);
    }

    (ctx.symbol_names, lowered_modules)
}

fn lower_module(mut ctx: BuildCtx, module: &ast::Module) -> (BuildCtx, ir::ModuleBody) {
    let module_id = *ctx.module_name_to_id.get(&module.name.name).unwrap();
    let mut body = ir::ModuleBody {
        module_id,
        functions: Default::default(),
        modules: Default::default(),
        span: module.span,
    };

    for stmt in &module.contents {
        match stmt {
            ast::ModuleStatement::Function(func) => {
                let next_id = {
                    let module_ctx = ctx.modules.get_mut(&module_id).unwrap();
                    let next_id = module_ctx.gen.next_defid();
                    module_ctx
                        .func_name_to_id
                        .insert(func.name.name.clone(), next_id);
                    ctx.symbol_names.insert(next_id, func.name.name.clone());
                    next_id
                };

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

                let module_ctx = ctx.modules.get_mut(&module_id).unwrap();
                module_ctx.functions.insert(next_id, (args, ret_type));
            }
            ast::ModuleStatement::Constant(_) => todo!(),
            ast::ModuleStatement::Struct(_) => todo!(),
            ast::ModuleStatement::Module(_) => {}
        }
    }

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
    module_id: DefId,
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
        ret_type: func
            .return_type
            .as_ref()
            .map(|x| lower_type(&mut ctx, x))
            .unwrap_or_else(|| TypeInfo {
                span: None,
                kind: ir::TypeKind::Unit,
            }),
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
            ast::Statement::FnCall(info) => {
                lower_fn_call_no_ret(&mut builder, info);
            }
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
    let lhs = {
        let rvalue = lower_expr(builder, lhs, type_hint);
        let local = builder.add_local(Local {
            mutable: false,
            span: None,
            ty: expr_type.clone(),
            kind: ir::LocalKind::Temp,
        });

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::StorageLive(local),
        });

        let place = Place {
            local,
            projection: Default::default(),
        };

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::Assign(place.clone(), rvalue),
        });

        place
    };
    let rhs = {
        let rvalue = lower_expr(builder, rhs, type_hint);
        let local = builder.add_local(Local {
            mutable: false,
            span: None,
            ty: expr_type.clone(),
            kind: ir::LocalKind::Temp,
        });

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::StorageLive(local),
        });

        let place = Place {
            local,
            projection: Default::default(),
        };

        builder.statements.push(Statement {
            span: None,
            kind: ir::StatementKind::Assign(place.clone(), rvalue),
        });

        place
    };

    match op {
        ast::BinaryOp::Arith(op, _) => match op {
            ast::ArithOp::Add => {
                ir::RValue::BinOp(ir::BinOp::Add, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::ArithOp::Sub => {
                ir::RValue::BinOp(ir::BinOp::Sub, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::ArithOp::Mul => {
                ir::RValue::BinOp(ir::BinOp::Mul, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::ArithOp::Div => {
                ir::RValue::BinOp(ir::BinOp::Div, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::ArithOp::Mod => {
                ir::RValue::BinOp(ir::BinOp::Rem, Operand::Move(lhs), Operand::Move(rhs))
            }
        },
        ast::BinaryOp::Logic(op, _) => match op {
            ast::LogicOp::And => {
                ir::RValue::LogicOp(ir::LogicalOp::And, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::LogicOp::Or => {
                ir::RValue::LogicOp(ir::LogicalOp::Or, Operand::Move(lhs), Operand::Move(rhs))
            }
        },
        ast::BinaryOp::Compare(op, _) => match op {
            ast::CmpOp::Eq => {
                ir::RValue::BinOp(ir::BinOp::Eq, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::CmpOp::NotEq => {
                ir::RValue::BinOp(ir::BinOp::Ne, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::CmpOp::Lt => {
                ir::RValue::BinOp(ir::BinOp::Lt, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::CmpOp::LtEq => {
                ir::RValue::BinOp(ir::BinOp::Le, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::CmpOp::Gt => {
                ir::RValue::BinOp(ir::BinOp::Gt, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::CmpOp::GtEq => {
                ir::RValue::BinOp(ir::BinOp::Ge, Operand::Move(lhs), Operand::Move(rhs))
            }
        },
        ast::BinaryOp::Bitwise(op, _) => match op {
            ast::BitwiseOp::And => {
                ir::RValue::BinOp(ir::BinOp::BitAnd, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::BitwiseOp::Or => {
                ir::RValue::BinOp(ir::BinOp::BitOr, Operand::Move(lhs), Operand::Move(rhs))
            }
            ast::BitwiseOp::Xor => {
                ir::RValue::BinOp(ir::BinOp::BitXor, Operand::Move(lhs), Operand::Move(rhs))
            }
        },
    }
}

fn lower_fn_call_no_ret(builder: &mut BodyBuilder, info: &ast::FnCallExpr) {
    let (arg_types, _ret_type) = builder.get_fn_by_name(&info.name.name).unwrap().clone();

    let mut args = Vec::new();

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
        dest: None,
        target: Some(next_block),
    };

    let statements = std::mem::take(&mut builder.statements);

    builder.body.blocks.push(ir::BasicBlock {
        id: builder.body.blocks.len(),
        statements: statements.into(),
        terminator,
    });
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
        dest: Some(dest_place.clone()),
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
        let rvalue = lower_expr(builder, value, Some(&ret_type));
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

pub fn lower_type(_ctx: &mut BuildCtx, t: &ast::Type) -> ir::TypeInfo {
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
