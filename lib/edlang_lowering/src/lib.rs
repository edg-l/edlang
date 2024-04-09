use std::collections::HashMap;

use ast::{BinaryOp, ModuleStatement, Span, WhileStmt};
use common::{BodyBuilder, BuildCtx};
use edlang_ast as ast;
use edlang_ir as ir;
use errors::LoweringError;
use ir::{
    AdtBody, AdtVariant, BasicBlock, Body, DefId, Local, LocalKind, Operand, Place, PlaceElem,
    ProgramBody, RValue, Statement, StatementKind, SwitchTarget, Terminator, TypeInfo, TypeKind,
};
use tracing::trace;

mod common;
pub mod errors;
mod prepass;

pub fn lower_modules(modules: &[ast::Module]) -> Result<ProgramBody, LoweringError> {
    let mut ctx = BuildCtx::default();

    // resolve symbols
    for (file_id, module) in modules.iter().enumerate() {
        ctx = prepass::prepass_module(ctx, module, file_id)?;
    }

    // resolve imports
    for (file_id, module) in modules.iter().enumerate() {
        ctx = prepass::prepass_imports(ctx, module, file_id)?;
    }

    for mod_def in modules {
        let id = *ctx
            .body
            .top_level_module_names
            .get(&mod_def.name.name)
            .expect("module should exist");

        ctx = lower_module(ctx, mod_def, id)?;
    }

    Ok(ctx.body)
}

fn lower_module(
    mut ctx: BuildCtx,
    module: &ast::Module,
    id: DefId,
) -> Result<BuildCtx, LoweringError> {
    // lower first structs, constants, types
    for content in &module.contents {
        match content {
            ModuleStatement::Constant(_) => todo!(),
            ModuleStatement::Struct(info) => {
                ctx = lower_struct(ctx, info, id)?;
            }
            // ModuleStatement::Type(_) => todo!(),
            _ => {}
        }
    }

    // fill fn sigs
    for content in &module.contents {
        match content {
            ModuleStatement::Function(fn_def) => {
                let body = ctx.body.modules.get(&id).unwrap();
                let fn_id = *body.symbols.functions.get(&fn_def.name.name).unwrap();

                let mut args = Vec::new();
                let ret_type;

                for arg in &fn_def.params {
                    let ty = lower_type(&ctx, &arg.arg_type, id)?;
                    args.push(ty);
                }

                if let Some(ty) = &fn_def.return_type {
                    ret_type = lower_type(&ctx, ty, id)?;
                } else {
                    ret_type = TypeInfo {
                        span: None,
                        kind: ir::TypeKind::Unit,
                    };
                }

                ctx.body.function_signatures.insert(fn_id, (args, ret_type));
            }
            ModuleStatement::StructImpl(info) => {
                // todo: handle generics
                assert!(info.generics.is_empty(), "generics not yet implemented");

                let struct_id = {
                    let body = ctx.body.modules.get(&id).unwrap();
                    *body.symbols.structs.get(&info.name.name).unwrap()
                };

                for fn_def in &info.methods {
                    let body = ctx.body.modules.get(&id).unwrap();

                    let fn_id = *body
                        .symbols
                        .methods
                        .get(&struct_id)
                        .expect("struct id not found")
                        .get(&fn_def.name.name)
                        .expect("struct method not found");

                    let mut args = Vec::new();
                    let ret_type;

                    for arg in &fn_def.params {
                        let ty = lower_type(&ctx, &arg.arg_type, id)?;
                        args.push(ty);
                    }

                    if let Some(ty) = &fn_def.return_type {
                        ret_type = lower_type(&ctx, ty, id)?;
                    } else {
                        ret_type = TypeInfo {
                            span: None,
                            kind: ir::TypeKind::Unit,
                        };
                    }

                    ctx.body.function_signatures.insert(fn_id, (args, ret_type));
                }
            }
            _ => {}
        }
    }

    for content in &module.contents {
        match content {
            ModuleStatement::Function(fn_def) => {
                let fn_id = {
                    let body = ctx.body.modules.get(&id).unwrap();
                    *body.symbols.functions.get(&fn_def.name.name).unwrap()
                };
                ctx = lower_function(ctx, fn_def, id, fn_id)?;
            }
            ModuleStatement::StructImpl(info) => {
                let struct_id = {
                    let body = ctx.body.modules.get(&id).unwrap();
                    *body.symbols.structs.get(&info.name.name).unwrap()
                };

                for fn_def in &info.methods {
                    // todo: handle generics
                    assert!(info.generics.is_empty(), "generics not yet implemented");
                    let fn_id = {
                        let body = ctx.body.modules.get(&id).unwrap();
                        *body
                            .symbols
                            .methods
                            .get(&struct_id)
                            .unwrap()
                            .get(&fn_def.name.name)
                            .unwrap()
                    };
                    ctx = lower_function(ctx, fn_def, id, fn_id)?;
                }
            }
            // ModuleStatement::Type(_) => todo!(),
            ModuleStatement::Module(mod_def) => {
                let body = ctx.body.modules.get(&id).unwrap();
                let id = *body.symbols.modules.get(&mod_def.name.name).unwrap();
                ctx = lower_module(ctx, mod_def, id)?;
            }
            _ => {}
        }
    }

    Ok(ctx)
}

fn lower_struct(
    mut ctx: BuildCtx,
    info: &ast::Struct,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mut body = AdtBody {
        def_id: {
            let body = ctx.body.modules.get(&module_id).unwrap();
            *body.symbols.structs.get(&info.name.name).unwrap()
        },
        is_pub: true, // todo struct pub
        name: info.name.name.clone(),
        variants: Vec::new(),
        name_to_idx: Default::default(),
        span: info.span,
    };

    for field in &info.fields {
        let variant = AdtVariant {
            def_id: ctx.gen.next_defid(),
            name: field.name.name.clone(),
            ty: lower_type(&ctx, &field.r#type, module_id)?,
        };
        body.variants.push(variant);
        body.name_to_idx
            .insert(field.name.name.clone(), body.variants.len() - 1);
    }

    ctx.body.structs.insert(body.def_id, body);
    Ok(ctx)
}

fn lower_function(
    ctx: BuildCtx,
    func: &ast::Function,
    module_id: DefId,
    fn_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mut builder = BodyBuilder {
        body: Body {
            blocks: Default::default(),
            locals: Default::default(),
            name: func.name.name.clone(),
            def_id: fn_id,
            is_pub: func.is_public,
            is_extern: func.is_extern,
            is_exported: func.is_exported || func.name.name == "main",
            fn_span: func.span,
        },
        local_module: module_id,
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        file_id: {
            let body = ctx.body.modules.get(&module_id).unwrap();
            body.file_id
        },
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

    if func.body.is_some() && !func.is_extern {
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
        for stmt in &func.body.as_ref().unwrap().body {
            if let ast::Statement::Let(info) = stmt {
                let ty = lower_type(&builder.ctx, &info.r#type, builder.local_module)?;
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

        for stmt in &func.body.as_ref().unwrap().body {
            lower_statement(&mut builder, stmt, &ret_ty)?;
        }

        if !builder.statements.is_empty() {
            let statements = std::mem::take(&mut builder.statements);
            builder.body.blocks.push(BasicBlock {
                statements: statements.into(),
                terminator: Terminator::Return,
                terminator_span: None,
            });
        }
    }
    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.def_id);
    ctx.body.functions.insert(body.def_id, body);
    Ok(ctx)
}

fn lower_statement(
    builder: &mut BodyBuilder,
    info: &ast::Statement,
    ret_type: &TypeInfo,
) -> Result<(), LoweringError> {
    match info {
        ast::Statement::Let(info) => lower_let(builder, info),
        ast::Statement::Assign(info) => lower_assign(builder, info),
        ast::Statement::For(_) => todo!(),
        ast::Statement::While(info) => lower_while(builder, info, ret_type),
        ast::Statement::If(info) => lower_if_stmt(builder, info, ret_type),
        ast::Statement::Return(info) => lower_return(builder, info, ret_type),
        ast::Statement::FnCall(info) => {
            lower_fn_call(builder, info)?;
            Ok(())
        }
    }
}

fn lower_while(
    builder: &mut BodyBuilder,
    info: &WhileStmt,
    ret_type: &TypeInfo,
) -> Result<(), LoweringError> {
    let statements = std::mem::take(&mut builder.statements);
    builder.body.blocks.push(BasicBlock {
        statements: statements.into(),
        terminator: Terminator::Target(builder.body.blocks.len() + 1),
        terminator_span: Some(info.block.span),
    });

    let (discriminator, discriminator_type, disc_span) =
        lower_expr(builder, &info.condition, None)?;

    let local = builder.add_temp_local(TypeKind::Bool);
    let place = Place {
        local,
        projection: Default::default(),
    };

    builder.statements.push(Statement {
        span: Some(disc_span),
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
        lower_statement(builder, stmt, ret_type)?;
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

    Ok(())
}

fn lower_if_stmt(
    builder: &mut BodyBuilder,
    info: &ast::IfStmt,
    ret_type: &TypeInfo,
) -> Result<(), LoweringError> {
    let cond_ty = find_expr_type(builder, &info.condition).expect("couldnt find cond type");
    let (condition, condition_ty, cond_span) = lower_expr(
        builder,
        &info.condition,
        Some(&TypeInfo {
            span: None,
            kind: cond_ty,
        }),
    )?;

    let local = builder.add_temp_local(TypeKind::Bool);
    let place = Place {
        local,
        projection: vec![].into(),
    };

    builder.statements.push(Statement {
        span: Some(cond_span),
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
        lower_statement(builder, stmt, ret_type)?;
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
            lower_statement(builder, stmt, ret_type)?;
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

    Ok(())
}

fn lower_let(builder: &mut BodyBuilder, info: &ast::LetStmt) -> Result<(), LoweringError> {
    let ty = lower_type(&builder.ctx, &info.r#type, builder.local_module)?;
    let (rvalue, found_ty, _span) = lower_expr(builder, &info.value, Some(&ty))?;

    if ty.kind != found_ty {
        return Err(LoweringError::UnexpectedType {
            span: info.span,
            found: found_ty,
            expected: ty.clone(),
            file_id: builder.file_id,
        });
    }

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

    Ok(())
}

fn lower_assign(builder: &mut BodyBuilder, info: &ast::AssignStmt) -> Result<(), LoweringError> {
    let (mut place, ty, _span) = lower_path(builder, &info.name)?;
    let mut path_ty = TypeInfo {
        span: None,
        kind: ty,
    };

    for _ in 0..info.deref_times {
        match &path_ty.kind {
            TypeKind::Ptr(is_mut, inner) => {
                if !is_mut {
                    panic!("trying to mutate non mut ptr");
                }
                path_ty = *inner.clone();
            }
            TypeKind::Ref(is_mut, inner) => {
                if !is_mut {
                    panic!("trying to mutate non mut ref");
                }
                path_ty = *inner.clone();
            }
            _ => unreachable!(),
        }
        place.projection.push(PlaceElem::Deref);
    }

    let (rvalue, _ty, _span) = lower_expr(builder, &info.value, Some(&path_ty))?;

    builder.statements.push(Statement {
        span: Some(info.name.first.span),
        kind: StatementKind::Assign(place, rvalue),
    });

    Ok(())
}

fn find_expr_type(builder: &mut BodyBuilder, info: &ast::Expression) -> Option<TypeKind> {
    Some(match info {
        ast::Expression::Value(val) => match val {
            ast::ValueExpr::Bool { .. } => TypeKind::Bool,
            ast::ValueExpr::Char { .. } => TypeKind::Char,
            ast::ValueExpr::Int { .. } => return None,
            ast::ValueExpr::Float { .. } => return None,
            ast::ValueExpr::Str { .. } => todo!(),
            ast::ValueExpr::Path(path) => {
                let local = builder.get_local(&path.first.name)?;
                let mut ty = local.ty.kind.clone();

                for seg in &path.extra {
                    match seg {
                        ast::PathSegment::Field(field_name) => {
                            ty = match ty {
                                TypeKind::Struct(id, _struct_name) => {
                                    let body = builder.ctx.body.structs.get(&id)?;
                                    body.variants[*body.name_to_idx.get(&field_name.name)?]
                                        .ty
                                        .kind
                                        .clone()
                                }
                                _ => unreachable!(),
                            }
                        }
                        ast::PathSegment::Index { .. } => todo!(),
                    }
                }

                ty
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
        ast::Expression::Deref(_, _) => todo!(),
        ast::Expression::AsRef(_, _, _) => todo!(),
        ast::Expression::StructInit(info) => {
            let id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name.name)
                .expect("struct not found");
            ir::TypeKind::Struct(id, info.name.name.name.clone())
        }
        ast::Expression::Cast(_, _new_ty, _) => {
            // checks?

            todo!()
        }
    })
}

fn lower_expr(
    builder: &mut BodyBuilder,
    info: &ast::Expression,
    type_hint: Option<&TypeInfo>,
) -> Result<(ir::RValue, TypeKind, Span), LoweringError> {
    Ok(match info {
        ast::Expression::Value(info) => {
            let (value, ty, span) = lower_value(builder, info, type_hint)?;

            (ir::RValue::Use(value, span), ty, span)
        }
        ast::Expression::FnCall(info) => {
            let (value, ty, span) = lower_fn_call(builder, info)?;

            (ir::RValue::Use(value, span), ty, span)
        }
        ast::Expression::Unary(_, _) => todo!(),
        ast::Expression::Binary(lhs, op, rhs) => {
            lower_binary_expr(builder, lhs, op, rhs, type_hint)?
        }
        ast::Expression::Deref(inner, deref_span) => {
            let (value, ty, _span) = lower_expr(builder, inner, type_hint)?;

            // check if its a use directly, to avoid a temporary.
            let mut value = match value {
                RValue::Use(op, _) => match op {
                    Operand::Copy(place) => place,
                    Operand::Move(place) => place,
                    Operand::Constant(_) => todo!("constant data deref"),
                },
                _ => unreachable!(),
            };

            value.projection.push(PlaceElem::Deref);

            let ty = match ty {
                TypeKind::Ptr(_, inner) => *inner,
                TypeKind::Ref(_, inner) => *inner,
                _ => todo!("proepr error here"),
            };

            (
                RValue::Use(Operand::Move(value), *deref_span),
                ty.kind,
                *deref_span,
            )
        }
        ast::Expression::AsRef(inner, mutable, as_ref_span) => {
            let type_hint = match type_hint {
                Some(inner) => match &inner.kind {
                    TypeKind::Ref(_, inner) => Some(inner.as_ref()),
                    _ => unreachable!(),
                },
                None => None,
            };
            let (mut value, ty, _span) = lower_expr(builder, inner, type_hint)?;

            // check if its a use directly, to avoid a temporary.
            value = match value {
                RValue::Use(op, _span) => RValue::Ref(*mutable, op, *as_ref_span),
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
                    RValue::Ref(*mutable, Operand::Move(inner_place), *as_ref_span)
                }
            };

            let ty = TypeKind::Ref(
                *mutable,
                Box::new(TypeInfo {
                    span: Some(*as_ref_span),
                    kind: ty,
                }),
            );

            (value, ty, *as_ref_span)
        }
        ast::Expression::StructInit(info) => {
            let id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name.name)
                .expect("struct not found");
            let struct_body = builder.ctx.body.structs.get(&id).unwrap().clone();
            let ty = TypeKind::Struct(id, struct_body.name.clone());
            let struct_local = builder.add_local(Local::temp(ty.clone()));

            let place = Place {
                local: struct_local,
                projection: Default::default(),
            };

            builder.statements.push(Statement {
                span: None,
                kind: StatementKind::StorageLive(struct_local),
            });

            for (field, value) in info.fields.iter() {
                let idx = *struct_body
                    .name_to_idx
                    .get(&field.name)
                    .expect("failed to find field");
                let mut field_place = place.clone();
                field_place
                    .projection
                    .push(PlaceElem::Field { field_idx: idx });

                let variant = &struct_body.variants[idx].ty;

                let (value, _value_ty, span) = lower_expr(builder, &value.value, Some(variant))?;

                builder.statements.push(Statement {
                    span: Some(span),
                    kind: StatementKind::Assign(field_place, value),
                });
            }

            (RValue::Use(Operand::Move(place), info.span), ty, info.span)
        }
        ast::Expression::Cast(exp, cast_ty, span) => {
            let (value, ty, _exp_span) = lower_expr(builder, exp, None)?;
            let new_ty = lower_type(&builder.ctx, cast_ty, builder.local_module)?;
            let kind = new_ty.kind.clone();

            // todo: some checks?

            // check if its a use directly, to avoid a temporary.
            let rvalue = match value {
                RValue::Use(op, _) => RValue::Cast(op, new_ty.clone(), *span),
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
                    RValue::Cast(Operand::Move(inner_place), new_ty.clone(), *span)
                }
            };

            (rvalue, kind, *span)
        }
    })
}

fn lower_binary_expr(
    builder: &mut BodyBuilder,
    lhs: &ast::Expression,
    op: &ast::BinaryOp,
    rhs: &ast::Expression,
    type_hint: Option<&TypeInfo>,
) -> Result<(ir::RValue, TypeKind, Span), LoweringError> {
    trace!("lowering binary op: {:?}", op);

    let (lhs, lhs_ty, lhs_span) = if type_hint.is_none() {
        let ty = find_expr_type(builder, lhs)
            .unwrap_or_else(|| find_expr_type(builder, rhs).expect("cant find type"));
        lower_expr(
            builder,
            lhs,
            Some(&TypeInfo {
                span: None,
                kind: ty,
            }),
        )?
    } else {
        lower_expr(builder, lhs, type_hint)?
    };
    let (rhs, rhs_ty, rhs_span) = if type_hint.is_none() {
        let ty = find_expr_type(builder, rhs).unwrap_or(lhs_ty.clone());
        lower_expr(
            builder,
            rhs,
            Some(&TypeInfo {
                span: None,
                kind: ty,
            }),
        )?
    } else {
        lower_expr(builder, rhs, type_hint)?
    };

    if lhs_ty != rhs_ty {
        return Err(LoweringError::UnexpectedType {
            span: rhs_span,
            found: rhs_ty,
            expected: TypeInfo {
                span: Some(lhs_span),
                kind: lhs_ty,
            },
            file_id: builder.file_id,
        });
    }

    let lhs = match lhs {
        RValue::Use(op, _span) => op,
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
        RValue::Use(op, _span) => op,
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

    Ok(match op {
        ast::BinaryOp::Arith(op, span) => (
            match op {
                ast::ArithOp::Add => ir::RValue::BinOp(ir::BinOp::Add, lhs, rhs, *span),
                ast::ArithOp::Sub => ir::RValue::BinOp(ir::BinOp::Sub, lhs, rhs, *span),
                ast::ArithOp::Mul => ir::RValue::BinOp(ir::BinOp::Mul, lhs, rhs, *span),
                ast::ArithOp::Div => ir::RValue::BinOp(ir::BinOp::Div, lhs, rhs, *span),
                ast::ArithOp::Mod => ir::RValue::BinOp(ir::BinOp::Rem, lhs, rhs, *span),
            },
            lhs_ty,
            *span,
        ),
        ast::BinaryOp::Logic(op, span) => (
            match op {
                ast::LogicOp::And => ir::RValue::LogicOp(ir::LogicalOp::And, lhs, rhs, *span),
                ast::LogicOp::Or => ir::RValue::LogicOp(ir::LogicalOp::Or, lhs, rhs, *span),
            },
            TypeKind::Bool,
            *span,
        ),
        ast::BinaryOp::Compare(op, span) => (
            match op {
                ast::CmpOp::Eq => ir::RValue::BinOp(ir::BinOp::Eq, lhs, rhs, *span),
                ast::CmpOp::NotEq => ir::RValue::BinOp(ir::BinOp::Ne, lhs, rhs, *span),
                ast::CmpOp::Lt => ir::RValue::BinOp(ir::BinOp::Lt, lhs, rhs, *span),
                ast::CmpOp::LtEq => ir::RValue::BinOp(ir::BinOp::Le, lhs, rhs, *span),
                ast::CmpOp::Gt => ir::RValue::BinOp(ir::BinOp::Gt, lhs, rhs, *span),
                ast::CmpOp::GtEq => ir::RValue::BinOp(ir::BinOp::Ge, lhs, rhs, *span),
            },
            TypeKind::Bool,
            *span,
        ),
        ast::BinaryOp::Bitwise(op, span) => (
            match op {
                ast::BitwiseOp::And => ir::RValue::BinOp(ir::BinOp::BitAnd, lhs, rhs, *span),
                ast::BitwiseOp::Or => ir::RValue::BinOp(ir::BinOp::BitOr, lhs, rhs, *span),
                ast::BitwiseOp::Xor => ir::RValue::BinOp(ir::BinOp::BitXor, lhs, rhs, *span),
            },
            lhs_ty,
            *span,
        ),
    })
}

fn lower_fn_call(
    builder: &mut BodyBuilder,
    info: &ast::FnCallExpr,
) -> Result<(Operand, TypeKind, Span), LoweringError> {
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

            let args: Vec<_> = args
                .iter()
                .map(|x| lower_type(&builder.ctx, x, builder.local_module))
                .collect::<Result<Vec<_>, LoweringError>>()?;
            let ret = ret
                .as_ref()
                .map(|x| lower_type(&builder.ctx, x, builder.local_module))
                .unwrap_or(Ok(TypeInfo {
                    span: None,
                    kind: TypeKind::Unit,
                }))?;
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
        let (rvalue, _rvalue_ty, _span) = lower_expr(builder, arg, Some(&arg_ty))?;
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

    Ok((Operand::Move(dest_place), ret_ty.kind.clone(), info.span))
}

fn lower_value(
    builder: &mut BodyBuilder,
    info: &ast::ValueExpr,
    type_hint: Option<&TypeInfo>,
) -> Result<(Operand, TypeKind, Span), LoweringError> {
    Ok(match info {
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
            *span,
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
            *span,
        ),
        ast::ValueExpr::Int { value, span } => {
            let (ty, val) = match type_hint {
                Some(type_hint) => match &type_hint.kind {
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
                    ir::TypeKind::Ptr(_, _) => (
                        ir::TypeKind::Int(ir::IntTy::Isize),
                        ir::ConstValue::Isize((*value) as isize),
                    ),
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
                *span,
            )
        }
        ast::ValueExpr::Float { value, span } => match type_hint {
            Some(type_hint) => match &type_hint.kind {
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
                        type_hint.kind.clone(),
                        *span,
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
                        type_hint.kind.clone(),
                        *span,
                    ),
                },
                _ => unreachable!(),
            },
            None => todo!(),
        },
        ast::ValueExpr::Str { value: _, span: _ } => todo!(),
        ast::ValueExpr::Path(info) => {
            let (place, ty, span) = lower_path(builder, info)?;
            (Operand::Move(place), ty, span)
        }
    })
}

fn lower_return(
    builder: &mut BodyBuilder,
    info: &ast::ReturnStmt,
    return_type: &TypeInfo,
) -> Result<(), LoweringError> {
    if let Some(value_expr) = &info.value {
        let (value, ty, span) = lower_expr(builder, value_expr, Some(return_type))?;

        if return_type.kind != ty {
            return Err(LoweringError::UnexpectedType {
                span,
                found: ty,
                expected: return_type.clone(),
                file_id: builder.file_id,
            });
        }

        builder.statements.push(Statement {
            span: Some(span),
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

    Ok(())
}

fn lower_path(
    builder: &mut BodyBuilder,
    info: &ast::PathExpr,
) -> Result<(ir::Place, TypeKind, Span), LoweringError> {
    let local = *builder.name_to_local.get(&info.first.name).ok_or(
        LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            file_id: builder.file_id,
        },
    )?;

    let mut ty = builder.body.locals[local].ty.kind.clone();
    let mut projection = Vec::new();

    for extra in &info.extra {
        match extra {
            ast::PathSegment::Field(name) => {
                // is while fine? auto deref
                while let TypeKind::Ref(_, inner) = ty {
                    projection.push(PlaceElem::Deref);
                    ty = inner.kind;
                }

                if let TypeKind::Struct(id, _name) = ty {
                    let struct_body = builder.ctx.body.structs.get(&id).unwrap();
                    let idx = *struct_body.name_to_idx.get(&name.name).unwrap();
                    projection.push(PlaceElem::Field { field_idx: idx });
                    ty = struct_body.variants[idx].ty.kind.clone();
                }
            }
            ast::PathSegment::Index { .. } => todo!(),
        }
    }

    Ok((
        Place {
            local,
            projection: projection.into(), // todo, array
        },
        ty,
        info.span,
    ))
}

#[allow(clippy::only_used_in_recursion)]
pub fn lower_type(
    ctx: &BuildCtx,
    t: &ast::Type,
    module_id: DefId,
) -> Result<ir::TypeInfo, LoweringError> {
    let mut ty = match t.name.name.as_str() {
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
        "str" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Str,
        },
        other => {
            let module = ctx.body.modules.get(&module_id).expect("module not found");
            if let Some(struct_id) = module.symbols.structs.get(other) {
                let struct_body = ctx.body.structs.get(struct_id).unwrap();
                ir::TypeInfo {
                    span: Some(struct_body.span),
                    kind: TypeKind::Struct(*struct_id, struct_body.name.clone()),
                }
            } else {
                Err(LoweringError::UnrecognizedType {
                    span: t.name.span,
                    name: t.name.name.clone(),
                    file_id: module.file_id,
                })?
            }
        }
    };

    for qualifier in t.qualifiers.iter().rev() {
        let kind = match qualifier {
            ast::TypeQualifier::Ref => TypeKind::Ref(false, Box::new(ty)),
            ast::TypeQualifier::RefMut => TypeKind::Ref(true, Box::new(ty)),
            ast::TypeQualifier::Ptr => TypeKind::Ptr(false, Box::new(ty)),
            ast::TypeQualifier::PtrMut => TypeKind::Ptr(true, Box::new(ty)),
        };

        ty = TypeInfo {
            span: Some(t.span),
            kind,
        };
    }

    Ok(ty)
}
