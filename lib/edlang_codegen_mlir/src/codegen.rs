use std::{collections::HashMap, error::Error};

use bumpalo::Bump;
use edlang_ast::{
    ArithOp, AssignStmt, BinaryOp, CmpOp, Constant, Expression, FnCallExpr, Function, IfStmt,
    LetStmt, LogicOp, Module, ModuleStatement, ReturnStmt, Statement, Struct, ValueExpr,
};
use edlang_session::Session;
use melior::{
    dialect::{
        arith::{self, CmpiPredicate},
        cf, func, memref,
    },
    ir::{
        attribute::{FlatSymbolRefAttribute, IntegerAttribute, StringAttribute, TypeAttribute},
        r#type::{FunctionType, IntegerType, MemRefType},
        Attribute, Block, BlockRef, Location, Module as MeliorModule, Region, Type, TypeLike,
        Value, ValueLike,
    },
    Context as MeliorContext,
};

use crate::util::call_site;
#[derive(Debug, Clone)]
pub struct LocalVar<'ctx, 'parent: 'ctx> {
    pub ast_type: edlang_ast::Type,
    // If it's none its on a register, otherwise allocated on the stack.
    pub is_alloca: bool,
    pub value: Value<'ctx, 'parent>,
}

impl<'ctx, 'parent> LocalVar<'ctx, 'parent> {
    pub fn param(value: Value<'ctx, 'parent>, ast_type: edlang_ast::Type) -> Self {
        Self {
            value,
            ast_type,
            is_alloca: false,
        }
    }

    pub fn alloca(value: Value<'ctx, 'parent>, ast_type: edlang_ast::Type) -> Self {
        Self {
            value,
            ast_type,
            is_alloca: true,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct ScopeContext<'ctx, 'parent: 'ctx> {
    pub locals: HashMap<String, LocalVar<'ctx, 'parent>>,
    pub functions: HashMap<String, &'parent Function>,
    pub structs: HashMap<String, &'parent Struct>,
    pub constants: HashMap<String, &'parent Constant>,
    pub function: Option<&'parent edlang_ast::Function>,
}

impl<'ctx, 'parent: 'ctx> ScopeContext<'ctx, 'parent> {
    fn is_type_signed(&self, type_info: &edlang_ast::Type) -> bool {
        let signed = ["i8", "i16", "i32", "i64", "i128"];
        signed.contains(&type_info.name.name.as_str())
    }

    fn is_float(&self, type_info: &edlang_ast::Type) -> bool {
        let signed = ["f32", "f64"];
        signed.contains(&type_info.name.name.as_str())
    }
}

struct BlockHelper<'ctx, 'this: 'ctx> {
    region: &'this Region<'ctx>,
    blocks_arena: &'this Bump,
}

impl<'ctx, 'this: 'ctx> BlockHelper<'ctx, 'this> {
    pub fn append_block(&self, block: Block<'ctx>) -> &'this BlockRef<'ctx, 'this> {
        let block = self.region.append_block(block);

        let block_ref: &'this mut BlockRef<'ctx, 'this> = self.blocks_arena.alloc(block);
        block_ref
    }
}

impl<'ctx, 'parent: 'ctx> ScopeContext<'ctx, 'parent> {
    fn resolve_type_name(
        &self,
        context: &'ctx MeliorContext,
        name: &str,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        Ok(match name {
            "u128" | "i128" => IntegerType::new(context, 128).into(),
            "u64" | "i64" => IntegerType::new(context, 64).into(),
            "u32" | "i32" => IntegerType::new(context, 32).into(),
            "u16" | "i16" => IntegerType::new(context, 16).into(),
            "u8" | "i8" => IntegerType::new(context, 8).into(),
            "f32" => Type::float32(context),
            "f64" => Type::float64(context),
            "bool" => IntegerType::new(context, 1).into(),
            _ => todo!("custom type lookup"),
        })
    }

    fn resolve_type(
        &self,
        context: &'ctx MeliorContext,
        r#type: &edlang_ast::Type,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        self.resolve_type_name(context, &r#type.name.name)
    }
}

pub fn compile_module(
    session: &Session,
    context: &MeliorContext,
    mlir_module: &MeliorModule,
    module: &Module,
) -> Result<(), Box<dyn Error>> {
    let mut scope_ctx: ScopeContext = Default::default();
    let block = mlir_module.body();

    // Save types
    for statement in &module.contents {
        match statement {
            ModuleStatement::Function(info) => {
                scope_ctx.functions.insert(info.name.name.clone(), info);
            }
            ModuleStatement::Constant(info) => {
                scope_ctx.constants.insert(info.name.name.clone(), info);
            }
            ModuleStatement::Struct(info) => {
                scope_ctx.structs.insert(info.name.name.clone(), info);
            }
            ModuleStatement::Module(_) => todo!(),
        }
    }

    for statement in &module.contents {
        match statement {
            ModuleStatement::Function(info) => {
                compile_function_def(session, context, &scope_ctx, &block, info)?;
            }
            ModuleStatement::Constant(_) => todo!(),
            ModuleStatement::Struct(_) => todo!(),
            ModuleStatement::Module(_) => todo!(),
        }
    }

    tracing::debug!("compiled module");

    Ok(())
}

fn get_location<'c>(context: &'c MeliorContext, session: &Session, offset: usize) -> Location<'c> {
    let (_, line, col) = session.source.get_offset_line(offset).unwrap();
    Location::new(
        context,
        &session.file_path.display().to_string(),
        line + 1,
        col + 1,
    )
}

fn compile_function_def<'ctx, 'parent>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &ScopeContext<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    info: &Function,
) -> Result<(), Box<dyn Error>> {
    tracing::debug!("compiling function: {}", info.name.name);
    let region = Region::new();

    let location = get_location(context, session, info.name.span.lo);
    let location = Location::name(context, &info.name.name, location);

    let mut args = Vec::with_capacity(info.params.len());
    let mut fn_args_types = Vec::with_capacity(info.params.len());

    for param in &info.params {
        let param_type = scope_ctx.resolve_type(context, &param.arg_type)?;
        let loc = get_location(context, session, param.name.span.lo);
        let loc = Location::name(context, &param.name.name, loc);
        args.push((param_type, loc));
        fn_args_types.push(param_type);
    }

    let return_type = if let Some(return_type) = &info.return_type {
        vec![scope_ctx.resolve_type(context, return_type)?]
    } else {
        vec![]
    };

    let func_type =
        TypeAttribute::new(FunctionType::new(context, &fn_args_types, &return_type).into());

    let blocks_arena = Bump::new();
    {
        let helper = BlockHelper {
            region: &region,
            blocks_arena: &blocks_arena,
        };
        let fn_block = helper.append_block(Block::new(&args));
        let mut scope_ctx = scope_ctx.clone();
        scope_ctx.function = Some(info);

        // Push arguments into locals
        for (i, param) in info.params.iter().enumerate() {
            scope_ctx.locals.insert(
                param.name.name.clone(),
                LocalVar::param(fn_block.argument(i)?.into(), param.arg_type.clone()),
            );
        }

        let final_block = compile_block(
            session,
            context,
            &mut scope_ctx,
            &helper,
            fn_block,
            &info.body,
        )?;

        if final_block.terminator().is_none() {
            final_block.append_operation(func::r#return(
                &[],
                Location::name(
                    context,
                    "return",
                    get_location(context, session, info.span.hi),
                ),
            ));
        }
    }

    let op = func::func(
        context,
        StringAttribute::new(context, &info.name.name),
        func_type,
        region,
        &[],
        location,
    );
    assert!(op.verify());

    block.append_operation(op);

    Ok(())
}

fn compile_block<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    mut block: &'parent BlockRef<'ctx, 'parent>,
    info: &'parent edlang_ast::Block,
) -> Result<&'parent BlockRef<'ctx, 'parent>, Box<dyn std::error::Error>> {
    tracing::debug!("compiling block");
    for stmt in &info.body {
        match stmt {
            Statement::Let(info) => {
                compile_let(session, context, scope_ctx, helper, block, info)?;
            }
            Statement::Assign(info) => {
                compile_assign(session, context, scope_ctx, helper, block, info)?;
            }
            Statement::For(_) => todo!(),
            Statement::While(_) => todo!(),
            Statement::If(info) => {
                block = compile_if_stmt(session, context, scope_ctx, helper, block, info)?;
            }
            Statement::Return(info) => {
                compile_return(session, context, scope_ctx, helper, block, info)?;
            }
            Statement::FnCall(info) => {
                compile_fn_call(session, context, scope_ctx, helper, block, info)?;
            }
        }
    }

    Ok(block)
}

fn compile_let<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent BlockRef<'ctx, 'parent>,
    info: &'parent LetStmt,
) -> Result<(), Box<dyn std::error::Error>> {
    tracing::debug!("compiling let");
    let value = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.value,
        Some(&info.r#type),
    )?;
    let location = get_location(context, session, info.name.span.lo);

    let memref_type = MemRefType::new(value.r#type(), &[1], None, None);

    let alloca: Value = block
        .append_operation(memref::alloca(
            context,
            memref_type,
            &[],
            &[],
            None,
            location,
        ))
        .result(0)?
        .into();
    let k0 = block
        .append_operation(arith::constant(
            context,
            IntegerAttribute::new(0, Type::index(context)).into(),
            location,
        ))
        .result(0)?
        .into();
    block.append_operation(memref::store(value, alloca, &[k0], location));

    scope_ctx.locals.insert(
        info.name.name.clone(),
        LocalVar::alloca(alloca, info.r#type.clone()),
    );

    Ok(())
}

fn compile_assign<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent BlockRef<'ctx, 'parent>,
    info: &AssignStmt,
) -> Result<(), Box<dyn std::error::Error>> {
    tracing::debug!("compiling assign");
    let local = scope_ctx
        .locals
        .get(&info.name.first.name)
        .expect("local should exist")
        .clone();

    assert!(local.is_alloca, "can only mutate local stack variables");

    let location = get_location(context, session, info.name.first.span.lo);

    let value = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.value,
        Some(&local.ast_type),
    )?;

    let k0 = block
        .append_operation(arith::constant(
            context,
            IntegerAttribute::new(0, Type::index(context)).into(),
            location,
        ))
        .result(0)?
        .into();
    block.append_operation(memref::store(value, local.value, &[k0], location));
    Ok(())
}

fn compile_return<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent BlockRef<'ctx, 'parent>,
    info: &ReturnStmt,
) -> Result<(), Box<dyn std::error::Error>> {
    tracing::debug!("compiling return");
    let location = get_location(context, session, info.span.lo);
    let location = Location::name(context, "return", location);

    let ret_type = scope_ctx.function.and_then(|x| x.return_type.clone());

    if let Some(value) = &info.value {
        let value = compile_expression(
            session,
            context,
            scope_ctx,
            helper,
            block,
            value,
            ret_type.as_ref(),
        )?;
        block.append_operation(func::r#return(&[value], location));
    } else {
        block.append_operation(func::r#return(&[], location));
    }

    Ok(())
}

fn compile_expression<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent BlockRef<'ctx, 'parent>,
    info: &Expression,
    type_hint: Option<&'parent edlang_ast::Type>,
) -> Result<Value<'ctx, 'parent>, Box<dyn std::error::Error>> {
    tracing::debug!("compiling expression");
    Ok(match info {
        Expression::Value(info) => match info {
            ValueExpr::Bool { value, span } => block
                .append_operation(arith::constant(
                    context,
                    IntegerAttribute::new((*value) as i64, IntegerType::new(context, 1).into())
                        .into(),
                    get_location(context, session, span.lo),
                ))
                .result(0)?
                .into(),
            ValueExpr::Char { value, span } => block
                .append_operation(arith::constant(
                    context,
                    IntegerAttribute::new((*value) as i64, IntegerType::new(context, 32).into())
                        .into(),
                    get_location(context, session, span.lo),
                ))
                .result(0)?
                .into(),
            ValueExpr::Int { value, span } => {
                let type_it = match type_hint.map(|x| scope_ctx.resolve_type(context, x)) {
                    Some(info) => info,
                    None => Ok(IntegerType::new(context, 32).into()),
                }?;
                block
                    .append_operation(arith::constant(
                        context,
                        IntegerAttribute::new((*value) as i64, type_it).into(),
                        get_location(context, session, span.lo),
                    ))
                    .result(0)?
                    .into()
            }
            ValueExpr::Float { value, span } => {
                let type_it = match type_hint.map(|x| scope_ctx.resolve_type(context, x)) {
                    Some(info) => info,
                    None => Ok(Type::float32(context)),
                }?;
                block
                    .append_operation(arith::constant(
                        context,
                        Attribute::parse(context, &format!("{value} : {type_it}")).unwrap(),
                        get_location(context, session, span.lo),
                    ))
                    .result(0)?
                    .into()
            }
            ValueExpr::Str { value: _, span: _ } => todo!(),
            ValueExpr::Path(path) => {
                let local = scope_ctx
                    .locals
                    .get(&path.first.name)
                    .expect("local not found");

                let location = get_location(context, session, path.first.span.lo);
                let location = Location::name(context, &path.first.name, location);

                if local.is_alloca {
                    let k0 = block
                        .append_operation(arith::constant(
                            context,
                            IntegerAttribute::new(0, Type::index(context)).into(),
                            location,
                        ))
                        .result(0)?
                        .into();

                    block
                        .append_operation(memref::load(local.value, &[k0], location))
                        .result(0)?
                        .into()
                } else {
                    local.value
                }
            }
        },
        Expression::FnCall(info) => {
            compile_fn_call(session, context, scope_ctx, helper, block, info)?
        }
        Expression::Unary(_, _) => todo!(),
        Expression::Binary(lhs, op, rhs) => {
            let lhs =
                compile_expression(session, context, scope_ctx, helper, block, lhs, type_hint)?;
            let rhs =
                compile_expression(session, context, scope_ctx, helper, block, rhs, type_hint)?;

            match op {
                BinaryOp::Arith(arith_op, span) => {
                    let location = get_location(context, session, span.lo);
                    let ast_type_hint = type_hint.expect("type info missing");

                    block.append_operation(if scope_ctx.is_float(ast_type_hint) {
                        match arith_op {
                            ArithOp::Add => arith::addf(lhs, rhs, location),
                            ArithOp::Sub => arith::subf(lhs, rhs, location),
                            ArithOp::Mul => arith::mulf(lhs, rhs, location),
                            ArithOp::Div => arith::divf(lhs, rhs, location),
                            ArithOp::Mod => arith::remf(lhs, rhs, location),
                        }
                    } else {
                        match arith_op {
                            ArithOp::Add => arith::addi(lhs, rhs, location),
                            ArithOp::Sub => arith::subi(lhs, rhs, location),
                            ArithOp::Mul => arith::muli(lhs, rhs, location),
                            ArithOp::Div => {
                                if scope_ctx.is_type_signed(ast_type_hint) {
                                    arith::divsi(lhs, rhs, location)
                                } else {
                                    arith::divui(lhs, rhs, location)
                                }
                            }
                            ArithOp::Mod => {
                                if scope_ctx.is_type_signed(ast_type_hint) {
                                    arith::remsi(lhs, rhs, location)
                                } else {
                                    arith::remui(lhs, rhs, location)
                                }
                            }
                        }
                    })
                }
                BinaryOp::Logic(logic_op, span) => {
                    let location = get_location(context, session, span.lo);

                    block.append_operation(match logic_op {
                        LogicOp::And => {
                            dbg!(lhs.r#type());
                            dbg!(rhs.r#type());
                            let const_true = block
                                .append_operation(arith::constant(
                                    context,
                                    IntegerAttribute::new(1, IntegerType::new(context, 1).into())
                                        .into(),
                                    location,
                                ))
                                .result(0)?
                                .into();
                            let lhs_bool = block
                                .append_operation(arith::cmpi(
                                    context,
                                    CmpiPredicate::Eq,
                                    lhs,
                                    const_true,
                                    location,
                                ))
                                .result(0)?
                                .into();
                            let rhs_bool = block
                                .append_operation(arith::cmpi(
                                    context,
                                    CmpiPredicate::Eq,
                                    rhs,
                                    const_true,
                                    location,
                                ))
                                .result(0)?
                                .into();
                            arith::andi(lhs_bool, rhs_bool, location)
                        }
                        LogicOp::Or => {
                            let const_true = block
                                .append_operation(arith::constant(
                                    context,
                                    IntegerAttribute::new(1, IntegerType::new(context, 1).into())
                                        .into(),
                                    location,
                                ))
                                .result(0)?
                                .into();
                            let lhs_bool = block
                                .append_operation(arith::cmpi(
                                    context,
                                    CmpiPredicate::Eq,
                                    lhs,
                                    const_true,
                                    location,
                                ))
                                .result(0)?
                                .into();
                            let rhs_bool = block
                                .append_operation(arith::cmpi(
                                    context,
                                    CmpiPredicate::Eq,
                                    rhs,
                                    const_true,
                                    location,
                                ))
                                .result(0)?
                                .into();
                            arith::ori(lhs_bool, rhs_bool, location)
                        }
                    })
                }
                BinaryOp::Compare(cmp_op, span) => {
                    let location = get_location(context, session, span.lo);
                    block.append_operation(match cmp_op {
                        CmpOp::Eq => arith::cmpi(context, CmpiPredicate::Eq, lhs, rhs, location),
                        CmpOp::NotEq => arith::cmpi(context, CmpiPredicate::Ne, lhs, rhs, location),
                        CmpOp::Lt => arith::cmpi(context, CmpiPredicate::Slt, lhs, rhs, location),
                        CmpOp::LtEq => arith::cmpi(context, CmpiPredicate::Sle, lhs, rhs, location),
                        CmpOp::Gt => arith::cmpi(context, CmpiPredicate::Sgt, lhs, rhs, location),
                        CmpOp::GtEq => arith::cmpi(context, CmpiPredicate::Sge, lhs, rhs, location),
                    })
                }
                BinaryOp::Bitwise(_, _) => todo!(),
            }
            .result(0)?
            .into()
        }
    })
}

fn compile_fn_call<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &ScopeContext<'ctx, 'parent>,
    _helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent BlockRef<'ctx, 'parent>,
    info: &FnCallExpr,
) -> Result<Value<'ctx, 'parent>, Box<dyn Error>> {
    let mut args = Vec::with_capacity(info.params.len());
    let location = get_location(context, session, info.name.span.lo);
    let location_callee = Location::name(context, &info.name.name, location);
    let location_caller = Location::name(
        context,
        &info.name.name,
        get_location(context, session, scope_ctx.function.unwrap().span.lo),
    );
    let location = call_site(location_callee, location_caller);

    let target_fn = scope_ctx
        .functions
        .get(&info.name.name)
        .expect("function not found");

    assert_eq!(
        info.params.len(),
        target_fn.params.len(),
        "parameter length doesnt match"
    );

    for (arg, arg_info) in info.params.iter().zip(&target_fn.params) {
        let value = compile_expression(
            session,
            context,
            scope_ctx,
            _helper,
            block,
            arg,
            Some(&arg_info.arg_type),
        )?;
        args.push(value);
    }

    let return_type = if let Some(ret_type) = &target_fn.return_type {
        vec![scope_ctx.resolve_type(context, ret_type)?]
    } else {
        vec![]
    };

    Ok(block
        .append_operation(func::call(
            context,
            FlatSymbolRefAttribute::new(context, &info.name.name),
            &args,
            &return_type,
            location,
        ))
        .result(0)?
        .into())
}

fn compile_if_stmt<'c, 'this: 'c>(
    session: &Session,
    context: &'c MeliorContext,
    scope_ctx: &mut ScopeContext<'c, 'this>,
    helper: &BlockHelper<'c, 'this>,
    block: &'this BlockRef<'c, 'this>,
    info: &'this IfStmt,
) -> Result<&'this BlockRef<'c, 'this>, Box<dyn Error>> {
    let condition = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.condition,
        None,
    )?;

    let then_successor = helper.append_block(Block::new(&[]));
    let else_successor = helper.append_block(Block::new(&[]));

    let location = get_location(context, session, info.span.lo);

    block.append_operation(cf::cond_br(
        context,
        condition,
        then_successor,
        else_successor,
        &[],
        &[],
        Location::name(context, "if", location),
    ));

    let mut then_successor = then_successor;
    let mut else_successor = else_successor;

    {
        let mut then_scope_ctx = scope_ctx.clone();
        then_successor = compile_block(
            session,
            context,
            &mut then_scope_ctx,
            helper,
            then_successor,
            &info.then_block,
        )?;
    }

    if let Some(else_block) = info.else_block.as_ref() {
        let mut else_scope_ctx = scope_ctx.clone();
        else_successor = compile_block(
            session,
            context,
            &mut else_scope_ctx,
            helper,
            else_successor,
            else_block,
        )?;
    }

    // both return
    if then_successor.terminator().is_some() && else_successor.terminator().is_some() {
        return Ok(then_successor);
    }

    let final_block = helper.append_block(Block::new(&[]));

    if then_successor.terminator().is_none() {
        then_successor.append_operation(cf::br(
            final_block,
            &[],
            get_location(context, session, info.span.hi),
        ));
    }

    if else_successor.terminator().is_none() {
        else_successor.append_operation(cf::br(
            final_block,
            &[],
            get_location(context, session, info.span.hi),
        ));
    }

    Ok(final_block)
}
