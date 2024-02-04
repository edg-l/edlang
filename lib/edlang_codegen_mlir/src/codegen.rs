use std::{collections::HashMap, error::Error, path::PathBuf};

use edlang_ir as ir;
use edlang_ir::DefId;
use edlang_session::Session;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::{DICompileUnit, DebugInfoBuilder},
    module::Module,
    targets::{InitializationConfig, Target, TargetData, TargetMachine, TargetTriple},
    types::{AnyType, BasicMetadataTypeEnum, BasicType},
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, PointerValue},
};
use ir::{TypeInfo, ValueTree};
use tracing::info;

#[derive(Debug, Clone, Copy)]
struct CompileCtx<'a> {
    context: &'a Context,
    session: &'a Session,
    modules: &'a HashMap<DefId, ir::ModuleBody>,
    symbols: &'a HashMap<DefId, String>,
}

struct ModuleCompileCtx<'ctx, 'm> {
    ctx: CompileCtx<'ctx>,
    builder: &'m Builder<'ctx>,
    module: Module<'m>,
    di_builder: DebugInfoBuilder<'ctx>,
    di_unit: DICompileUnit<'ctx>,
    target_data: TargetData,
}

pub fn compile(
    session: &Session,
    modules: &HashMap<DefId, ir::ModuleBody>,
    symbols: &HashMap<DefId, String>,
) -> Result<PathBuf, Box<dyn Error>> {
    let context = Context::create();
    let builder = context.create_builder();

    let ctx = CompileCtx {
        context: &context,
        session,
        modules,
        symbols,
    };

    let mut llvm_modules = Vec::new();

    Target::initialize_native(&InitializationConfig::default())?;
    let triple = TargetMachine::get_default_triple();
    let cpu_features = TargetMachine::get_host_cpu_features();
    let cpu_name = TargetMachine::get_host_cpu_name();
    let target = Target::from_triple(&triple)?;
    let machine = target
        .create_target_machine(
            &triple,
            cpu_name.to_str()?,
            cpu_features.to_str()?,
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    let filename = session.file_path.file_name().unwrap().to_string_lossy();
    let dir = session.file_path.parent().unwrap().to_string_lossy();
    for (id, module) in modules.iter() {
        let name = ctx.symbols.get(id).unwrap();
        let llvm_module = context.create_module(name);
        llvm_module.set_source_file_name(&filename);
        llvm_module.set_triple(&triple);
        let (di_builder, di_unit) = llvm_module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::Rust,
            &filename,
            &dir,
            "edlang",
            true,
            "", // compiler flags
            1,
            "", // split name
            inkwell::debug_info::DWARFEmissionKind::Full,
            module.module_id.module_id.try_into().unwrap(), // compile unit id?
            false,
            false,
            "",
            "edlang-sdk",
        );

        let module_ctx = ModuleCompileCtx {
            ctx,
            module: llvm_module,
            di_builder,
            di_unit,
            builder: &builder,
            target_data: machine.get_target_data(),
        };

        compile_module(&module_ctx, module);

        module_ctx.module.verify()?;

        module_ctx
            .module
            .print_to_file(session.output_file.with_extension("ll"))?;

        machine.write_to_file(
            &module_ctx.module,
            inkwell::targets::FileType::Assembly,
            &session.output_file.with_extension("asm"),
        )?;
        machine.write_to_file(
            &module_ctx.module,
            inkwell::targets::FileType::Object,
            &session.output_file.with_extension("o"),
        )?;
        // todo link modules together
        llvm_modules.push(module_ctx.module);
    }

    Ok(session.output_file.with_extension("o"))
}

fn compile_module(ctx: &ModuleCompileCtx, module: &ir::ModuleBody) {
    info!("compiling module");
    for (_fn_id, func) in module.functions.iter() {
        compile_fn_signature(ctx, func);
    }

    for (_fn_id, func) in module.functions.iter() {
        compile_fn(ctx, func).unwrap();
    }
}

fn compile_fn_signature(ctx: &ModuleCompileCtx, body: &ir::Body) {
    let name = ctx.ctx.symbols.get(&body.def_id).unwrap();
    info!("compiling fn sig: {}", name);

    let (args, ret_type) = { (body.get_args(), body.ret_type.clone()) };

    let args: Vec<BasicMetadataTypeEnum> = args
        .iter()
        .map(|x| compile_basic_type(ctx, &x.ty).into())
        .collect();
    // let ret_type = compile_basic_type(ctx, &ret_type);

    let fn_type = if let ir::TypeKind::Unit = ret_type.kind {
        ctx.ctx.context.void_type().fn_type(&args, false)
    } else {
        compile_basic_type(ctx, &ret_type).fn_type(&args, false)
    };

    ctx.module.add_function(
        name,
        fn_type,
        Some(if body.is_extern {
            inkwell::module::Linkage::AvailableExternally
        } else if body.is_pub {
            inkwell::module::Linkage::External
        } else {
            inkwell::module::Linkage::Private
        }),
    );
}

fn compile_fn(ctx: &ModuleCompileCtx, body: &ir::Body) -> Result<(), BuilderError> {
    let name = ctx.ctx.symbols.get(&body.def_id).unwrap();
    info!("compiling fn body: {}", name);
    // let (args, ret_type) = { (body.get_args(), body.ret_type.clone().unwrap()) };

    let fn_value = ctx.module.get_function(name).unwrap();

    let block = ctx.ctx.context.append_basic_block(fn_value, "entry");
    ctx.builder.position_at_end(block);

    let mut locals = HashMap::new();
    let mut ret_local = None;

    let mut arg_counter = 0;

    for (index, local) in body.locals.iter().enumerate() {
        match local.kind {
            ir::LocalKind::Temp => {
                let ptr = ctx
                    .builder
                    .build_alloca(compile_basic_type(ctx, &local.ty), &index.to_string())?;
                locals.insert(index, ptr);
            }
            ir::LocalKind::Arg => {
                let ptr = ctx
                    .builder
                    .build_alloca(compile_basic_type(ctx, &local.ty), &index.to_string())?;
                ctx.builder
                    .build_store(ptr, fn_value.get_nth_param(arg_counter).unwrap())?;
                arg_counter += 1;
                locals.insert(index, ptr);
            }
            ir::LocalKind::ReturnPointer => {
                if let ir::TypeKind::Unit = &local.ty.kind {
                } else {
                    ret_local = Some(index);
                    let ptr = ctx
                        .builder
                        .build_alloca(compile_basic_type(ctx, &local.ty), &index.to_string())?;
                    locals.insert(index, ptr);
                }
            }
        }
    }

    let mut blocks = Vec::with_capacity(body.blocks.len());

    for (index, _block) in body.blocks.iter().enumerate() {
        let llvm_block = ctx
            .ctx
            .context
            .append_basic_block(fn_value, &format!("block_{index}"));
        blocks.push(llvm_block);
    }

    ctx.builder.build_unconditional_branch(blocks[0])?;

    for (block, llvm_block) in body.blocks.iter().zip(&blocks) {
        info!("compiling block");
        ctx.builder.position_at_end(*llvm_block);
        for stmt in &block.statements {
            info!("compiling stmt");
            match &stmt.kind {
                ir::StatementKind::Assign(place, rvalue) => match rvalue {
                    ir::RValue::Use(op) => {
                        let value = compile_load_operand(ctx, body, &locals, op)?.0;
                        ctx.builder
                            .build_store(*locals.get(&place.local).unwrap(), value)?;
                    }
                    ir::RValue::Ref(_, _) => todo!(),
                    ir::RValue::BinOp(op, lhs, rhs) => {
                        let value = compile_bin_op(ctx, body, &locals, *op, lhs, rhs)?;
                        ctx.builder
                            .build_store(*locals.get(&place.local).unwrap(), value)?;
                    }
                    ir::RValue::LogicOp(_, _, _) => todo!(),
                    ir::RValue::UnOp(_, _) => todo!(),
                },
                ir::StatementKind::StorageLive(_) => {
                    // https://llvm.org/docs/LangRef.html#int-lifestart
                }
                ir::StatementKind::StorageDead(_) => {}
            }
        }

        info!("compiling terminator");
        match &block.terminator {
            ir::Terminator::Target(id) => {
                ctx.builder.build_unconditional_branch(blocks[*id])?;
            }
            ir::Terminator::Return => match ret_local {
                Some(local) => {
                    let ptr = *locals.get(&local).unwrap();
                    let pointee_ty = compile_basic_type(ctx, &body.locals[local].ty);
                    let value = ctx.builder.build_load(pointee_ty, ptr, "")?;
                    ctx.builder.build_return(Some(&value))?;
                }
                None => {
                    ctx.builder.build_return(None)?;
                }
            },
            ir::Terminator::Switch => todo!(),
            ir::Terminator::Call {
                func,
                args,
                dest,
                target,
            } => todo!(),
            ir::Terminator::Unreachable => todo!(),
        }
    }

    Ok(())
}

fn compile_bin_op<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    body: &ir::Body,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: ir::BinOp,
    lhs: &ir::Operand,
    rhs: &ir::Operand,
) -> Result<BasicValueEnum<'ctx>, BuilderError> {
    let (lhs_value, lhs_ty) = compile_load_operand(ctx, body, locals, lhs)?;
    let (rhs_value, _rhs_ty) = compile_load_operand(ctx, body, locals, rhs)?;

    let is_float = matches!(lhs_ty.kind, ir::TypeKind::Float(_));
    let is_signed = matches!(lhs_ty.kind, ir::TypeKind::Int(_));

    Ok(match op {
        ir::BinOp::Add => {
            if is_float {
                ctx.builder
                    .build_float_add(
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_add(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Sub => {
            if is_float {
                ctx.builder
                    .build_float_sub(
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_sub(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Mul => {
            if is_float {
                ctx.builder
                    .build_float_mul(
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_add(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Div => {
            if is_float {
                ctx.builder
                    .build_float_div(
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else if is_signed {
                ctx.builder
                    .build_int_signed_div(
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_unsigned_div(
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Rem => {
            if is_float {
                ctx.builder
                    .build_float_rem(
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else if is_signed {
                ctx.builder
                    .build_int_signed_rem(
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_unsigned_rem(
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::BitXor => ctx
            .builder
            .build_xor(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
            .as_basic_value_enum(),
        ir::BinOp::BitAnd => ctx
            .builder
            .build_and(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
            .as_basic_value_enum(),
        ir::BinOp::BitOr => ctx
            .builder
            .build_or(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
            .as_basic_value_enum(),
        ir::BinOp::Shl => ctx
            .builder
            .build_left_shift(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
            .as_basic_value_enum(),
        ir::BinOp::Shr => ctx
            .builder
            .build_right_shift(
                lhs_value.into_int_value(),
                rhs_value.into_int_value(),
                is_signed,
                "",
            )?
            .as_basic_value_enum(),
        ir::BinOp::Eq => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OEQ,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Lt => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OLT,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        if is_signed {
                            inkwell::IntPredicate::SLT
                        } else {
                            inkwell::IntPredicate::ULT
                        },
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Le => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OLE,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        if is_signed {
                            inkwell::IntPredicate::SLE
                        } else {
                            inkwell::IntPredicate::ULE
                        },
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Ne => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::ONE,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Ge => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OGE,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        if is_signed {
                            inkwell::IntPredicate::SGE
                        } else {
                            inkwell::IntPredicate::UGE
                        },
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Gt => {
            if is_float {
                ctx.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OGT,
                        lhs_value.into_float_value(),
                        rhs_value.into_float_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_compare(
                        if is_signed {
                            inkwell::IntPredicate::SGT
                        } else {
                            inkwell::IntPredicate::UGT
                        },
                        lhs_value.into_int_value(),
                        rhs_value.into_int_value(),
                        "",
                    )?
                    .as_basic_value_enum()
            }
        }
        ir::BinOp::Offset => todo!(),
    })
}

fn compile_load_operand<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    body: &ir::Body,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: &ir::Operand,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    // todo: implement projection
    Ok(match op {
        ir::Operand::Copy(place) => {
            let pointee_ty = compile_basic_type(ctx, &body.locals[place.local].ty);
            let ptr = *locals.get(&place.local).unwrap();
            (
                ctx.builder.build_load(pointee_ty, ptr, "")?,
                body.locals[place.local].ty.clone(),
            )
        }
        ir::Operand::Move(place) => {
            let pointee_ty = compile_basic_type(ctx, &body.locals[place.local].ty);
            let ptr = *locals.get(&place.local).unwrap();
            (
                ctx.builder.build_load(pointee_ty, ptr, "")?,
                body.locals[place.local].ty.clone(),
            )
        }
        ir::Operand::Constant(data) => match &data.kind {
            ir::ConstKind::Value(value) => (
                compile_value(ctx, value, &data.type_info)?,
                data.type_info.clone(),
            ),
            ir::ConstKind::ZeroSized => todo!(),
        },
    })
}

fn compile_value<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    val: &ValueTree,
    ty: &ir::TypeInfo,
) -> Result<BasicValueEnum<'ctx>, BuilderError> {
    let ty = compile_basic_type(ctx, ty);
    Ok(match val {
        ValueTree::Leaf(const_val) => match const_val {
            ir::ConstValue::Bool(x) => ty
                .into_int_type()
                .const_int((*x) as u64, false)
                .as_basic_value_enum(),
            ir::ConstValue::I8(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::I16(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::I32(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::I64(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::I128(x) => ty
                .into_int_type()
                .const_int_from_string(&x.to_string(), inkwell::types::StringRadix::Decimal)
                .unwrap()
                .as_basic_value_enum(),
            ir::ConstValue::U8(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::U16(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::U32(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::U64(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::U128(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
            ir::ConstValue::F32(x) => ty
                .into_float_type()
                .const_float((*x) as f64)
                .as_basic_value_enum(),
            ir::ConstValue::F64(x) => ty.into_float_type().const_float(*x).as_basic_value_enum(),
        },
        ValueTree::Branch(_) => todo!(),
    })
}

fn compile_type<'a>(
    ctx: &'a ModuleCompileCtx,
    ty: &ir::TypeInfo,
) -> inkwell::types::AnyTypeEnum<'a> {
    // ctx.di_builder.create_basic_type(name, size_in_bits, encoding, flags)
    let context = ctx.module.get_context();
    match &ty.kind {
        ir::TypeKind::Unit => context.void_type().as_any_type_enum(),
        ir::TypeKind::FnDef(def_id, _generic_args) => {
            let (args, ret_type) = {
                let fn_body = ctx
                    .ctx
                    .modules
                    .get(&def_id.get_module_defid())
                    .unwrap()
                    .functions
                    .get(def_id)
                    .unwrap();
                (fn_body.get_args(), fn_body.ret_type.clone())
            };

            let args: Vec<BasicMetadataTypeEnum> = args
                .iter()
                .map(|x| compile_basic_type(ctx, &x.ty).into())
                .collect();
            let ret_type = compile_basic_type(ctx, &ret_type);

            ret_type.fn_type(&args, false).as_any_type_enum()
        }
        _ => compile_basic_type(ctx, ty).as_any_type_enum(),
    }
}

fn compile_basic_type<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    ty: &ir::TypeInfo,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    // ctx.di_builder.create_basic_type(name, size_in_bits, encoding, flags)
    match &ty.kind {
        ir::TypeKind::Unit => panic!(),
        ir::TypeKind::Bool => ctx.ctx.context.bool_type().as_basic_type_enum(),
        ir::TypeKind::Char => ctx.ctx.context.i32_type().as_basic_type_enum(),
        ir::TypeKind::Int(ty) => match ty {
            ir::IntTy::I128 => ctx.ctx.context.i128_type().as_basic_type_enum(),
            ir::IntTy::I64 => ctx.ctx.context.i64_type().as_basic_type_enum(),
            ir::IntTy::I32 => ctx.ctx.context.i32_type().as_basic_type_enum(),
            ir::IntTy::I16 => ctx.ctx.context.i16_type().as_basic_type_enum(),
            ir::IntTy::I8 => ctx.ctx.context.i8_type().as_basic_type_enum(),
            ir::IntTy::Isize => ctx
                .ctx
                .context
                .ptr_sized_int_type(&ctx.target_data, None)
                .as_basic_type_enum(),
        },
        ir::TypeKind::Uint(ty) => match ty {
            ir::UintTy::U128 => ctx.ctx.context.i128_type().as_basic_type_enum(),
            ir::UintTy::U64 => ctx.ctx.context.i64_type().as_basic_type_enum(),
            ir::UintTy::U32 => ctx.ctx.context.i32_type().as_basic_type_enum(),
            ir::UintTy::U16 => ctx.ctx.context.i16_type().as_basic_type_enum(),
            ir::UintTy::U8 => ctx.ctx.context.i8_type().as_basic_type_enum(),
            ir::UintTy::Usize => ctx
                .ctx
                .context
                .ptr_sized_int_type(&ctx.target_data, None)
                .as_basic_type_enum(),
        },
        ir::TypeKind::Float(ty) => match ty {
            ir::FloatTy::F32 => ctx.ctx.context.f32_type().as_basic_type_enum(),
            ir::FloatTy::F64 => ctx.ctx.context.f64_type().as_basic_type_enum(),
        },
        ir::TypeKind::FnDef(_def_id, _generic_args) => {
            panic!()
        }
    }
}
