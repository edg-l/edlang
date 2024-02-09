use std::{collections::HashMap, error::Error, path::PathBuf};

use edlang_ir as ir;
use edlang_ir::DefId;
use edlang_session::Session;
use edlang_span::Span;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::{
        AsDIScope, DICompileUnit, DIFile, DIFlagsConstants, DILexicalBlock, DILocation, DIScope,
        DISubprogram, DIType, DebugInfoBuilder,
    },
    module::Module,
    targets::{InitializationConfig, Target, TargetData, TargetMachine},
    types::{AnyType, BasicMetadataTypeEnum, BasicType},
    values::{BasicValue, BasicValueEnum, PointerValue},
};
use ir::{LocalKind, ModuleBody, ProgramBody, TypeInfo, ValueTree};
use llvm_sys::debuginfo::LLVMDIFlagPublic;
use tracing::info;

#[derive(Debug, Clone, Copy)]
struct CompileCtx<'a> {
    context: &'a Context,
    session: &'a Session,
    program: &'a ProgramBody,
}

struct ModuleCompileCtx<'ctx, 'm> {
    ctx: CompileCtx<'ctx>,
    builder: &'m Builder<'ctx>,
    module: Module<'m>,
    di_builder: DebugInfoBuilder<'ctx>,
    di_unit: DICompileUnit<'ctx>,
    target_data: TargetData,
    module_id: DefId,
    di_namespace: DIScope<'ctx>,
}

impl<'ctx, 'm> ModuleCompileCtx<'ctx, 'm> {
    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx.program.modules.get(&self.module_id).unwrap()
    }

    pub fn set_debug_loc(&self, scope: DIScope<'ctx>, span: Span) -> DILocation<'ctx> {
        let (_, line, column) = self.ctx.session.source.get_offset_line(span.lo).unwrap();
        let debug_loc = self.di_builder.create_debug_location(
            self.ctx.context,
            line as u32 + 1,
            column as u32 + 1,
            scope,
            None,
        );
        self.builder.set_current_debug_location(debug_loc);
        debug_loc
    }
}

pub fn compile(session: &Session, program: &ProgramBody) -> Result<PathBuf, Box<dyn Error>> {
    let context = Context::create();
    let builder = context.create_builder();

    let ctx = CompileCtx {
        context: &context,
        session,
        program,
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
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();
    machine.set_asm_verbosity(true);
    info!("compiling for: {:?}", target.get_description());

    let filename = session.file_path.file_name().unwrap().to_string_lossy();
    let dir = session.file_path.parent().unwrap().to_string_lossy();
    for module_id in program.top_level_modules.iter() {
        let module = ctx.program.modules.get(module_id).unwrap();
        let llvm_module = context.create_module(&module.name);
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
            module.module_id.program_id.try_into().unwrap(), // compile unit id?
            false,
            false,
            "",
            "edlang-sdk",
        );

        let di_namespace = di_builder
            .create_namespace(di_unit.as_debug_info_scope(), &module.name, true)
            .as_debug_info_scope();

        let mut module_ctx = ModuleCompileCtx {
            ctx,
            module: llvm_module,
            di_builder,
            di_unit,
            builder: &builder,
            target_data: machine.get_target_data(),
            module_id: *module_id,
            di_namespace,
        };

        compile_module(&mut module_ctx, *module_id);

        module_ctx.di_builder.finalize();
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

fn compile_module(ctx: &mut ModuleCompileCtx, module_id: DefId) {
    let module = ctx.ctx.program.modules.get(&module_id).unwrap();
    info!("compiling module");
    for id in module.functions.iter() {
        compile_fn_signature(ctx, *id);
    }

    for id in module.functions.iter() {
        compile_fn(ctx, *id).unwrap();
    }
}

fn compile_fn_signature(ctx: &ModuleCompileCtx<'_, '_>, fn_id: DefId) {
    let (arg_types, ret_type) = ctx.ctx.program.function_signatures.get(&fn_id).unwrap();
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
    info!("compiling fn sig: {}", body.name);

    let args: Vec<BasicMetadataTypeEnum> = arg_types
        .iter()
        .map(|x| compile_basic_type(ctx, x).into())
        .collect();

    let di_args: Vec<DIType> = arg_types
        .iter()
        .map(|x| compile_debug_type(ctx, x))
        .collect();
    let di_ret_type = if let ir::TypeKind::Unit = ret_type.kind {
        None
    } else {
        Some(compile_debug_type(ctx, ret_type))
    };

    let fn_type = if let ir::TypeKind::Unit = ret_type.kind {
        ctx.ctx.context.void_type().fn_type(&args, false)
    } else {
        compile_basic_type(ctx, ret_type).fn_type(&args, false)
    };

    let fn_value = ctx.module.add_function(
        &body.name,
        fn_type,
        Some(if body.is_extern {
            inkwell::module::Linkage::AvailableExternally
        } else if body.is_pub {
            inkwell::module::Linkage::External
        } else {
            inkwell::module::Linkage::Private
        }),
    );

    // nonlazybind
    fn_value.add_attribute(
        inkwell::attributes::AttributeLoc::Function,
        ctx.ctx.context.create_enum_attribute(37, 0),
    );
    let (_, line, _col) = ctx
        .ctx
        .session
        .source
        .get_offset_line(body.fn_span.lo)
        .unwrap();

    let di_type = ctx.di_builder.create_subroutine_type(
        ctx.di_unit.get_file(),
        di_ret_type,
        &di_args,
        if body.is_pub {
            DIFlagsConstants::PUBLIC
        } else {
            DIFlagsConstants::PRIVATE
        },
    );
    let subprogram = ctx.di_builder.create_function(
        ctx.di_namespace,
        &body.name,
        Some(&body.name),
        ctx.di_unit.get_file(),
        line as u32 + 1,
        di_type,
        body.is_pub,
        true,
        line as u32 + 1,
        0,
        false,
    );
    fn_value.set_subprogram(subprogram);
}

fn compile_fn(ctx: &ModuleCompileCtx, fn_id: DefId) -> Result<(), BuilderError> {
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
    info!("compiling fn body: {}", body.name);

    let fn_value = ctx.module.get_function(&body.name).unwrap();
    let di_program = fn_value.get_subprogram().unwrap();

    let mut debug_loc = ctx.set_debug_loc(di_program.as_debug_info_scope(), body.fn_span);
    let mut lexical_block = ctx.di_builder.create_lexical_block(
        debug_loc.get_scope(),
        ctx.di_unit.get_file(),
        debug_loc.get_line(),
        debug_loc.get_column(),
    );
    debug_loc = ctx.set_debug_loc(lexical_block.as_debug_info_scope(), body.fn_span);

    let block = ctx.ctx.context.append_basic_block(fn_value, "entry");
    ctx.builder.position_at_end(block);

    let mut locals = HashMap::new();
    let mut di_locals = HashMap::new();
    let mut ret_local = None;

    let mut arg_counter = 0;

    for (index, local) in body.locals.iter().enumerate() {
        if let Some(span) = local.span {
            debug_loc = ctx.set_debug_loc(debug_loc.get_scope(), span);
        }

        match local.kind {
            ir::LocalKind::Temp => {
                if let ir::TypeKind::Unit = &local.ty.kind {
                } else {
                    let ptr = ctx.builder.build_alloca(
                        compile_basic_type(ctx, &local.ty),
                        local.debug_name.as_deref().unwrap_or(&index.to_string()),
                    )?;
                    locals.insert(index, ptr);
                }
            }
            ir::LocalKind::Arg => {
                if let ir::TypeKind::Unit = &local.ty.kind {
                } else {
                    let ty = compile_basic_type(ctx, &local.ty);
                    let debug_ty = compile_debug_type(ctx, &local.ty);
                    let name = local.debug_name.as_ref().unwrap();
                    let ptr = ctx.builder.build_alloca(ty, name)?;
                    let value = fn_value.get_nth_param(arg_counter).unwrap();
                    ctx.builder.build_store(ptr, value)?;

                    let di_local = ctx.di_builder.create_parameter_variable(
                        debug_loc.get_scope(),
                        local.debug_name.as_ref().unwrap(),
                        arg_counter,
                        ctx.di_unit.get_file(),
                        debug_loc.get_line(),
                        debug_ty,
                        true,
                        0,
                    );
                    ctx.di_builder.insert_dbg_value_before(
                        value,
                        di_local,
                        None,
                        debug_loc,
                        block.get_first_instruction().unwrap(),
                    );

                    arg_counter += 1;
                    locals.insert(index, ptr);
                    di_locals.insert(index, di_local);
                }
            }
            ir::LocalKind::ReturnPointer => {
                if let ir::TypeKind::Unit = &local.ty.kind {
                } else {
                    ret_local = Some(index);
                    let ptr = ctx
                        .builder
                        .build_alloca(compile_basic_type(ctx, &local.ty), "return_ptr")?;
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
            if let Some(span) = stmt.span {
                debug_loc = ctx.set_debug_loc(debug_loc.get_scope(), span);
            }

            info!("compiling stmt");
            match &stmt.kind {
                ir::StatementKind::Assign(place, rvalue) => {
                    let local = &body.locals[place.local];

                    let (value, _value_ty) = compile_rvalue(ctx, fn_id, &locals, rvalue)?;
                    let instruction = ctx
                        .builder
                        .build_store(*locals.get(&place.local).unwrap(), value)?;

                    if let Some(_debug_name) = &local.debug_name {
                        let di_local = di_locals.get(&place.local).unwrap();
                        ctx.di_builder.insert_dbg_value_before(
                            value,
                            *di_local,
                            None,
                            debug_loc,
                            instruction,
                        );
                    }
                }
                ir::StatementKind::StorageLive(local_idx) => {
                    let local = &body.locals[*local_idx];

                    if local.debug_name.is_some() {
                        ctx.builder.set_current_debug_location(debug_loc);
                        let ty = compile_debug_type(ctx, &local.ty);
                        let var = match local.kind {
                            LocalKind::Temp => ctx.di_builder.create_auto_variable(
                                debug_loc.get_scope(),
                                local.debug_name.as_ref().unwrap(),
                                ctx.di_unit.get_file(),
                                debug_loc.get_line(),
                                ty,
                                true,
                                0,
                                ty.get_align_in_bits(),
                            ),
                            LocalKind::Arg => {
                                unreachable!()
                            }
                            LocalKind::ReturnPointer => unreachable!(),
                        };
                        di_locals.insert(*local_idx, var);
                        ctx.di_builder.insert_declare_at_end(
                            locals[&local_idx],
                            Some(var),
                            None,
                            debug_loc,
                            *llvm_block,
                        );
                    }

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
                destination: dest,
                target,
            } => {
                let target_fn_body = ctx.ctx.program.functions.get(func).unwrap();
                let fn_value = ctx.module.get_function(&target_fn_body.name).unwrap();
                let args: Vec<_> = args
                    .iter()
                    .map(|x| compile_rvalue(ctx, fn_id, &locals, x).unwrap().0.into())
                    .collect();
                let result = ctx.builder.build_call(fn_value, &args, "")?;

                let is_void = matches!(body.locals[dest.local].ty.kind, ir::TypeKind::Unit);

                if !is_void {
                    ctx.builder.build_store(
                        *locals.get(&dest.local).unwrap(),
                        result.try_as_basic_value().expect_left("value was right"),
                    )?;
                }

                if let Some(target) = target {
                    ctx.builder.build_unconditional_branch(blocks[*target])?;
                }
            }
            ir::Terminator::Unreachable => {
                ctx.builder.build_unreachable()?;
            }
        }
    }

    Ok(())
}

fn compile_bin_op<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: ir::BinOp,
    lhs: &ir::Operand,
    rhs: &ir::Operand,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    let (lhs_value, lhs_ty) = compile_load_operand(ctx, fn_id, locals, lhs)?;
    let (rhs_value, _rhs_ty) = compile_load_operand(ctx, fn_id, locals, rhs)?;

    let is_float = matches!(lhs_ty.kind, ir::TypeKind::Float(_));
    let is_signed = matches!(lhs_ty.kind, ir::TypeKind::Int(_));

    Ok(match op {
        ir::BinOp::Add => {
            let value = if is_float {
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
            };
            (value, lhs_ty)
        }
        ir::BinOp::Sub => {
            let value = if is_float {
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
            };
            (value, lhs_ty)
        }
        ir::BinOp::Mul => {
            let value = if is_float {
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
            };
            (value, lhs_ty)
        }
        ir::BinOp::Div => {
            let value = if is_float {
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
            };
            (value, lhs_ty)
        }
        ir::BinOp::Rem => {
            let value = if is_float {
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
            };
            (value, lhs_ty)
        }
        ir::BinOp::BitXor => (
            ctx.builder
                .build_xor(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                .as_basic_value_enum(),
            lhs_ty,
        ),
        ir::BinOp::BitAnd => (
            ctx.builder
                .build_and(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                .as_basic_value_enum(),
            lhs_ty,
        ),
        ir::BinOp::BitOr => (
            ctx.builder
                .build_or(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                .as_basic_value_enum(),
            lhs_ty,
        ),
        ir::BinOp::Shl => (
            ctx.builder
                .build_left_shift(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
                .as_basic_value_enum(),
            lhs_ty,
        ),
        ir::BinOp::Shr => (
            ctx.builder
                .build_right_shift(
                    lhs_value.into_int_value(),
                    rhs_value.into_int_value(),
                    is_signed,
                    "",
                )?
                .as_basic_value_enum(),
            lhs_ty,
        ),
        ir::BinOp::Eq => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Lt => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Le => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Ne => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Ge => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Gt => {
            let value = if is_float {
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
            };
            (
                value,
                TypeInfo {
                    span: None,
                    kind: ir::TypeKind::Bool,
                },
            )
        }
        ir::BinOp::Offset => todo!(),
    })
}

fn compile_rvalue<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    rvalue: &ir::RValue,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    Ok(match rvalue {
        ir::RValue::Use(op) => compile_load_operand(ctx, fn_id, locals, op)?,
        ir::RValue::Ref(_, _) => todo!(),
        ir::RValue::BinOp(op, lhs, rhs) => compile_bin_op(ctx, fn_id, locals, *op, lhs, rhs)?,
        ir::RValue::LogicOp(_, _, _) => todo!(),
        ir::RValue::UnOp(_, _) => todo!(),
    })
}

fn compile_load_operand<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: &ir::Operand,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    // todo: implement projection
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
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
            ir::ConstValue::U64(x) => ty.into_int_type().const_int(*x, true).as_basic_value_enum(),
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
            let (args, ret_type) = { ctx.ctx.program.function_signatures.get(def_id).unwrap() };

            let args: Vec<BasicMetadataTypeEnum> = args
                .iter()
                .map(|x| compile_basic_type(ctx, x).into())
                .collect();
            let ret_type = compile_basic_type(ctx, ret_type);

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
        ir::TypeKind::Unit => todo!(),
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

fn compile_debug_type<'ctx>(ctx: &ModuleCompileCtx<'ctx, '_>, ty: &ir::TypeInfo) -> DIType<'ctx> {
    // 1 == address
    // 2 = boolean
    // 4 = float
    // 5 = signed
    // 11 = numeric string
    match &ty.kind {
        ir::TypeKind::Unit => todo!(),
        ir::TypeKind::Bool => ctx
            .di_builder
            .create_basic_type("bool", 1, 2, LLVMDIFlagPublic)
            .unwrap()
            .as_type(),
        ir::TypeKind::Char => ctx
            .di_builder
            .create_basic_type("char", 1, 6, LLVMDIFlagPublic)
            .unwrap()
            .as_type(),
        ir::TypeKind::Int(ty) => match ty {
            ir::IntTy::I128 => ctx
                .di_builder
                .create_basic_type("i128", 128, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I64 => ctx
                .di_builder
                .create_basic_type("i64", 64, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I32 => ctx
                .di_builder
                .create_basic_type("i32", 32, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I16 => ctx
                .di_builder
                .create_basic_type("i16", 16, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I8 => ctx
                .di_builder
                .create_basic_type("i8", 8, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::Isize => ctx
                .di_builder
                .create_basic_type("isize", 64, 5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::Uint(ty) => match ty {
            ir::UintTy::U128 => ctx
                .di_builder
                .create_basic_type("u128", 128, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U64 => ctx
                .di_builder
                .create_basic_type("u64", 64, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U32 => ctx
                .di_builder
                .create_basic_type("u32", 32, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U16 => ctx
                .di_builder
                .create_basic_type("u16", 16, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U8 => ctx
                .di_builder
                .create_basic_type("u8", 8, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::Usize => ctx
                .di_builder
                .create_basic_type("usize", 64, 7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::Float(ty) => match ty {
            ir::FloatTy::F32 => ctx
                .di_builder
                .create_basic_type("f32", 32, 4, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::FloatTy::F64 => ctx
                .di_builder
                .create_basic_type("f64", 64, 4, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::FnDef(_def_id, _generic_args) => {
            panic!()
        }
    }
}
