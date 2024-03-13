use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    path::PathBuf,
};

use edlang_ir as ir;
use edlang_ir::DefId;
use edlang_session::Session;
use edlang_span::Span;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::{
        AsDIScope, DICompileUnit, DIFlagsConstants, DILocation, DIScope, DIType, DebugInfoBuilder,
    },
    module::Module,
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetData, TargetMachine},
    types::{AnyType, BasicMetadataTypeEnum, BasicType},
    values::{BasicValue, BasicValueEnum, PointerValue},
    AddressSpace,
};
use ir::{LocalKind, ModuleBody, ProgramBody, TypeInfo, ValueTree};
use llvm_sys::debuginfo::LLVMDIFlagPublic;
use tracing::{info, trace};

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
        let (_, line, column) = self.ctx.session.sources[self.get_module_body().file_id]
            .get_offset_line(span.lo)
            .unwrap();
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

    let mut llvm_modules = VecDeque::new();

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
            match session.optlevel {
                edlang_session::OptLevel::None => inkwell::OptimizationLevel::None,
                edlang_session::OptLevel::Less => inkwell::OptimizationLevel::Less,
                edlang_session::OptLevel::Default => inkwell::OptimizationLevel::Default,
                edlang_session::OptLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
            },
            if session.library {
                inkwell::targets::RelocMode::DynamicNoPic
            } else {
                inkwell::targets::RelocMode::Default
            },
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();
    // machine.set_asm_verbosity(true);

    info!("compiling for: {:?}", target.get_description());

    for module_id in program.top_level_modules.iter() {
        let module = ctx.program.modules.get(module_id).unwrap();
        let file_path = session.file_paths[module.file_id].clone();
        let abs_file_path = file_path
            .canonicalize()
            .expect("failed to canonicalize file path");
        let filename = file_path.file_name().unwrap().to_str().unwrap();
        let dirname = abs_file_path
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();

        let llvm_module = context.create_module(&module.name);
        llvm_module.set_source_file_name(filename);
        llvm_module.set_triple(&triple);
        llvm_module.set_data_layout(&machine.get_target_data().get_data_layout());
        let (di_builder, di_unit) = llvm_module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::Rust,
            filename,
            dirname,
            "edlang",
            true,
            "", // compiler flags
            1,
            "", // split name
            match session.debug_info {
                edlang_session::DebugInfo::None => inkwell::debug_info::DWARFEmissionKind::None,
                edlang_session::DebugInfo::Full => inkwell::debug_info::DWARFEmissionKind::Full,
            },
            module.module_id.program_id.try_into().unwrap(), // compile unit id?
            false,
            false,
            "",
            "edlang-sdk",
        );

        let di_namespace = di_builder
            .create_namespace(di_unit.get_file().as_debug_info_scope(), &module.name, true)
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

        // todo link modules together
        llvm_modules.push_back(module_ctx.module);
    }

    let module = llvm_modules.pop_front().unwrap();

    for x in llvm_modules.into_iter() {
        module.link_in_module(x)?;
    }

    let opt = match session.optlevel {
        edlang_session::OptLevel::None => "0",
        edlang_session::OptLevel::Less => "1",
        edlang_session::OptLevel::Default => "2",
        edlang_session::OptLevel::Aggressive => "3",
    };

    let passopt = PassBuilderOptions::create();

    module.run_passes(&format!("default<O{}>", opt), &machine, passopt)?;

    if session.output_llvm {
        module.print_to_file(session.output_file.with_extension("ll"))?;
    }

    if session.output_asm {
        machine.write_to_file(
            &module,
            inkwell::targets::FileType::Assembly,
            &session.output_file.with_extension("asm"),
        )?;
    }

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Object,
        &session.output_file.with_extension("o"),
    )?;

    Ok(session.output_file.with_extension("o"))
}

fn compile_module(ctx: &mut ModuleCompileCtx, module_id: DefId) {
    let module = ctx.ctx.program.modules.get(&module_id).unwrap();
    trace!("compiling module: {:?}", module_id);
    for id in module.functions.iter() {
        compile_fn_signature(ctx, *id, true);
    }

    for id in module.functions.iter() {
        compile_fn(ctx, *id).unwrap();
    }
}

fn compile_fn_signature(ctx: &ModuleCompileCtx<'_, '_>, fn_id: DefId, is_definition: bool) {
    let (arg_types, ret_type) = ctx.ctx.program.function_signatures.get(&fn_id).unwrap();
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
    trace!("compiling fn sig: {}", body.name);

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
        &body.get_mangled_name(),
        fn_type,
        Some(if body.is_extern || body.is_exported {
            inkwell::module::Linkage::External
        } else {
            inkwell::module::Linkage::Private
        }),
    );

    // https://llvm.org/doxygen/group__LLVMCCoreTypes.html

    fn_value.set_call_conventions(0); // cconv

    let (_, line, _col) = ctx.ctx.session.sources[ctx.get_module_body().file_id]
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

    if fn_value.get_subprogram().is_none() {
        let subprogram = ctx.di_builder.create_function(
            ctx.di_namespace,
            &body.name,
            Some(&body.get_mangled_name()),
            ctx.di_unit.get_file(),
            line as u32 + 1,
            di_type,
            body.is_exported || body.is_extern,
            is_definition && !body.is_extern,
            line as u32 + 1,
            0,
            false,
        );
        fn_value.set_subprogram(subprogram);
    }
}

fn compile_fn(ctx: &ModuleCompileCtx, fn_id: DefId) -> Result<(), BuilderError> {
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();

    if body.is_extern {
        return Ok(());
    }

    trace!("compiling fn body: {}", body.name);

    let fn_value = ctx.module.get_function(&body.get_mangled_name()).unwrap();
    let di_program = fn_value.get_subprogram().unwrap();

    let mut debug_loc = ctx.set_debug_loc(di_program.as_debug_info_scope(), body.fn_span);
    let lexical_block = ctx.di_builder.create_lexical_block(
        debug_loc.get_scope(),
        ctx.di_unit.get_file(),
        debug_loc.get_line(),
        debug_loc.get_column(),
    );
    debug_loc = ctx.set_debug_loc(lexical_block.as_debug_info_scope(), body.fn_span);

    trace!("compiling entry block");

    let block = ctx.ctx.context.append_basic_block(fn_value, "entry");
    ctx.builder.position_at_end(block);

    let mut locals = HashMap::new();
    let mut di_locals = HashMap::new();
    let mut ret_local = None;

    let mut arg_counter = 0;

    for (index, local) in body.locals.iter().enumerate() {
        trace!("compiling local {}: {:?}", index, local);
        if let Some(span) = local.span {
            debug_loc = ctx.set_debug_loc(debug_loc.get_scope(), span);
        }

        match local.kind {
            ir::LocalKind::Temp => {
                if let ir::TypeKind::Unit = &local.ty.kind {
                } else {
                    let ptr = ctx.builder.build_alloca(
                        compile_basic_type(ctx, &local.ty),
                        local
                            .debug_name
                            .as_deref()
                            .unwrap_or(&format!("temp_var_{index}")),
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

    trace!("creating blocks");
    let mut blocks = Vec::with_capacity(body.blocks.len());

    for (index, _block) in body.blocks.iter().enumerate() {
        let llvm_block = ctx
            .ctx
            .context
            .append_basic_block(fn_value, &format!("block_{index}"));
        blocks.push(llvm_block);
    }

    ctx.builder.build_unconditional_branch(blocks[0])?;

    for (block_idx, (block, llvm_block)) in body.blocks.iter().zip(&blocks).enumerate() {
        trace!("compiling block {}", block_idx);
        ctx.builder.position_at_end(*llvm_block);
        for (idx, stmt) in block.statements.iter().enumerate() {
            if let Some(span) = stmt.span {
                debug_loc = ctx.set_debug_loc(debug_loc.get_scope(), span);
            }

            trace!("compiling stmt {}: {:?}", idx, stmt);
            match &stmt.kind {
                ir::StatementKind::Assign(place, rvalue) => {
                    let local = &body.locals[place.local];
                    let mut local_ty = local.ty.clone();
                    let mut ptr = *locals.get(&place.local).unwrap();

                    for proj in &place.projection {
                        match proj {
                            ir::PlaceElem::Deref => {
                                ptr = ctx
                                    .builder
                                    .build_load(compile_basic_type(ctx, &local_ty), ptr, "deref")?
                                    .into_pointer_value();
                                local_ty = match local_ty.kind {
                                    ir::TypeKind::Ptr(_, inner) => *inner,
                                    ir::TypeKind::Ref(_, inner) => *inner,
                                    _ => unreachable!(),
                                }
                            }
                            ir::PlaceElem::Field { field_idx } => {
                                ptr = ctx.builder.build_struct_gep(
                                    compile_basic_type(ctx, &local_ty),
                                    ptr,
                                    (*field_idx) as u32,
                                    "",
                                )?;
                                local_ty = match local_ty.kind {
                                    ir::TypeKind::Struct(id, _) => {
                                        let strc = ctx.ctx.program.structs.get(&id).unwrap();
                                        strc.variants[*field_idx].ty.clone()
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            ir::PlaceElem::Index { .. } => todo!(),
                        }
                    }

                    let (value, _value_ty) = compile_rvalue(ctx, fn_id, &locals, rvalue)?;
                    let instruction = ctx.builder.build_store(ptr, value)?;

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

        trace!("compiling terminator: {:?}", block.terminator);
        if let Some(span) = block.terminator_span {
            debug_loc = ctx.set_debug_loc(debug_loc.get_scope(), span);
        }
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
            ir::Terminator::SwitchInt {
                discriminator,
                targets,
            } => {
                let (condition, _condition_ty) =
                    compile_load_operand(ctx, fn_id, &locals, discriminator)?;
                let cond = condition.into_int_value();
                let mut cases = Vec::new();

                for (value, target) in targets.values.iter().zip(targets.targets.iter()) {
                    let target = *target;
                    let ty_kind = value.get_type();
                    let block = blocks[target];
                    let value = compile_value(
                        ctx,
                        value,
                        &TypeInfo {
                            span: None,
                            kind: ty_kind,
                        },
                    )?
                    .into_int_value();
                    cases.push((value, block));
                }

                ctx.builder
                    .build_switch(cond, blocks[*targets.targets.last().unwrap()], &cases)?;
            }
            ir::Terminator::Call {
                func,
                args,
                destination: dest,
                target,
            } => {
                let target_fn_body = ctx.ctx.program.functions.get(func).unwrap();
                // compile_fn_signature(ctx, *id, true);
                let fn_value = match ctx.module.get_function(&target_fn_body.get_mangled_name()) {
                    Some(x) => x,
                    None => {
                        compile_fn_signature(ctx, target_fn_body.def_id, false);
                        ctx.module
                            .get_function(&target_fn_body.get_mangled_name())
                            .unwrap()
                    }
                };
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

fn compile_unary_op<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: ir::UnOp,
    value: &ir::Operand,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    let (value, ty) = compile_load_operand(ctx, fn_id, locals, value)?;

    let is_float = matches!(ty.kind, ir::TypeKind::Float(_));

    Ok(match op {
        ir::UnOp::Not => {
            assert!(ty.kind.is_integer(), "must be a integer");
            let value = ctx
                .builder
                .build_not(value.into_int_value(), "not")?
                .as_basic_value_enum();
            (value, ty)
        }
        ir::UnOp::Neg => {
            let value = if is_float {
                ctx.builder
                    .build_float_neg(value.into_float_value(), "negf")?
                    .as_basic_value_enum()
            } else {
                ctx.builder
                    .build_int_neg(value.into_int_value(), "negi")?
                    .as_basic_value_enum()
            };
            (value, ty)
        }
    })
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

    let (result, ty) = match op {
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
                    .build_int_mul(lhs_value.into_int_value(), rhs_value.into_int_value(), "")?
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
    };

    Ok((result, ty))
}

fn compile_rvalue<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    rvalue: &ir::RValue,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    Ok(match rvalue {
        ir::RValue::Use(op, span) => {
            ctx.set_debug_loc(
                ctx.builder
                    .get_current_debug_location()
                    .unwrap()
                    .get_scope(),
                *span,
            );
            compile_load_operand(ctx, fn_id, locals, op)?
        }
        ir::RValue::Ref(_mutable, op, span) => {
            ctx.set_debug_loc(
                ctx.builder
                    .get_current_debug_location()
                    .unwrap()
                    .get_scope(),
                *span,
            );
            match op {
                ir::Operand::Copy(_) => todo!(),
                ir::Operand::Move(place) => {
                    let mut ptr = *locals.get(&place.local).unwrap();
                    let mut local_ty = {
                        let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
                        body.locals[place.local].ty.clone()
                    };

                    for proj in &place.projection {
                        match proj {
                            ir::PlaceElem::Deref => {
                                local_ty = match local_ty.kind {
                                    ir::TypeKind::Ptr(_, inner) => *inner,
                                    ir::TypeKind::Ref(_, inner) => *inner,
                                    _ => unreachable!(),
                                };
                                ptr = ctx
                                    .builder
                                    .build_load(compile_basic_type(ctx, &local_ty), ptr, "deref")?
                                    .into_pointer_value();
                            }
                            ir::PlaceElem::Field { .. } => todo!(),
                            ir::PlaceElem::Index { .. } => todo!(),
                        }
                    }

                    (ptr.as_basic_value_enum(), local_ty)
                }
                ir::Operand::Constant(_) => todo!("references to constants not yet implemented"),
            }
        }
        ir::RValue::BinOp(op, lhs, rhs, span) => {
            ctx.set_debug_loc(
                ctx.builder
                    .get_current_debug_location()
                    .unwrap()
                    .get_scope(),
                *span,
            );
            compile_bin_op(ctx, fn_id, locals, *op, lhs, rhs)?
        }
        ir::RValue::LogicOp(_, _, _, _span) => todo!(),
        ir::RValue::UnOp(op, value, span) => {
            ctx.set_debug_loc(
                ctx.builder
                    .get_current_debug_location()
                    .unwrap()
                    .get_scope(),
                *span,
            );
            compile_unary_op(ctx, fn_id, locals, *op, value)?
        }
        ir::RValue::Cast(op, target_ty, span) => {
            ctx.set_debug_loc(
                ctx.builder
                    .get_current_debug_location()
                    .unwrap()
                    .get_scope(),
                *span,
            );

            let target_ty = target_ty.clone();
            let target_llvm_ty = compile_basic_type(ctx, &target_ty);
            let (value, ty) = compile_load_operand(ctx, fn_id, locals, op)?;
            let current_ty = compile_basic_type(ctx, &ty);

            if target_llvm_ty.is_pointer_type() {
                // int to ptr
                let target_llvm_ty = target_llvm_ty.into_pointer_type();
                if current_ty.is_int_type() {
                    let value =
                        ctx.builder
                            .build_int_to_ptr(value.into_int_value(), target_llvm_ty, "")?;
                    (value.as_basic_value_enum(), target_ty)
                } else if current_ty.is_pointer_type() {
                    (value, target_ty.clone())
                } else {
                    unreachable!("cast from {:?} to ptr", current_ty)
                }
            } else if target_llvm_ty.is_int_type() {
                let is_signed = target_ty.kind.is_signed_integer();
                let target_llvm_ty = target_llvm_ty.into_int_type();
                if current_ty.is_int_type() {
                    // int to int casts
                    let current_ty = current_ty.into_int_type();

                    match current_ty
                        .get_bit_width()
                        .cmp(&target_llvm_ty.get_bit_width())
                    {
                        std::cmp::Ordering::Greater => {
                            let value = ctx.builder.build_int_truncate(
                                value.into_int_value(),
                                target_llvm_ty,
                                "",
                            )?;
                            (value.as_basic_value_enum(), target_ty)
                        }
                        std::cmp::Ordering::Equal => (value, target_ty.clone()),
                        std::cmp::Ordering::Less => {
                            if is_signed {
                                let value = ctx.builder.build_int_s_extend(
                                    value.into_int_value(),
                                    target_llvm_ty,
                                    "",
                                )?;
                                (value.as_basic_value_enum(), target_ty)
                            } else {
                                let value = ctx.builder.build_int_z_extend(
                                    value.into_int_value(),
                                    target_llvm_ty,
                                    "",
                                )?;
                                (value.as_basic_value_enum(), target_ty)
                            }
                        }
                    }
                } else if current_ty.is_float_type() {
                    // float to int casts
                    if is_signed {
                        let value = ctx.builder.build_float_to_signed_int(
                            value.into_float_value(),
                            target_llvm_ty,
                            "",
                        )?;
                        (value.as_basic_value_enum(), target_ty)
                    } else {
                        let value = ctx.builder.build_float_to_unsigned_int(
                            value.into_float_value(),
                            target_llvm_ty,
                            "",
                        )?;
                        (value.as_basic_value_enum(), target_ty)
                    }
                } else if current_ty.is_pointer_type() {
                    // ptr to int
                    let value = ctx.builder.build_ptr_to_int(
                        value.into_pointer_value(),
                        target_llvm_ty,
                        "",
                    )?;
                    (value.as_basic_value_enum(), target_ty)
                } else {
                    todo!("cast {:?} to int", current_ty)
                }
            } else {
                todo!("cast from {:?} to {:?}", current_ty, target_llvm_ty)
            }
        }
    })
}

fn compile_load_operand<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    op: &ir::Operand,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    Ok(match op {
        ir::Operand::Copy(place) => compile_load_place(ctx, fn_id, locals, place, true)?,
        ir::Operand::Move(place) => compile_load_place(ctx, fn_id, locals, place, false)?,
        ir::Operand::Constant(data) => match &data.kind {
            ir::ConstKind::Value(value) => (
                compile_value(ctx, value, &data.type_info)?,
                data.type_info.clone(),
            ),
            ir::ConstKind::ZeroSized => todo!(),
        },
    })
}

fn compile_load_place<'ctx>(
    ctx: &ModuleCompileCtx<'ctx, '_>,
    fn_id: DefId,
    locals: &HashMap<usize, PointerValue<'ctx>>,
    place: &ir::Place,
    _is_copy: bool,
) -> Result<(BasicValueEnum<'ctx>, TypeInfo), BuilderError> {
    let body = ctx.ctx.program.functions.get(&fn_id).unwrap();
    let mut ptr = *locals.get(&place.local).unwrap();
    let mut local_ty = body.locals[place.local].ty.clone();

    for proj in &place.projection {
        match proj {
            ir::PlaceElem::Deref => {
                ptr = ctx
                    .builder
                    .build_load(compile_basic_type(ctx, &local_ty), ptr, "deref")?
                    .into_pointer_value();
                local_ty = match local_ty.kind {
                    ir::TypeKind::Ptr(_, inner) => *inner,
                    ir::TypeKind::Ref(_, inner) => *inner,
                    _ => unreachable!(),
                }
            }
            ir::PlaceElem::Field { field_idx } => {
                local_ty = match local_ty.kind {
                    ir::TypeKind::Struct(id, _) => {
                        let struct_body = ctx.ctx.program.structs.get(&id).unwrap();
                        let ty = struct_body.variants[*field_idx].ty.clone();
                        let field_name = struct_body.variants[*field_idx].name.clone();
                        ptr = ctx.builder.build_struct_gep(
                            compile_basic_type(ctx, &local_ty),
                            ptr,
                            (*field_idx).try_into().unwrap(),
                            &format!("ptr_field_{field_name}"),
                        )?;
                        ty
                    }
                    _ => unreachable!(),
                }
            }
            ir::PlaceElem::Index { .. } => todo!(),
        }
    }

    let pointee_ty = compile_basic_type(ctx, &local_ty);

    Ok((
        ctx.builder.build_load(pointee_ty, ptr, "")?,
        body.locals[place.local].ty.clone(),
    ))
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
            ir::ConstValue::Char(x) => ty
                .into_int_type()
                .const_int((*x) as u64, false)
                .as_basic_value_enum(),
            ir::ConstValue::Isize(x) => ty
                .into_int_type()
                .const_int((*x) as u64, true)
                .as_basic_value_enum(),
        },
        ValueTree::Branch(_) => todo!(),
    })
}

fn _compile_type<'a>(
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
        ir::TypeKind::Ptr(_is_mut, _pointee) => ctx
            .ctx
            .context
            .ptr_sized_int_type(&ctx.target_data, None)
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        ir::TypeKind::Ref(_, _) => ctx
            .ctx
            .context
            .ptr_sized_int_type(&ctx.target_data, None)
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        ir::TypeKind::Struct(id, _) => {
            let body = ctx.ctx.program.structs.get(id).unwrap();

            let mut fields = Vec::new();

            for field in &body.variants {
                let ty = compile_basic_type(ctx, &field.ty);
                fields.push(ty);
            }

            ctx.ctx
                .context
                .struct_type(&fields, false)
                .as_basic_type_enum()
        }
        ir::TypeKind::Str => todo!(),
    }
}

fn compile_debug_type<'ctx>(ctx: &ModuleCompileCtx<'ctx, '_>, ty: &ir::TypeInfo) -> DIType<'ctx> {
    // 1 == address
    // 2 = boolean
    // 4 = float
    // 5 = signed
    // 11 = numeric string
    // https://dwarfstd.org/doc/DWARF5.pdf#section.7.8

    // https://github.com/GaloisInc/dwarf-tools/blob/master/src/DWARF/DW/TAG.hs
    let name = ty.kind.to_string();
    let name = &name;
    match &ty.kind {
        ir::TypeKind::Unit => todo!(),
        ir::TypeKind::Bool => ctx
            .di_builder
            .create_basic_type(name, 1, 0x2, LLVMDIFlagPublic)
            .unwrap()
            .as_type(),
        ir::TypeKind::Char => ctx
            .di_builder
            .create_basic_type(name, 8, 0x6, LLVMDIFlagPublic)
            .unwrap()
            .as_type(),
        ir::TypeKind::Int(ty) => match ty {
            ir::IntTy::I128 => ctx
                .di_builder
                .create_basic_type(name, 128, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I64 => ctx
                .di_builder
                .create_basic_type(name, 64, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I32 => ctx
                .di_builder
                .create_basic_type(name, 32, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I16 => ctx
                .di_builder
                .create_basic_type(name, 16, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::I8 => ctx
                .di_builder
                .create_basic_type(name, 8, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::IntTy::Isize => ctx
                .di_builder
                .create_basic_type(name, 64, 0x5, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::Uint(ty) => match ty {
            ir::UintTy::U128 => ctx
                .di_builder
                .create_basic_type(name, 128, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U64 => ctx
                .di_builder
                .create_basic_type(name, 64, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U32 => ctx
                .di_builder
                .create_basic_type(name, 32, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U16 => ctx
                .di_builder
                .create_basic_type(name, 16, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::U8 => ctx
                .di_builder
                .create_basic_type(name, 8, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::UintTy::Usize => ctx
                .di_builder
                .create_basic_type(name, 64, 0x7, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::Float(ty) => match ty {
            ir::FloatTy::F32 => ctx
                .di_builder
                .create_basic_type(name, 32, 4, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
            ir::FloatTy::F64 => ctx
                .di_builder
                .create_basic_type(name, 64, 4, LLVMDIFlagPublic)
                .unwrap()
                .as_type(),
        },
        ir::TypeKind::FnDef(_def_id, _generic_args) => {
            panic!()
        }
        ir::TypeKind::Ptr(_is_mut, pointee) => ctx
            .di_builder
            .create_pointer_type(
                name,
                compile_debug_type(ctx, pointee),
                (ctx.target_data.get_pointer_byte_size(None) * 8).into(),
                ctx.target_data.get_pointer_byte_size(None),
                AddressSpace::default(),
            )
            .as_type(),
        ir::TypeKind::Ref(_, inner) => ctx
            .di_builder
            .create_reference_type(compile_debug_type(ctx, inner), 0x10)
            .as_type(),
        ir::TypeKind::Struct(id, _) => {
            let body = ctx.ctx.program.structs.get(id).unwrap();

            let mut fields = Vec::new();

            for field in &body.variants {
                let ty = compile_debug_type(ctx, &field.ty);
                fields.push(ty);
            }

            let (_, line, _column) = ctx.ctx.session.sources[ctx.get_module_body().file_id]
                .get_offset_line(body.span.lo)
                .unwrap();
            let real_ty = compile_basic_type(ctx, ty);

            ctx.di_builder
                .create_struct_type(
                    ctx.di_namespace,
                    &body.name,
                    ctx.di_unit.get_file(),
                    (line + 1).try_into().unwrap(),
                    ctx.target_data.get_bit_size(&real_ty),
                    ctx.target_data.get_abi_alignment(&real_ty),
                    0,
                    None,
                    &fields,
                    0,
                    None,
                    &body.name,
                )
                .as_type()
        }
        ir::TypeKind::Str => todo!(),
    }
}
