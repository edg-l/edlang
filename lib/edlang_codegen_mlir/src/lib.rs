#![allow(clippy::too_many_arguments)]

use std::{
    ffi::{CStr, CString},
    mem::MaybeUninit,
    path::PathBuf,
    ptr::{addr_of_mut, null_mut},
    sync::OnceLock,
};

use edlang_session::{OptLevel, Session};
use llvm_sys::{
    core::{LLVMContextCreate, LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule},
    target::{
        LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs,
        LLVM_InitializeAllTargets,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
        LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetHostCPUFeatures,
        LLVMGetHostCPUName, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile,
        LLVMTargetRef,
    },
};
use melior::ir::Module;

use crate::ffi::mlirTranslateModuleToLLVMIR;

pub mod codegen;
mod ffi;
pub mod linker;

/// Converts a module to an object.
/// The object will be written to the specified target path.
/// TODO: error handling
///
/// Returns the path to the object.
pub fn compile_to_object(
    session: &Session,
    module: &Module,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    tracing::debug!("Compiling to object file");
    if !session.target_dir.exists() {
        std::fs::create_dir_all(&session.target_dir)?;
    }

    let target_file = session
        .file_path
        .clone()
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let target_file = PathBuf::from(target_file).with_extension("o");
    tracing::debug!("Target file: {:?}", target_file);

    let target_path = session.target_dir.join(target_file);

    // TODO: Rework so you can specify target and host features, etc.
    // Right now it compiles for the native cpu feature set and arch.
    static INITIALIZED: OnceLock<()> = OnceLock::new();

    INITIALIZED.get_or_init(|| unsafe {
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmPrinters();
        tracing::debug!("initialized llvm targets");
    });

    unsafe {
        let llvm_context = LLVMContextCreate();

        let op = module.as_operation().to_raw();

        let llvm_module = mlirTranslateModuleToLLVMIR(op, llvm_context);

        let mut null = null_mut();
        let mut error_buffer = addr_of_mut!(null);

        let target_triple = LLVMGetDefaultTargetTriple();
        tracing::debug!("Target triple: {:?}", CStr::from_ptr(target_triple));

        let target_cpu = LLVMGetHostCPUName();
        tracing::debug!("Target CPU: {:?}", CStr::from_ptr(target_cpu));

        let target_cpu_features = LLVMGetHostCPUFeatures();
        tracing::debug!(
            "Target CPU Features: {:?}",
            CStr::from_ptr(target_cpu_features)
        );

        let mut target: MaybeUninit<LLVMTargetRef> = MaybeUninit::uninit();

        if LLVMGetTargetFromTriple(target_triple, target.as_mut_ptr(), error_buffer) != 0 {
            let error = CStr::from_ptr(*error_buffer);
            let err = error.to_string_lossy().to_string();
            tracing::error!("error getting target triple: {}", err);
            LLVMDisposeMessage(*error_buffer);
            panic!("{err}")
        } else if !(*error_buffer).is_null() {
            LLVMDisposeMessage(*error_buffer);
            error_buffer = addr_of_mut!(null);
        }

        let target = target.assume_init();

        let machine = LLVMCreateTargetMachine(
            target,
            target_triple.cast(),
            target_cpu.cast(),
            target_cpu_features.cast(),
            match session.optlevel {
                OptLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                OptLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
                OptLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                OptLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
            },
            if session.library {
                LLVMRelocMode::LLVMRelocDynamicNoPic
            } else {
                LLVMRelocMode::LLVMRelocDefault
            },
            LLVMCodeModel::LLVMCodeModelDefault,
        );

        let filename = CString::new(target_path.as_os_str().to_string_lossy().as_bytes()).unwrap();
        tracing::debug!("filename to llvm: {:?}", filename);
        let ok = LLVMTargetMachineEmitToFile(
            machine,
            llvm_module,
            filename.as_ptr().cast_mut(),
            LLVMCodeGenFileType::LLVMObjectFile, // object (binary) or assembly (textual)
            error_buffer,
        );

        if ok != 0 {
            let error = CStr::from_ptr(*error_buffer);
            let err = error.to_string_lossy().to_string();
            tracing::error!("error emitting to file: {:?}", err);
            LLVMDisposeMessage(*error_buffer);
            panic!("{err}")
        } else if !(*error_buffer).is_null() {
            LLVMDisposeMessage(*error_buffer);
        }

        LLVMDisposeTargetMachine(machine);
        LLVMDisposeModule(llvm_module);
        LLVMContextDispose(llvm_context);

        Ok(target_path)
    }
}
