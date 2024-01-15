use std::error::Error;

use edlang_ast::Module;
use edlang_session::Session;
use melior::{
    dialect::DialectRegistry,
    ir::{operation::OperationPrintingFlags, Location, Module as MeliorModule},
    pass::{self, PassManager},
    utility::{register_all_dialects, register_all_llvm_translations, register_all_passes},
    Context as MeliorContext,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Context {
    melior_context: MeliorContext,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        let melior_context = initialize_mlir();
        Self { melior_context }
    }

    pub fn compile(
        &self,
        session: &Session,
        module: &Module,
    ) -> Result<MeliorModule, Box<dyn Error>> {
        let file_path = session.file_path.display().to_string();
        let location = Location::new(&self.melior_context, &file_path, 0, 0);

        let mut melior_module = MeliorModule::new(location);

        super::codegen::compile_module(session, &self.melior_context, &melior_module, module)?;

        assert!(melior_module.as_operation().verify());

        tracing::debug!(
            "MLIR Code before passes:\n{}",
            melior_module.as_operation().to_string_with_flags(
                OperationPrintingFlags::new().enable_debug_info(true, true)
            )?
        );

        // TODO: Add proper error handling.
        self.run_pass_manager(&mut melior_module)?;

        tracing::debug!(
            "MLIR Code after passes:\n{}",
            melior_module.as_operation().to_string_with_flags(
                OperationPrintingFlags::new().enable_debug_info(true, true)
            )?
        );

        Ok(melior_module)
    }

    fn run_pass_manager(&self, module: &mut MeliorModule) -> Result<(), melior::Error> {
        let pass_manager = PassManager::new(&self.melior_context);
        pass_manager.enable_verifier(true);
        pass_manager.add_pass(pass::transform::create_canonicalizer());
        pass_manager.add_pass(pass::conversion::create_scf_to_control_flow());
        pass_manager.add_pass(pass::conversion::create_arith_to_llvm());
        pass_manager.add_pass(pass::conversion::create_control_flow_to_llvm());
        pass_manager.add_pass(pass::conversion::create_func_to_llvm());
        pass_manager.add_pass(pass::conversion::create_index_to_llvm());
        pass_manager.add_pass(pass::conversion::create_finalize_mem_ref_to_llvm());
        pass_manager.add_pass(pass::conversion::create_reconcile_unrealized_casts());
        pass_manager.run(module)
    }
}

/// Initialize an MLIR context.
pub fn initialize_mlir() -> MeliorContext {
    let context = MeliorContext::new();
    context.append_dialect_registry(&{
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);
        registry
    });
    context.load_all_available_dialects();
    register_all_passes();
    register_all_llvm_translations(&context);
    context
}
