use llvm_sys::prelude::{LLVMContextRef, LLVMModuleRef};

extern "C" {
    /// Translate operation that satisfies LLVM dialect module requirements into an LLVM IR module living in the given context.
    /// This translates operations from any dilalect that has a registered implementation of LLVMTranslationDialectInterface.
    pub fn mlirTranslateModuleToLLVMIR(
        module_operation_ptr: mlir_sys::MlirOperation,
        llvm_context: LLVMContextRef,
    ) -> LLVMModuleRef;
}
