use edlang_ast::{Ident, Span};
use edlang_ir::{TypeInfo, TypeKind};
use thiserror::Error;

use crate::DefId;

#[derive(Debug, Error, Clone)]
pub enum LoweringError {
    #[error("module {module:?} not found")]
    ModuleNotFound { span: Span, module: String },
    #[error("function {function:?} not found")]
    FunctionNotFound { span: Span, function: String },
    #[error("symbol {:?} not found", symbol.name)]
    ImportNotFound {
        module_span: Span,
        import_span: Span,
        symbol: Ident,
    },
    #[error("trying to mutate a non-mutable reference")]
    BorrowNotMutable {
        span: Span,
        type_span: Option<Span>,
        name: String,
    },
    #[error("unrecognized type {name}")]
    UnrecognizedType { span: Span, name: String },
    #[error("id not found")]
    IdNotFound { span: Span, id: DefId },
    #[error("feature not yet implemented: {message}")]
    NotYetImplemented { span: Span, message: &'static str },
    #[error("unexpected type")]
    UnexpectedType {
        span: Span,
        found: TypeKind,
        expected: TypeInfo,
    },
    #[error("use of underclared variable {name:?}")]
    UseOfUndeclaredVariable { span: Span, name: String },
}
