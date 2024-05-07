use edlang_ast::{Ident, Span};
use edlang_ir::{TypeInfo, TypeKind};
use thiserror::Error;

use crate::DefId;

#[derive(Debug, Error, Clone)]
pub enum LoweringError {
    #[error("module {module:?} not found")]
    ModuleNotFound {
        span: Span,
        module: String,
        file_id: usize,
    },
    #[error("function {function:?} not found")]
    FunctionNotFound {
        span: Span,
        function: String,
        file_id: usize,
    },
    #[error("symbol {:?} not found", symbol.name)]
    ImportNotFound {
        module_span: Span,
        import_span: Span,
        symbol: Ident,
        file_id: usize,
    },
    #[error("trying to mutate a non-mutable reference")]
    BorrowNotMutable {
        span: Span,
        type_span: Option<Span>,
        name: String,
        file_id: usize,
    },
    #[error("unrecognized type {name}")]
    UnrecognizedType {
        span: Span,
        name: String,
        file_id: usize,
    },
    #[error("id not found")]
    IdNotFound {
        span: Span,
        id: DefId,
        file_id: usize,
    },
    #[error("feature not yet implemented: {message}")]
    NotYetImplemented {
        span: Span,
        message: &'static str,
        file_id: usize,
    },
    #[error("unexpected type")]
    UnexpectedType {
        span: Span,
        found: TypeKind,
        expected: TypeInfo,
        file_id: usize,
    },
    #[error("use of underclared variable {name:?}")]
    UseOfUndeclaredVariable {
        span: Span,
        name: String,
        file_id: usize,
    },
    #[error("parameter count mismatch to function call")]
    ParamCountMismatch {
        span: Span,
        has_args: usize,
        needs: usize,
        file_id: usize,
    },
    #[error("can't mutate this value because it's not declared mutable")]
    NotMutable {
        span: Span,
        declare_span: Option<Span>,
        file_id: usize,
    },
    #[error("can't take a mutable borrow to this value because it's not declared mutable")]
    CantTakeMutableBorrow {
        span: Span,
        declare_span: Option<Span>,
        file_id: usize,
    },
    #[error("this method requires a mutable 'self'")]
    NotMutableSelf {
        span: Span,
        path_span: Span,
        file_id: usize,
    },
}
