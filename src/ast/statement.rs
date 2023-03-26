use super::{Expr, Function, Identifier, SpanValue};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementBody {
    pub ident: Identifier,
    pub expr: SpanValue<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Assignment(StatementBody),
    Definition(StatementBody),
    Return(Option<SpanValue<Box<Expr>>>),
    Function(Function),
}

impl Statement {
    pub fn new_assignment(ident: Identifier, expr: SpanValue<Box<Expr>>) -> Self {
        Self::Assignment(StatementBody { ident, expr })
    }

    pub fn new_definition(ident: Identifier, expr: SpanValue<Box<Expr>>) -> Self {
        Self::Definition(StatementBody { ident, expr })
    }

    pub fn new_function(func: Function) -> Self {
        Self::Function(func)
    }
}
