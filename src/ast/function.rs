use super::{Identifier, SpanValue, Statement};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub ident: Identifier,
    pub type_name: Identifier,
}

impl Parameter {
    pub fn new(ident: Identifier, type_name: Identifier) -> Self {
        Self { ident, type_name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub ident: Identifier,
    pub params: Vec<Parameter>,
    pub body: Vec<SpanValue<Statement>>,
    pub return_type: Option<Identifier>,
}

impl Function {
    pub fn new(
        ident: Identifier,
        params: Vec<Parameter>,
        body: Vec<SpanValue<Statement>>,
        return_type: Option<Identifier>,
    ) -> Self {
        Self {
            ident,
            params,
            body,
            return_type,
        }
    }
}
