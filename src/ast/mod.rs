pub mod function;
pub mod span;
pub mod statement;

pub use function::*;
pub use span::*;
pub use statement::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl OpCode {
    pub fn to_str(&self) -> &'static str {
        match self {
            OpCode::Add => "addi",
            OpCode::Sub => "subi",
            OpCode::Mul => "muli",
            OpCode::Div => "divi",
            OpCode::Rem => "remi",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub SpanValue<String>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Number(pub SpanValue<i64>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Identifier(Identifier),
    Number(Number),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Term(Term),
    Call(Identifier, Vec<SpanValue<Box<Self>>>),
    Op(SpanValue<Box<Self>>, OpCode, SpanValue<Box<Self>>),
}

impl Expr {
    pub fn new_ident(value: Identifier) -> Self {
        Self::Term(Term::Identifier(value))
    }

    pub fn new_number(value: Number) -> Self {
        Self::Term(Term::Number(value))
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<SpanValue<Statement>>,
}

impl Program {
    pub fn new(statements: Vec<SpanValue<Statement>>) -> Self {
        Self { statements }
    }
}
