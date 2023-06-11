use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub span: (usize, usize),
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: (usize, usize)) -> Self {
        Self { value, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Eq,
    Ne,
}

impl OpCode {
    pub fn to_str(&self) -> &'static str {
        match self {
            OpCode::Add => "addi",
            OpCode::Sub => "subi",
            OpCode::Mul => "muli",
            OpCode::Div => "divi",
            OpCode::Rem => "remi",
            OpCode::And => "and",
            OpCode::Or => "or",
            OpCode::Eq => "eq",
            OpCode::Ne => "ne",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeExp {
    Integer { bits: u32, signed: bool },
    Boolean,
    Array { of: Box<Self>, len: Option<u32> },
    Pointer { target: Box<Self> },
    Other { id: String },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralValue {
    String(String),
    Integer {
        value: String,
        bits: u32,
        signed: bool,
    },
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Literal(LiteralValue),
    Variable {
        name: Spanned<String>,
    },
    Call {
        function: String,
        args: Vec<Box<Self>>,
    },
    BinaryOp(Box<Self>, OpCode, Box<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub ident: String,
    pub type_exp: TypeExp,
}

impl Parameter {
    pub const fn new(ident: String, type_exp: TypeExp) -> Self {
        Self { ident, type_exp }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub body: Vec<Statement>,
    pub scope_type_info: HashMap<String, Vec<TypeExp>>,
    pub return_type: Option<TypeExp>,
}

impl Function {
    pub fn new(
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
        return_type: Option<TypeExp>,
    ) -> Self {
        Self {
            name,
            params,
            body,
            return_type,
            scope_type_info: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructField {
    pub ident: String,
    pub field_type: TypeExp,
}

impl StructField {
    pub const fn new(ident: String, type_name: TypeExp) -> Self {
        Self {
            ident,
            field_type: type_name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: String,
        value: Box<Expression>,
        value_type: Option<TypeExp>,
        span: (usize, usize),
    },
    Mutate {
        name: String,
        value: Box<Expression>,
        span: (usize, usize),
    },
    If {
        condition: Box<Expression>,
        body: Vec<Statement>,
        scope_type_info: HashMap<String, Vec<TypeExp>>,
        else_body: Option<Vec<Statement>>,
        else_body_scope_type_info: HashMap<String, Vec<TypeExp>>,
    },
    Return(Option<Box<Expression>>),
    Function(Function),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}
