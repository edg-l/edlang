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
    Integer {
        bits: u32,
        signed: bool,
    },
    Boolean,
    Array {
        of: Spanned<Box<Self>>,
        len: Option<u32>,
    },
    Pointer {
        target: Spanned<Box<Self>>,
    },
    Other {
        id: String,
    },
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
        name: String,
    },
    Call {
        function: Spanned<String>,
        args: Vec<Spanned<Box<Self>>>,
    },
    BinaryOp(Spanned<Box<Self>>, OpCode, Spanned<Box<Self>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub ident: Spanned<String>,
    pub type_exp: Spanned<TypeExp>,
}

impl Parameter {
    pub const fn new(ident: Spanned<String>, type_exp: Spanned<TypeExp>) -> Self {
        Self { ident, type_exp }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Spanned<String>,
    pub params: Vec<Parameter>,
    pub body: Vec<Spanned<Statement>>,
    pub scope_type_info: HashMap<String, Vec<TypeExp>>,
    pub return_type: Option<Spanned<TypeExp>>,
}

impl Function {
    pub fn new(
        name: Spanned<String>,
        params: Vec<Parameter>,
        body: Vec<Spanned<Statement>>,
        return_type: Option<Spanned<TypeExp>>,
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
    pub ident: Spanned<String>,
    pub field_type: Spanned<TypeExp>,
}

impl StructField {
    pub const fn new(ident: Spanned<String>, type_name: Spanned<TypeExp>) -> Self {
        Self {
            ident,
            field_type: type_name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub name: Spanned<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: Spanned<String>,
        value: Spanned<Box<Expression>>,
        value_type: Option<Spanned<TypeExp>>,
    },
    Mutate {
        name: Spanned<String>,
        value: Spanned<Box<Expression>>,
    },
    If {
        condition: Spanned<Box<Expression>>,
        body: Vec<Spanned<Statement>>,
        scope_type_info: HashMap<String, Vec<TypeExp>>,
        else_body: Option<Vec<Spanned<Statement>>>,
        else_body_scope_type_info: HashMap<String, Vec<TypeExp>>,
    },
    Return(Option<Spanned<Box<Expression>>>),
    Function(Function),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Spanned<Statement>>,
}

impl Program {
    pub fn new(statements: Vec<Spanned<Statement>>) -> Self {
        Self { statements }
    }
}
