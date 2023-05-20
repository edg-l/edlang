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
pub enum LiteralValue {
    String,
    Integer {
        bits: Option<u32>,
        signed: bool,
        value: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Literal(LiteralValue),
    Variable(String),
    Call {
        function: String,
        args: Vec<Box<Self>>,
    },
    BinaryOp(Box<Self>, OpCode, Box<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub ident: String,
    pub type_name: String,
}

impl Parameter {
    pub const fn new(ident: String, type_name: String) -> Self {
        Self { ident, type_name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub body: Vec<Statement>,
    pub return_type: Option<String>,
}

impl Function {
    pub const fn new(
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
        return_type: Option<String>,
    ) -> Self {
        Self {
            name,
            params,
            body,
            return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Let {
        name: String,
        value: Box<Expression>,
        type_name: Option<String>,
    },
    Mutate {
        name: String,
        value: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    Return(Option<Box<Expression>>),
    Function(Function),
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
