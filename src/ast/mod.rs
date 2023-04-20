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
pub enum LiteralValue {
    String,
    Integer {
        bits: usize,
        signed: bool
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Literal(LiteralValue),
    Variable(String),
    Call {
        function: String,
        args: Vec<Box<Self>>
    },
    BinaryOp(Box<Self>, OpCode, Box<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub ident: String,
    pub type_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub body: Vec<Statement>,
    pub return_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Variable {
        name: String,
        value: Box<Expression>
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
