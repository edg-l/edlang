use logos::Logos;
use std::fmt;

// todo: https://github.com/maciejhirsz/logos/issues/133#issuecomment-619444615
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?")]
pub enum Token {
    #[token("let")]
    KeywordLet,
    #[token("print")]
    KeywordPrint,
    #[token("fn")]
    KeywordFn,
    #[token("return")]
    KeywordReturn,
    #[token("struct")]
    KeywordStruct,
    #[token("ptr")]
    KeywordPtr,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,

    #[regex(r"_?\p{XID_Start}\p{XID_Continue}*", |lex| lex.slice().parse().ok())]
    Identifier(String),
    #[regex(r"\d+", |lex| lex.slice().parse().ok())]
    Integer(String),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice().to_string())]
    String(String),
    #[regex(r"(true|false)", |lex| lex.slice().parse().ok())]
    Boolean(bool),

    #[token("bool")]
    KeywordBool,
    #[token("i8")]
    Inti8,
    #[token("i16")]
    Inti16,
    #[token("i32")]
    Inti32,
    #[token("i64")]
    Inti64,
    #[token("u8")]
    Intu8,
    #[token("u16")]
    Intu16,
    #[token("u32")]
    Intu32,
    #[token("u64")]
    Intu64,

    #[token("f32")]
    Float32,
    #[token("f64")]
    Float64,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,
    #[token("[")]
    LeftSquareBracket,
    #[token("]")]
    RightSquareBracket,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("->")]
    Arrow,
    #[token(",")]
    Coma,
    #[token("<")]
    LessThanSign,
    #[token(">")]
    MoreThanSign,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("%")]
    OperatorRem,
    #[token("&&")]
    OperatorAnd,
    #[token("||")]
    OperatorOr,
    #[token("==")]
    OperatorEq,
    #[token("!=")]
    OperatorNe,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
