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

    #[regex(r"_?\p{XID_Start}\p{XID_Continue}*", |lex| lex.slice().parse().ok())]
    Identifier(String),
    #[regex(r"\d+", |lex| lex.slice().parse().ok())]
    Integer(String),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,
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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
