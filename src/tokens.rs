use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?")]
pub enum Token {
    #[token("var")]
    KeywordVar,
    #[token("print")]
    KeywordPrint,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().parse().ok())]
    Identifier(String),
    #[regex(r"\d+", |lex| lex.slice().parse().ok())]
    Integer(i64),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,

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
