use crate::{lexer::LexicalError, tokens::Token};
use lalrpop_util::ParseError;

pub type Error = ParseError<usize, Token, LexicalError>;
