use std::{ops::Range, path::Path};

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use error::Error;
use lalrpop_util::ParseError;
use lexer::{Lexer, LexicalError};

pub mod error;
pub mod lexer;
pub mod tokens;

pub mod grammar {
    #![allow(dead_code, unused_imports, unused_variables)]

    pub use self::grammar::*;
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub grammar);
}

pub fn parse_ast(source: &str) {
    let lexer = Lexer::new(source);
    let parser = grammar::IdentParser::new();
}

pub fn print_error(path: &str, source: &str, error: &Error) -> Result<(), std::io::Error> {
    let source = Source::from(source);
    match error {
        ParseError::InvalidToken { location } => {
            let loc = *location;
            Report::build(ReportKind::Error, path, loc)
                .with_code(1)
                .with_message("Invalid token")
                .with_label(Label::new((path, loc..(loc + 1))).with_message("invalid token"))
                .finish()
                .eprint((path, source))?;
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let loc = *location;
            Report::build(ReportKind::Error, path, loc)
                .with_code(2)
                .with_message("Unrecognized end of file")
                .with_label(Label::new((path, loc..(loc + 1))).with_message(format!(
                    "unrecognized eof, expected one of the following: {:?}",
                    expected
                )))
                .finish()
                .eprint((path, source))?;
        }
        ParseError::UnrecognizedToken { token, expected } => {
            Report::build(ReportKind::Error, path, token.0)
                .with_code(3)
                .with_message("Unrecognized token")
                .with_label(Label::new((path, token.0..token.2)).with_message(format!(
                    "unrecognized token {:?}, expected one of the following: {:?}",
                    token.1, expected
                )))
                .finish()
                .eprint((path, source))?;
        }
        ParseError::ExtraToken { token } => {
            Report::build(ReportKind::Error, path, token.0)
                .with_code(4)
                .with_message("Extra token")
                .with_label(
                    Label::new((path, token.0..token.2))
                        .with_message(format!("unexpected extra token {:?}", token.1)),
                )
                .finish()
                .eprint((path, source))?;
        }
        ParseError::User { error } => match error {
            LexicalError::InvalidToken(err, range) => match err {
                tokens::LexingError::NumberParseError => {
                    Report::build(ReportKind::Error, path, range.start)
                        .with_code(4)
                        .with_message("Error parsing literal number")
                        .with_label(
                            Label::new((path, range.start..range.end))
                                .with_message("error parsing literal number"),
                        )
                        .finish()
                        .eprint((path, source))?;
                }
                tokens::LexingError::Other => {
                    Report::build(ReportKind::Error, path, range.start)
                        .with_code(4)
                        .with_message("Other error")
                        .with_label(
                            Label::new((path, range.start..range.end)).with_message("other error"),
                        )
                        .finish()
                        .eprint((path, source))?;
                }
            },
        },
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::parse_ast;

    #[test]
    fn test_ident() {
        parse_ast("hello");
    }
}
