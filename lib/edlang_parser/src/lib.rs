use std::ops::Range;

use crate::error::Error;
use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use itertools::Itertools;
use lalrpop_util::ParseError;
use lexer::{Lexer, LexicalError};
use tokens::Token;

pub mod error;
pub mod lexer;
pub mod tokens;

pub mod grammar {
    #![allow(dead_code, unused_imports, unused_variables)]

    pub use self::grammar::*;
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub grammar);
}

pub fn parse_ast(
    source: &str,
) -> Result<edlang_ast::Module, ParseError<usize, Token, LexicalError>> {
    let lexer = Lexer::new(source);
    let parser = grammar::ModuleParser::new();
    parser.parse(lexer)
}

pub fn print_report<'a>(
    path: &'a str,
    source: &'a str,
    report: Report<'static, (&'a str, Range<usize>)>,
) -> Result<(), std::io::Error> {
    let source = Source::from(source);
    report.eprint((path, source))
}

pub fn error_to_report<'a>(
    path: &'a str,
    error: &Error,
) -> Result<Report<'static, (&'a str, Range<usize>)>, std::io::Error> {
    let mut colors = ColorGenerator::new();
    let report = match error {
        ParseError::InvalidToken { location } => {
            let loc = *location;
            Report::build(ReportKind::Error, path, loc)
                .with_code("P1")
                .with_label(
                    Label::new((path, loc..(loc + 1)))
                        .with_color(colors.next())
                        .with_message("invalid token"),
                )
                .finish()
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let loc = *location;
            Report::build(ReportKind::Error, path, loc)
                .with_code("P2")
                .with_label(
                    Label::new((path, loc..(loc + 1)))
                        .with_message(format!(
                            "unrecognized eof, expected one of the following: {}",
                            expected.iter().join(", ")
                        ))
                        .with_color(colors.next()),
                )
                .finish()
        }
        ParseError::UnrecognizedToken { token, expected } => {
            Report::build(ReportKind::Error, path, token.0)
                .with_code(3)
                .with_label(
                    Label::new((path, token.0..token.2))
                        .with_message(format!(
                            "unrecognized token {:?}, expected one of the following: {}",
                            token.1,
                            expected.iter().join(", ")
                        ))
                        .with_color(colors.next()),
                )
                .finish()
        }
        ParseError::ExtraToken { token } => Report::build(ReportKind::Error, path, token.0)
            .with_code("P3")
            .with_message("Extra token")
            .with_label(
                Label::new((path, token.0..token.2))
                    .with_message(format!("unexpected extra token {:?}", token.1)),
            )
            .finish(),
        ParseError::User { error } => match error {
            LexicalError::InvalidToken(err, range) => match err {
                tokens::LexingError::NumberParseError => {
                    Report::build(ReportKind::Error, path, range.start)
                        .with_code(4)
                        .with_message("Error parsing literal number")
                        .with_label(
                            Label::new((path, range.start..range.end))
                                .with_message("error parsing literal number")
                                .with_color(colors.next()),
                        )
                        .finish()
                }
                tokens::LexingError::Other => Report::build(ReportKind::Error, path, range.start)
                    .with_code(4)
                    .with_message("Other error")
                    .with_label(
                        Label::new((path, range.start..range.end))
                            .with_message("other error")
                            .with_color(colors.next()),
                    )
                    .finish(),
            },
        },
    };

    Ok(report)
}

#[cfg(test)]
mod test {
}
