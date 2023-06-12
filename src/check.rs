use crate::{
    ast::{self, Statement},
    codegen::ProgramData,
    lexer::LexicalError,
    tokens::Token,
    type_analysis::TypeError,
};
use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use lalrpop_util::ParseError;

#[derive(Debug)]
pub enum Check<'a> {
    Warning(DisplayList<'a>),
    Error(DisplayList<'a>),
}

/// Checks this is a valid edlang program.
pub fn check<'a>(data: &'a ProgramData, ast: &ast::Program) -> Vec<Check<'a>> {
    let mut errors = vec![];

    for statement in &ast.statements {
        match &statement.value {
            Statement::Let { name, .. } => {
                // can't have a top level assignment yet.
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("unexpected let at top level"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source: &data.source,
                        line_start: 1,
                        fold: true,
                        origin: None,
                        annotations: vec![SourceAnnotation {
                            label: "unexpected statement",
                            annotation_type: AnnotationType::Error,
                            range: name.span,
                        }],
                    }],
                    opt: FormatOptions {
                        color: true,
                        ..Default::default()
                    },
                };

                let dl = DisplayList::from(snippet);
                errors.push(Check::Error(dl));
            }
            Statement::Mutate { name, .. } => {
                // can't have a top level assignment yet.
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("unexpected assignment at top level"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source: &data.source,
                        line_start: 1,
                        fold: true,
                        origin: None,
                        annotations: vec![SourceAnnotation {
                            label: "unexpected statement",
                            annotation_type: AnnotationType::Error,
                            range: name.span,
                        }],
                    }],
                    opt: FormatOptions {
                        color: true,
                        ..Default::default()
                    },
                };

                let dl = DisplayList::from(snippet);
                errors.push(Check::Error(dl));
            }
            _ => {}
        }
    }
    errors
}

pub fn print_error(source: &str, err: ParseError<usize, Token, LexicalError>) {
    match err {
        ParseError::InvalidToken { location } => {
            let snippet = Snippet {
                title: None,
                footer: vec![],
                slices: vec![Slice {
                    source,
                    line_start: 1,
                    fold: true,
                    origin: None,
                    annotations: vec![SourceAnnotation {
                        label: "invalid token",
                        annotation_type: AnnotationType::Error,
                        range: (location, location),
                    }],
                }],
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            };
            let dl = DisplayList::from(snippet);
            println!("{dl}");
        }
        ParseError::UnrecognizedEof {
            location: _,
            expected: _,
        } => todo!(),
        ParseError::UnrecognizedToken {
            token: _,
            expected: _,
        } => todo!(),
        ParseError::ExtraToken { token: _ } => todo!(),
        ParseError::User { error } => match error {
            LexicalError::InvalidToken(err, range) => {
                let title = format!("invalid token (lexical error): {:?}", err);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&title),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source,
                        line_start: 1,
                        fold: false,
                        origin: None,
                        annotations: vec![SourceAnnotation {
                            label: "invalid token (lexical error)",
                            annotation_type: AnnotationType::Error,
                            range: (range.start, range.end),
                        }],
                    }],
                    opt: FormatOptions {
                        color: true,
                        ..Default::default()
                    },
                };
                let dl = DisplayList::from(snippet);
                println!("{dl}");
            }
        },
    };
}

pub fn print_type_error(source: &str, err: TypeError) {
    match err {
        TypeError::Mismatch {
            found: _,
            expected: _,
            span,
        } => {
            let snippet = Snippet {
                title: Some(Annotation {
                    id: None,
                    label: Some("type mismatch"),
                    annotation_type: AnnotationType::Error,
                }),
                footer: vec![],
                slices: vec![Slice {
                    source,
                    line_start: 1,
                    fold: false,
                    origin: None,
                    annotations: vec![SourceAnnotation {
                        label: "type mismatch",
                        annotation_type: AnnotationType::Error,
                        range: span,
                    }],
                }],
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            };
            let dl = DisplayList::from(snippet);
            println!("{dl}");
        }
        TypeError::UndeclaredVariable { name: _, span } => {
            let snippet = Snippet {
                title: Some(Annotation {
                    id: None,
                    label: Some("undeclared variable"),
                    annotation_type: AnnotationType::Error,
                }),
                footer: vec![],
                slices: vec![Slice {
                    source,
                    line_start: 1,
                    fold: false,
                    origin: None,
                    annotations: vec![SourceAnnotation {
                        label: "undeclared variable",
                        annotation_type: AnnotationType::Error,
                        range: span,
                    }],
                }],
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            };
            let dl = DisplayList::from(snippet);
            println!("{dl}");
        }
    };
}
