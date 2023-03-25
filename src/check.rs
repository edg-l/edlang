use crate::{
    ast::{self, Statement},
    codegen::ProgramData,
};
use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

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
            Statement::Assignment(_x) => {
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
                        origin: Some(&data.filename),
                        annotations: vec![
                            SourceAnnotation {
                                label: "unexpected statement",
                                annotation_type: AnnotationType::Error,
                                range: statement.span.into(),
                            },
                        ],
                    }],
                    opt: FormatOptions {
                        color: true,
                        ..Default::default()
                    },
                };

                let dl = DisplayList::from(snippet);
                errors.push(Check::Error(dl));
            }
            Statement::Definition(_) => todo!(),
            Statement::Return(_) => unreachable!(),
            Statement::Function(_function) => {}
        }
    }

    errors
}
