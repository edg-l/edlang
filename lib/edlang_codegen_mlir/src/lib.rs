#![allow(clippy::too_many_arguments)]

use edlang_ir as ir;
use std::path::PathBuf;

use edlang_session::Session;
use ir::ProgramBody;

pub mod codegen;
pub mod linker;

pub fn compile(
    session: &Session,
    program: &ProgramBody,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    codegen::compile(session, program)
}
