use std::{
    borrow::Cow,
    fmt,
    path::{Path, PathBuf},
    process::Child,
};

use ariadne::Source;
use edlang_codegen_llvm::linker::{link_binary, link_shared_lib};
use edlang_lowering::lower_modules;
use edlang_session::{DebugInfo, OptLevel, Session};
use tempfile::TempDir;

#[derive(Debug, Clone)]
struct TestError(Cow<'static, str>);

impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for TestError {}

#[derive(Debug)]
pub struct CompileResult {
    pub folder: TempDir,
    pub object_file: PathBuf,
    pub binary_file: PathBuf,
}

pub fn compile_program(
    source: &str,
    name: &str,
    library: bool,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let module = edlang_parser::parse_ast(source).unwrap();

    let test_dir = tempfile::tempdir()?;
    let test_dir_path = test_dir.path();
    let target_dir = test_dir_path.join("build_artifacts/");
    if !target_dir.exists() {
        std::fs::create_dir_all(&target_dir)?;
    }
    let output_file = target_dir.join(PathBuf::from(name));
    let output_file = if library {
        output_file.with_extension(Session::get_platform_library_ext())
    } else if cfg!(target_os = "windows") {
        output_file.with_extension("exe")
    } else {
        output_file.with_extension("")
    };

    let session = Session {
        file_path: PathBuf::from(name),
        debug_info: DebugInfo::Full,
        optlevel: OptLevel::Default,
        source: Source::from(source.to_string()),
        library,
        target_dir,
        output_file,
        output_llvm: false,
        output_asm: false,
    };

    let program_ir = lower_modules(&[module])?;

    let object_path = edlang_codegen_llvm::compile(&session, &program_ir)?;

    if library {
        link_shared_lib(&object_path, &session.output_file)?;
    } else {
        link_binary(&object_path, &session.output_file)?;
    }

    Ok(CompileResult {
        folder: test_dir,
        object_file: object_path,
        binary_file: session.output_file,
    })
}

pub fn run_program(program: &Path, args: &[&str]) -> Result<Child, std::io::Error> {
    std::process::Command::new(dbg!(program)).args(args).spawn()
}
