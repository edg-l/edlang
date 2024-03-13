use std::{path::PathBuf, time::Instant};

use anyhow::Result;
use ariadne::{sources, Source};
use clap::Parser;
use edlang_lowering::lower_modules;
use edlang_session::{DebugInfo, OptLevel, Session};
use walkdir::WalkDir;

use crate::linker::{link_binary, link_shared_lib};

pub mod linker;

#[derive(Parser, Debug)]
#[command(author, version, about = "edlang compiler driver", long_about = None, bin_name = "edlangc")]
pub struct CompilerArgs {
    /// The input file.
    pub input: PathBuf,

    /// The output file.
    pub output: PathBuf,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    pub release: bool,

    /// Set the optimization level, 0,1,2,3
    #[arg(short = 'O', long)]
    pub optlevel: Option<u8>,

    /// Always add debug info
    #[arg(long)]
    pub debug_info: Option<bool>,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    pub library: bool,

    /// Print the edlang AST
    #[arg(long, default_value_t = false)]
    pub ast: bool,

    /// Print the edlang IR
    #[arg(long, default_value_t = false)]
    pub ir: bool,

    /// Output llvm ir
    #[arg(long, default_value_t = false)]
    pub llvm: bool,

    /// Output asm
    #[arg(long, default_value_t = false)]
    pub asm: bool,

    /// Output a object file
    #[arg(long, default_value_t = false)]
    pub object: bool,
}

pub fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let args = CompilerArgs::parse();

    let object = compile(&args)?;

    if args.library {
        link_shared_lib(&[object.clone()], &args.output)?;
    } else {
        link_binary(&[object.clone()], &args.output)?;
    }

    if !args.object {
        std::fs::remove_file(object)?;
    }

    Ok(())
}

pub fn compile(args: &CompilerArgs) -> Result<PathBuf> {
    let mut files = Vec::new();
    for entry in WalkDir::new(&args.input).sort_by_file_name() {
        let entry = entry?;
        if let Some(ext) = entry.path().extension() {
            if ext.eq_ignore_ascii_case("ed") {
                files.push(entry.path().to_path_buf());
            }
        }
    }

    if files.is_empty() {
        panic!("files is empty");
    }

    let start_time = Instant::now();

    let mut modules = Vec::new();

    for path in files {
        let source = std::fs::read_to_string(&path)?;

        let module_ast = edlang_parser::parse_ast(
            &source,
            &path.file_stem().expect("no file stem").to_string_lossy(),
        );

        let module_temp = match module_ast {
            Ok(module) => module,
            Err(error) => {
                let path = path.display().to_string();
                let report = edlang_parser::error_to_report(&path, &error)?;
                edlang_parser::print_report(&path, &source, report)?;
                std::process::exit(1)
            }
        };
        modules.push((path, source, module_temp));
    }

    let session = Session {
        file_paths: modules.iter().map(|x| x.0.clone()).collect(),
        debug_info: if let Some(debug_info) = args.debug_info {
            if debug_info {
                DebugInfo::Full
            } else {
                DebugInfo::None
            }
        } else if args.release {
            DebugInfo::None
        } else {
            DebugInfo::Full
        },
        optlevel: if let Some(optlevel) = args.optlevel {
            match optlevel {
                0 => OptLevel::None,
                1 => OptLevel::Less,
                2 => OptLevel::Default,
                _ => OptLevel::Aggressive,
            }
        } else if args.release {
            OptLevel::Aggressive
        } else {
            OptLevel::None
        },
        sources: modules.iter().map(|x| Source::from(x.1.clone())).collect(),
        library: args.library,
        output_file: args.output.with_extension("o"),
        output_asm: args.asm,
        output_llvm: args.llvm,
    };
    tracing::debug!("Output file: {:#?}", session.output_file);
    tracing::debug!("Is library: {:#?}", session.library);
    tracing::debug!("Optlevel: {:#?}", session.optlevel);
    tracing::debug!("Debug Info: {:#?}", session.debug_info);

    let path_cache: Vec<_> = modules
        .iter()
        .map(|x| (x.0.display().to_string(), x.1.clone()))
        .collect();
    let modules: Vec<_> = modules.iter().map(|x| x.2.clone()).collect();

    if args.ast {
        std::fs::write(
            session.output_file.with_extension("ast"),
            format!("{:#?}", modules),
        )?;
    }

    let program_ir = match lower_modules(&modules) {
        Ok(ir) => ir,
        Err(error) => {
            let report = edlang_check::lowering_error_to_report(error, &session);
            report.eprint(sources(path_cache))?;
            std::process::exit(1);
        }
    };

    if args.ir {
        std::fs::write(
            session.output_file.with_extension("ir"),
            format!("{:#?}", program_ir),
        )?;
    }

    let object_path = edlang_codegen_llvm::compile(&session, &program_ir).unwrap();

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(object_path)
}
