use std::{error::Error, path::PathBuf, time::Instant};

use ariadne::Source;
use clap::Parser;
use edlang_codegen_llvm::linker::{link_binary, link_shared_lib};
use edlang_lowering::lower_modules;
use edlang_session::{DebugInfo, OptLevel, Session};

#[derive(Parser, Debug)]
#[command(author, version, about = "edlang compiler driver", long_about = None, bin_name = "edlang")]
pub struct CompilerArgs {
    /// The input file.
    input: PathBuf,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    release: bool,

    /// Set the optimization level, 0,1,2,3
    #[arg(short = 'O', long)]
    optlevel: Option<u8>,

    /// Always add debug info
    #[arg(long)]
    debug_info: Option<bool>,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    library: bool,

    /// Print the edlang AST
    #[arg(long, default_value_t = false)]
    ast: bool,

    /// Print the edlang IR
    #[arg(long, default_value_t = false)]
    ir: bool,

    /// Output llvm ir
    #[arg(long, default_value_t = false)]
    llvm: bool,

    /// Output asm
    #[arg(long, default_value_t = false)]
    asm: bool,
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let start_time = Instant::now();

    tracing_subscriber::fmt::init();

    let args = CompilerArgs::parse();

    let path = args.input.display().to_string();
    let source = std::fs::read_to_string(&args.input)?;

    let module = edlang_parser::parse_ast(&source);

    let module = match module {
        Ok(module) => module,
        Err(error) => {
            let report = edlang_parser::error_to_report(&path, &error)?;
            edlang_parser::print_report(&path, &source, report)?;
            std::process::exit(1)
        }
    };

    let cwd = std::env::current_dir()?;
    // todo: find a better name, "target" would clash with rust if running in the source tree.
    let target_dir = cwd.join("target_ed/");
    if !target_dir.exists() {
        std::fs::create_dir_all(&target_dir)?;
    }
    let output_file = target_dir.join(PathBuf::from(args.input.file_name().unwrap()));
    let output_file = if args.library {
        output_file.with_extension(Session::get_platform_library_ext())
    } else if cfg!(target_os = "windows") {
        output_file.with_extension("exe")
    } else {
        output_file.with_extension("")
    };

    let session = Session {
        file_path: args.input,
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
        source: Source::from(source),
        library: args.library,
        target_dir,
        output_file,
        output_asm: args.asm,
        output_llvm: args.llvm,
    };
    tracing::debug!("Input file: {:#?}", session.file_path);
    tracing::debug!("Target dir: {:#?}", session.target_dir);
    tracing::debug!("Output file: {:#?}", session.output_file);
    tracing::debug!("Is library: {:#?}", session.library);
    tracing::debug!("Optlevel: {:#?}", session.optlevel);
    tracing::debug!("Debug Info: {:#?}", session.debug_info);

    if args.ast {
        println!("{:#?}", module);
        return Ok(());
    }

    let program_ir = match lower_modules(&[module.clone()]) {
        Ok(ir) => ir,
        Err(error) => {
            let report = edlang_check::lowering_error_to_report(error, &session);
            let path = session.file_path.display().to_string();
            report.eprint((path, session.source.clone()))?;
            std::process::exit(1);
        }
    };

    if args.ir {
        println!("{:#?}", program_ir);
        return Ok(());
    }

    let object_path = edlang_codegen_llvm::compile(&session, &program_ir)?;

    if session.library {
        link_shared_lib(&object_path, &session.output_file)?;
    } else {
        link_binary(&object_path, &session.output_file)?;
    }

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(())
}
