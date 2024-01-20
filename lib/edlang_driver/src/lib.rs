use std::{error::Error, path::PathBuf, time::Instant};

use ariadne::Source;
use clap::Parser;
use edlang_codegen_mlir::linker::{link_binary, link_shared_lib};
use edlang_session::{DebugInfo, OptLevel, Session};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CompilerArgs {
    /// The input file.
    input: PathBuf,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    release: bool,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    library: bool,

    #[arg(long, default_value_t = false)]
    mlir: bool,

    #[arg(long, default_value_t = false)]
    ast: bool,
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
    let output_file = target_dir.join(PathBuf::from(args.input.file_name().unwrap()));
    let output_file = if args.library {
        output_file.with_extension("so")
    } else {
        output_file.with_extension("")
    };

    let session = Session {
        file_path: args.input,
        debug_info: if args.release {
            DebugInfo::None
        } else {
            DebugInfo::Full
        },
        optlevel: if args.release {
            OptLevel::Aggressive
        } else {
            OptLevel::None
        },
        source: Source::from(source),
        library: args.library,
        target_dir,
        output_file,
    };
    tracing::debug!("Compiling with session: {:#?}", session);

    if args.ast {
        println!("{:#?}", module);
        return Ok(());
    }

    if args.mlir {
        println!("{}", edlang_codegen_mlir::compile_mlir(&session, &module)?);
        return Ok(());
    }

    let object_path = edlang_codegen_mlir::compile(&session, &module)?;

    if session.library {
        link_shared_lib(&object_path, &session.output_file.with_extension("so"))?;
    } else {
        link_binary(&object_path, &session.output_file.with_extension(""))?;
    }

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(())
}
