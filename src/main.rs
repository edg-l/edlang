#![allow(clippy::too_many_arguments)]

use check::print_error;
use clap::{Parser, Subcommand};
use codegen::ProgramData;
use color_eyre::Result;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use lalrpop_util::lalrpop_mod;
use std::{fs, path::PathBuf, println};

use crate::{ast::Program, check::Check, lexer::Lexer};

pub mod ast;
pub mod check;
pub mod codegen;
pub mod lexer;
pub mod tokens;
pub mod type_analysis;

lalrpop_mod!(pub grammar);

#[derive(Parser)]
#[command(
    author,
    version,
    about,
    long_about = r#"A experimental language using LLVM."#
)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Check if the code is valid.
    Check {
        /// The input file.
        input: PathBuf,
    },
    /// Prints the code AST.
    Ast {
        /// The input file.
        input: PathBuf,
    },
    /// Compile the edlang source file.
    Compile {
        /// The input file.
        input: PathBuf,

        /// Output optimized llvm ir.
        #[arg(long)]
        optimize: bool,

        /// The output file. If not specified its output will be stdout.
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Turn on debug info.
        #[arg(short, long)]
        debug: bool,
    },
    /// Compile and run a program. Main needs to return nothing.
    Run {
        /// The input file.
        input: PathBuf,
    },
}

fn check_program(program: &ProgramData, ast: &ast::Program) -> bool {
    let errors = check::check(program, ast);

    let mut error_count = 0;
    let mut warn_count = 0;
    for error in errors {
        match error {
            Check::Warning(x) => {
                warn_count += 1;
                println!("{}", x)
            }
            Check::Error(x) => {
                error_count += 1;
                println!("{}", x)
            }
        }
    }

    println!("Warnings: {warn_count}");
    println!("Errors: {error_count}");

    error_count == 0
}

fn main() -> Result<()> {
    color_eyre::install()?;
    tracing_subscriber::fmt::init();
    let args = Args::parse();

    match args.command {
        Commands::Check { input } => {
            let code = fs::read_to_string(&input)?;
            let lexer = Lexer::new(code.as_str());
            let parser = grammar::ProgramParser::new();
            let mut ast = parser.parse(lexer)?;
            type_analysis::type_inference(&mut ast);
            let program = ProgramData::new(&input, &code);
            check_program(&program, &ast);
        }
        Commands::Ast { input } => {
            let code = fs::read_to_string(input)?;
            let lexer = Lexer::new(code.as_str());
            let parser = grammar::ProgramParser::new();
            match parser.parse(lexer) {
                Ok(mut ast) => {
                    type_analysis::type_inference(&mut ast);
                    println!("{ast:#?}");
                }
                Err(e) => {
                    print_error(&code, e);
                }
            }
        }
        Commands::Compile {
            input,
            output,
            debug: _,
            optimize: _,
        } => {
            let code = fs::read_to_string(&input)?;
            let lexer = Lexer::new(code.as_str());
            let parser = grammar::ProgramParser::new();
            let mut ast: Program = parser.parse(lexer)?;
            type_analysis::type_inference(&mut ast);

            let program = ProgramData::new(&input, &code);

            let file_name = input.file_name().unwrap().to_string_lossy();

            if !check_program(&program, &ast) {
                return Ok(());
            }

            // println!("{:#?}", ast);
            let context = Context::create();
            let mut codegen = codegen::CodeGen::new(&context, &file_name, program, ast)?;
            codegen.compile_ast()?;
            let generated_llvm_ir = codegen.generated_code();

            if let Some(output) = output {
                fs::write(output, generated_llvm_ir)?;
            } else {
                println!("{generated_llvm_ir}");
            }
        }
        Commands::Run { input } => {
            let code = fs::read_to_string(&input)?;
            let lexer = Lexer::new(&code[..]);
            let parser = grammar::ProgramParser::new();
            let ast = parser.parse(lexer).unwrap();

            let program = ProgramData::new(&input, &code);

            let file_name = input.file_name().unwrap().to_string_lossy();

            let context = Context::create();
            let mut codegen = codegen::CodeGen::new(&context, &file_name, program, ast)?;
            codegen.compile_ast()?;
            let execution_engine = codegen
                .module
                .create_jit_execution_engine(OptimizationLevel::Aggressive)
                .unwrap();

            unsafe {
                let main: JitFunction<unsafe extern "C" fn() -> ()> =
                    execution_engine.get_function("main")?;
                main.call();
            };
        }
    }

    Ok(())
}
