use std::path::PathBuf;

use chsc::{
    chs_ast::{self, RawModule, hir::HIRModule, parser::Parser, typechecker::TypeChecker},
    chs_error,
    chs_mir::MIRModule,
    chs_util::{CHSError, CHSResult, binary_exists},
    cli,
    config::Config,
};
use clap::Parser as _;

fn main() {
    let _chs_config = Config::default();

    if !binary_exists("cc") {
        eprintln!("[ERROR] cc binary not found.");
        std::process::exit(1);
    }

    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Compile {
            input,
            output,
            compiler_flags,
            silent,
            keep,
        } => {
            if !silent {
                println!("[INFO] Compiling file: {}", input);
                if let Some(ref out) = output {
                    println!("[INFO] Output: {}", out);
                }
            }
            let result = compile(input, output, compiler_flags, false, silent, keep);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }

        cli::Commands::CompileRun {
            input,
            output,
            compiler_flags,
        } => {
            let result = compile(input, output, compiler_flags, true, true, false);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }
    }
}

macro_rules! log {
    ($silent:expr, $($arg:tt)*) => {
        if !$silent {
            println!($($arg)*);
        }
    }
}

#[allow(unused)]
fn compile(
    input_path: String,
    outpath: Option<String>,
    compiler_flags: Vec<String>,
    run: bool,
    silent: bool,
    keep: bool,
) -> CHSResult<()> {
    let file_path = PathBuf::from(&input_path);
    let out_path = outpath
        .map(PathBuf::from)
        .unwrap_or_else(|| file_path.with_extension(""));

    log!(silent, "[INFO] Reading module from file: {}", input_path);
    let raw_module = RawModule::new(chs_ast::read_file(&input_path), input_path);
    let mut lexer = chslexer::PeekableLexer::new(&raw_module.source);

    log!(silent, "[INFO] Parsing module...");

    let module = Parser::new(&mut lexer)
        .parse()
        .map_err(|err| chs_error!("{}", err))?;

    log!(silent, "[INFO] Converting to HIR...");
    let mut module = HIRModule::from_ast(module);

    log!(silent, "[INFO] Running type checker...");
    let mut checker = TypeChecker::new(&raw_module);
    checker.check_module(&mut module)?;
    let module = MIRModule::from_hir(module);

    log!(silent, "[INFO] Code generation is not implemented...");
    println!("{}", module.print());
    todo!();

    Ok(())
}
