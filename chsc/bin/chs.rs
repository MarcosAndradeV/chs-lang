#![allow(dead_code)]
#![allow(unused)]

use std::{fs, path::PathBuf, process::Command};

use chsc::{
    chs_ast::{self, RawModule, hir::HIRModule, parser::Parser, typechecker::TypeChecker},
    chs_error,
    chs_util::{CHSError, CHSResult, file_changed},
    cli,
    config::Config,
    return_chs_error,
};
use clap::Parser as _;

fn main() {
    let _chs_config = Config::default();

    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Compile {
            input,
            output,
            compiler_flags,
            silent,
        } => {
            if !silent {
                println!("[INFO] Compiling file: {}", input);
                if let Some(ref out) = output {
                    println!("[INFO] Output: {}", out);
                }
            }
            let result = compile(input, output, compiler_flags, silent);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }

        cli::Commands::CompileRun {
            input,
            output,
            compiler_flags,
            silent
        } => {
            let result = compile_run(input, output, compiler_flags, silent);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }
    }
}

fn compile_run(
    input: String,
    output: Option<String>,
    compiler_flags: Vec<String>,
    silent: bool,
) -> CHSResult<()> {
    todo!()
}

macro_rules! log {
    ($silent:expr, $($arg:tt)*) => {
        if !$silent {
            println!($($arg)*);
        }
    }
}

fn compile(
    input_path: String,
    outpath: Option<String>,
    compiler_flags: Vec<String>,
    silent: bool,
) -> CHSResult<()> {
    let file_path = PathBuf::from(&input_path);
    let out_path = outpath
        .map(PathBuf::from)
        .unwrap_or_else(|| file_path.with_extension(""));

    log!(silent, "[INFO] Reading module from file: {}", input_path);
    let raw_module = RawModule::new(chs_ast::read_file(&input_path), input_path);

    log!(silent, "[INFO] Parsing module...");
    let module = Parser::new(&raw_module).parse()?;

    log!(silent, "[INFO] Converting to HIR...");
    let mut module = HIRModule::from_ast(module);

    log!(silent, "[INFO] Running type checker...");
    let mut checker = TypeChecker::new(module.raw_module);
    checker.check_module(&mut module)?;

    return_chs_error!("Unimplemented");
}

fn cleanup_files(silent: bool, paths: &[&PathBuf]) {
    for temp_file in paths {
        if let Err(e) = fs::remove_file(temp_file) {
            log!(
                silent,
                "[WARN] Failed to remove temp file {}: {}",
                temp_file.display(),
                e
            );
        }
    }
}

fn run_exe(path: PathBuf) -> CHSResult<()> {
    println!("[INFO] Running executable...");
    let output = Command::new(&path)
        .status()
        .map_err(|e| chs_error!("Failed to execute binary: {}", e))?;

    if output.success() {
        Ok(())
    } else {
        Err(chs_error!("Execution failed"))
    }
}
