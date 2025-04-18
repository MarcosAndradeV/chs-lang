#![allow(unused)]
mod my_cli;
use std::{
    env::Args,
    fs::File,
    io::{Empty, Write},
    os::unix::process::CommandExt,
    path::PathBuf,
    process::{self, exit, ExitCode, Stdio},
};

use chs_ast::{
    flow_checker::FlowChecker, hir::HIRModule, mir::MIRModule, parser::Parser, typechecker::TypeChecker, RawModule
};
use chs_util::{chs_error, return_chs_error, CHSError, CHSResult};

use my_cli::{Cmd, MyCLI};

fn main() {
    let cl = MyCLI::create_from_args()
        .add_cmd("help", Cmd::new().help("Print help message"))
        .add_cmd(
            "compile",
            Cmd::new()
                .help("Compiles the program.")
                .arg("INPUT", 0)
                .flag("o", "OUTPUT", false)
                .flag("l", "LIB", false)
                .flag_bool("r")
                .flag_bool("s")
                .flag_bool("emit-asm"),
        );
    match cl.get_matches() {
        Some(("help", ..)) => cl.usage(),
        Some(("compile", flags, args)) => {
            if let Some(file_path) = args.get(0).cloned() {
                if let Err(err) = compile(
                    file_path,
                    flags.get("o").cloned(),
                    flags.is_present("r"),
                    flags.is_present("s"),
                    flags.is_present("emit-asm"),
                ) {
                    eprintln!("{err}");
                }
            } else {
                eprintln!("No file provided")
            }
        }
        _ => cl.usage(),
    }
}

#[allow(dead_code)]
fn compile(
    file_path: String,
    outpath: Option<String>,
    run: bool,
    silent: bool,
    emit_asm: bool,
) -> CHSResult<()> {
    let raw_module = RawModule::new(chs_ast::read_flie(&file_path), file_path);

    let module = Parser::new(&raw_module).parse()?;

    let module = HIRModule::from_ast(module);

    let mut checker = TypeChecker::new(module.raw_module);
    checker.check_module(&module)?;

    let tenv = checker.env();
    let module = MIRModule::from_hir(module, tenv);

    let mut checker = FlowChecker::new(&module);
    checker.check_module().map_err(|errors| {
        // Combine all flow errors into a single CHSError with a formatted message
        let error_messages: Vec<String> = errors.iter()
            .map(|e| e.to_string())
            .collect();
        
        CHSError(format!(
            "Control flow errors detected:\n{}",
            error_messages.join("\n")
        ))
    })?;

    dbg!(module.items);

    todo!("Finalize compilation");

    Ok(())
}