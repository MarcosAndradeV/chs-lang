use std::{env::Args, fs::File, io::Write, os::unix::process::CommandExt, process::{self, exit, ExitCode}};

use chs_ast::nodes::TypedModule;
use chs_codegen::FasmGenerator;
use chs_util::{chs_error, CHSError, CHSResult};

fn help(program: &str, _: &mut Args) -> CHSResult<()> {
    usage(program);
    Ok(())
}

fn compile(program: &str, args: &mut Args) -> CHSResult<()> {
    let file_path = args.next().expect("Expected file path for compile command.");

    let module = match chs_ast::parse_file(file_path) {
        Ok(module) => module,
        Err(err) => return Err(err),
    };

    let typed_module = match TypedModule::from_module(module) {
        Ok(typed_module) => typed_module,
        Err(err) => return Err(err),
    };

    let fasm_code = match FasmGenerator::generate(typed_module) {
        Ok(code) => code,
        Err(err) => return Err(err),
    };

    let output_path = fasm_code.out_path();
    let mut out_file = File::create(output_path).map_err(|err| CHSError(err.to_string()))?;
    write!(out_file, "{}", fasm_code).map_err(|err| CHSError(err.to_string()))?;
    println!("[INFO] Generating {}", output_path.display());

    let mut fasm_process = process::Command::new("fasm")
        .arg(output_path)
        .arg(output_path.with_extension(""))
        .spawn()
        .expect("Failed to spawn fasm process");

    let fasm_result = fasm_process.wait_with_output().expect("Failed to wait for fasm");
    if !fasm_result.status.success() {
        chs_error!(String::from_utf8_lossy(&fasm_result.stderr));
    }

    Ok(())
}

pub const COMMANDS: &[Command] = &[
    Command {
        name: "help",
        descripition: "Print this message",
        run: help,
    },
    Command {
        name: "compile",
        descripition: "Compile a program: chs compile <file.chs>",
        run: compile,
    },
];

pub struct Command {
    pub name: &'static str,
    pub descripition: &'static str,
    pub run: fn(&str, &mut Args) -> CHSResult<()>,
}

pub fn usage(program: &str) {
    println!("USAGE: {program} <COMMAND> [OPTIONS]");
    println!("COMMANDS:");

    for ele in COMMANDS.iter() {
        println!(
            "      {name: <7}      {descripition}",
            name = ele.name,
            descripition = ele.descripition,
        );
    }
}
