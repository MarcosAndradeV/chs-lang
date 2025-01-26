use std::{env::Args, process::ExitCode};

use chs_ast::nodes::TypedModule;
use chs_ir::FasmGenerator;

pub const COMMANDS: &[Command] = &[
    Command {
        name: "help",
        descripition: "Print this message",
        run: |program, _| {
            usage(program);
            ExitCode::SUCCESS
        },
    },
    Command {
        name: "compile",
        descripition: "Compile a program: chs compile <file.chs>",
        run: |program, args| {
            if let Some(file_path) = args.next() {
                match chs_ast::parse_file(file_path)
                    .and_then(|m| TypedModule::from_module(m))
                    .and_then(|m| FasmGenerator::generate(m))
                {
                    Ok(asm) => {
                        println!("{asm}");
                        ExitCode::SUCCESS
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        ExitCode::FAILURE
                    }
                }
            } else {
                eprintln!("Expect file path.");
                ExitCode::FAILURE
            }
        },
    },
    Command {
        name: "parse",
        descripition: "Parse a program and dump it's AST: chs parse <file.chs>",
        run: |program, args| {
            if let Some(file_path) = args.next() {
                match chs_ast::parse_file(file_path) {
                    Ok(ast) => {
                        println!("{ast:?}");
                        ExitCode::SUCCESS
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        ExitCode::FAILURE
                    }
                }
            } else {
                eprintln!("Expect file path.");
                ExitCode::FAILURE
            }
        },
    },
    Command {
        name: "check",
        descripition: "Parse a program and dump it's AST: chs check <file.chs>",
        run: |program, args| {
            if let Some(file_path) = args.next() {
                match chs_ast::parse_file(file_path).and_then(|m| TypedModule::from_module(m)) {
                    Ok(ast) => {
                        println!("{ast:?}");
                        ExitCode::SUCCESS
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        ExitCode::FAILURE
                    }
                }
            } else {
                eprintln!("Expect file path.");
                ExitCode::FAILURE
            }
        },
    },
];

pub struct Command {
    pub name: &'static str,
    pub descripition: &'static str,
    pub run: fn(&str, &mut Args) -> ExitCode,
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
