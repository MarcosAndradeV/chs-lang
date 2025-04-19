#![allow(unused)]
use std::{
    env::Args,
    fs::{self, File},
    io::{Empty, Write},
    os::unix::process::CommandExt,
    path::PathBuf,
    process::{self, Command, ExitCode, Stdio, exit},
};

use chsc::{
    chs_ast::{
        self, RawModule, flow_checker::FlowChecker, hir::HIRModule, mir::MIRModule, parser::Parser,
        typechecker::TypeChecker,
    },
    chs_codegen::qbe_backend::QBEBackend,
    chs_lexer,
    chs_util::{CHSError, CHSResult},
    my_cli::{Cmd, MyCLI},
};

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

fn compile(
    file_path: String,
    outpath: Option<String>,
    run: bool,
    silent: bool,
    emit_asm: bool,
) -> CHSResult<()> {
    println!("[INFO] Reading module from file: {}", file_path);
    let raw_module = RawModule::new(chs_ast::read_flie(&file_path), file_path);

    println!("[INFO] Parsing module...");
    let module = Parser::new(&raw_module).parse()?;

    println!("[INFO] Converting to HIR...");
    let module = HIRModule::from_ast(module);

    println!("[INFO] Running type checker...");
    let mut checker = TypeChecker::new(module.raw_module);
    checker.check_module(&module)?;

    println!("[INFO] Converting to MIR...");
    let tenv = checker.env();
    let module = MIRModule::from_hir(module, tenv);

    println!("[INFO] Running flow checker...");
    let mut checker = FlowChecker::new(&module);
    checker.check_module().map_err(|errors| {
        let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();

        CHSError(format!(
            "Control flow errors detected:\n{}",
            error_messages.join("\n")
        ))
    })?;

    println!("[INFO] Generating code using QBE backend...");
    let mut backend = QBEBackend::new(&raw_module);
    backend.generate_module(module);
    let module = backend.finish();

    let file_path = PathBuf::from(&raw_module.file_path);
    let ssa_path = file_path.with_extension("ssa");
    let asm_path = file_path.with_extension("s");
    let out_path = file_path.with_extension("");

    fs::write(&ssa_path, module.to_string()).unwrap();

    println!(
        "[INFO] CMD: qbe -o {} {}",
        asm_path.display(),
        ssa_path.display()
    );
    Command::new("qbe")
        .arg("-o")
        .arg(&asm_path)
        .arg(&ssa_path)
        .status()
        .unwrap();
    println!(
        "[INFO] CMD: cc -o {} {}",
        out_path.display(),
        asm_path.display()
    );
    Command::new("cc")
        .arg("-o")
        .arg(&out_path)
        .arg(&asm_path)
        .status()
        .unwrap();

    println!("[INFO] Compilation completed successfully!");
    Ok(())
}
