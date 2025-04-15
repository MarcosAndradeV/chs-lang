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

use chs_ast::nodes::TypedModule;
use chs_codegen::FasmGenerator;
use chs_util::{chs_error, CHSError, CHSResult};

use my_cli::{Cmd, MyCLI};

fn main() {
    let cl = MyCLI::create_from_args()
        .add_cmd("help", Cmd::new().help("Print help message"))
        .add_cmd(
            "compile",
            Cmd::new()
                .help("Compiles the program.")
                .arg("INPUT", 0)
                .flag("-o", "OUTPUT", false)
                .flag_bool("r")
                .flag_bool("s")
                .flag_bool("emit-asm"),
        )
        .add_cmd(
            "check",
            Cmd::new()
                .help("Typecheck the program.")
                .arg("INPUT", 0),
        )
        .add_cmd(
            "run",
            Cmd::new()
                .help("Run the program with the VM.")
                .arg("INPUT", 0),
        );
    match cl.get_matches() {
        Some(("help", ..)) => cl.usage(),
        Some(("compile", flags, args)) => {
            if let Some(file_path) = args.get(0).cloned() {
                if let Err(err) = compile(
                    file_path,
                    flags.get("-o").cloned(),
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
        Some(("check", _, args)) => {
            if let Some(file_path) = args.get(0).cloned() {
                if let Err(err) = check(file_path) {
                    eprintln!("{err}")
                }
            } else {
                eprintln!("No file provided")
            }
        }
        Some(("run", _, args)) => {
            if let Some(file_path) = args.get(0).cloned() {
                if let Err(err) = run(file_path) {
                    eprintln!("{err}")
                }
            } else {
                eprintln!("No file provided")
            }
        }
        _ => cl.usage(),
    }
}

fn run(file_path: String) -> CHSResult<()> {
    let module = chs_ast::parse_file(PathBuf::from(file_path))?;
    Ok(())
}

fn compile(
    file_path: String,
    outpath: Option<String>,
    run: bool,
    silent: bool,
    emit_asm: bool,
) -> CHSResult<()> {
    let module = chs_ast::parse_file(PathBuf::from(file_path))?;

    let typed_module = TypedModule::from_module(module)?;

    let fasm_code = FasmGenerator::generate(typed_module)?;

    let fasm_path = fasm_code.out_path();
    let mut out_file = File::create(fasm_path).map_err(|err| CHSError(err.to_string()))?;
    write!(out_file, "{}", fasm_code).map_err(|err| CHSError(err.to_string()))?;
    if !silent {
        println!("[INFO] Generating {}", fasm_path.display());
    }
    let output_path = match outpath {
        Some(a) => PathBuf::from(a),
        None => fasm_path.with_extension(""),
    };
    let mut fasm_proc = process::Command::new("fasm");
    fasm_proc.arg(fasm_path).arg(&output_path);

    if silent {
        fasm_proc.stdout(Stdio::null());
    }

    let result = fasm_proc
        .spawn()
        .expect("Failed to spawn fasm process")
        .wait_with_output()
        .expect("Failed to wait for fasm");
    if !result.status.success() {
        chs_error!(String::from_utf8_lossy(&result.stderr));
    }

    if !emit_asm {
        let mut rm_proc = process::Command::new("rm")
            .arg(fasm_path)
            .spawn()
            .expect("Failed to spawn rm process");

        let result = rm_proc.wait_with_output().expect("Failed to wait for fasm");
        if !result.status.success() {
            chs_error!("Failed {}", String::from_utf8_lossy(&result.stderr));
        }
    }

    if run {
        let mut run_proc = process::Command::new(format!("./{}", output_path.display()))
            .spawn()
            .expect("Failed to spawn rm process");

        let result = run_proc
            .wait_with_output()
            .expect("Failed to wait for fasm");
        if !result.status.success() {
            chs_error!("Failed {}", String::from_utf8_lossy(&result.stderr));
        }
    }

    Ok(())
}

fn check(file_path: String) -> CHSResult<()> {
    let module = chs_ast::parse_file(PathBuf::from(file_path))?;

    let typed_module = TypedModule::from_module(module)?;

    println!(
        "Module \"{}\" is type checked.",
        typed_module.file_path.display()
    );

    Ok(())
}
