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

use chs_ast::{ parser::Parser, RawModule};
use chs_codegen::fasm;
use chs_util::{return_chs_error, CHSError, CHSResult};

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

    dbg!(module);
    todo!();
    let fasm_code = fasm::Module::new();

    let fasm_path = PathBuf::from(&raw_module.file_path).with_extension("asm");
    let mut out_file = File::create(&fasm_path).map_err(|err| CHSError(err.to_string()))?;
    write!(out_file, "{}", fasm_code).map_err(|err| CHSError(err.to_string()))?;
    if !silent {
        println!("[INFO] Generating {}", fasm_path.display());
    }
    let fasm_output_path = match outpath {
        Some(ref a) => PathBuf::from(a).with_extension("o"),
        None => fasm_path.with_extension("o"),
    };
    let mut fasm_proc = process::Command::new("fasm");
    fasm_proc.arg(&fasm_path).arg(&fasm_output_path);

    if silent {
        fasm_proc.stdout(Stdio::null());
    }

    let result = fasm_proc
        .spawn()
        .expect("Failed to spawn fasm process")
        .wait_with_output()
        .expect("Failed to wait for fasm");
    if !result.status.success() {
        return_chs_error!(String::from_utf8_lossy(&result.stderr));
    }

    let gcc_output_path = match outpath {
        Some(a) => PathBuf::from(a),
        None => fasm_path.with_extension(""),
    };

    let mut gcc_proc = process::Command::new("gcc");
    gcc_proc.arg("-static").arg("-o").arg(&gcc_output_path).arg(&fasm_output_path);

    if silent {
        gcc_proc.stdout(Stdio::null());
    }

    let result = gcc_proc
        .spawn()
        .expect("Failed to spawn gcc process")
        .wait_with_output()
        .expect("Failed to wait for gcc");
    if !result.status.success() {
        return_chs_error!(String::from_utf8_lossy(&result.stderr));
    }

    if !emit_asm {
        let mut rm_proc = process::Command::new("rm")
            .arg(fasm_path)
            .arg(fasm_output_path)
            .spawn()
            .expect("Failed to spawn rm process");

        let result = rm_proc.wait_with_output().expect("Failed to wait for rm");
        if !result.status.success() {
            return_chs_error!("Failed {}", String::from_utf8_lossy(&result.stderr));
        }
    }

    if run {
        let mut run_proc = process::Command::new(format!("./{}", gcc_output_path.display()))
            .spawn()
            .expect("Failed to spawn run process");

        let result = run_proc
            .wait_with_output()
            .expect("Failed to wait for run");
        if !result.status.success() {
            return_chs_error!("Failed {}", String::from_utf8_lossy(&result.stderr));
        }
    }

    Ok(())
}
