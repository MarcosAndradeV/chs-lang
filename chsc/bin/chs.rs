#![allow(unused)]

use std::{
    env::Args,
    fs::File,
    io::Write,
    os::unix::process::CommandExt,
    path::PathBuf,
    process::{self, exit, ExitCode},
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
                .arg("INPUT", 1)
                .flag("-o", "OUTPUT")
                .flag_bool("-r"),
        )
        .add_cmd(
            "check",
            Cmd::new()
                .help("Check the program not compile.")
                .arg("INPUT", 1),
        );
    match cl.get_matches() {
        Some(("help", ..)) => cl.usage(),
        Some(("compile", flags, args)) => {
            if let Some(file_path) = args.get("INPUT").cloned() {
                match flags.get("-o").cloned() {
                    Some(out_path) => {
                        if let Err(err) = compile(file_path, Some(out_path), flags.is_present("-r")) {
                            eprintln!("{err}");
                        }
                    }
                    None => {
                        eprintln!("No file provided for `-o`");
                    }
                }
            } else {
                eprintln!("No file provided")
            }
        }
        Some(("check", _, args)) => {
            if let Some(file_path) = args.get("INPUT").cloned() {
                if let Err(err) = check(file_path) {
                    eprintln!("{err}")
                }
            } else {
                eprintln!("No file provided")
            }
        }
        _ => cl.usage(),
    }
}

fn compile(file_path: String, outpath: Option<String>, run: bool) -> CHSResult<()> {
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

    let fasm_path = fasm_code.out_path();
    let mut out_file = File::create(fasm_path).map_err(|err| CHSError(err.to_string()))?;
    write!(out_file, "{}", fasm_code).map_err(|err| CHSError(err.to_string()))?;
    println!("[INFO] Generating {}", fasm_path.display());
    let output_path = match outpath {
        Some(a) => PathBuf::from(a),
        None => fasm_path.with_extension(""),
    };
    let mut fasm_proc = process::Command::new("fasm")
        .arg(fasm_path)
        .arg(&output_path)
        .spawn()
        .expect("Failed to spawn fasm process");

    let result = fasm_proc
        .wait_with_output()
        .expect("Failed to wait for fasm");
    if !result.status.success() {
        chs_error!(String::from_utf8_lossy(&result.stderr));
    }

    let mut rm_proc = process::Command::new("rm")
        .arg(fasm_path)
        .spawn()
        .expect("Failed to spawn rm process");

    let result = rm_proc.wait_with_output().expect("Failed to wait for fasm");
    if !result.status.success() {
        chs_error!("Failed {}", String::from_utf8_lossy(&result.stderr));
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
    let module = match chs_ast::parse_file(file_path) {
        Ok(module) => module,
        Err(err) => return Err(err),
    };

    let typed_module = match TypedModule::from_module(module) {
        Ok(typed_module) => typed_module,
        Err(err) => return Err(err),
    };

    println!("Module \"{}\" is type checked.", typed_module.file_path.display());

    Ok(())
}
