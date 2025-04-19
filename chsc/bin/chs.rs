use std::{fs, path::PathBuf, process::Command};

use chsc::{
    chs_ast::{
        self, RawModule, flow_checker::FlowChecker, hir::HIRModule, mir::MIRModule, parser::Parser,
        typechecker::TypeChecker,
    },
    chs_codegen::qbe_backend::QBEBackend,
    chs_error,
    chs_util::{CHSError, CHSResult, binary_exists, file_changed},
    cli, return_chs_error,
};
use clap::Parser as _;

fn main() {
    if !binary_exists("qbe") {
        eprintln!("[ERROR] qbe binary not found. Please install it.");
        std::process::exit(1);
    }

    if !binary_exists("cc") {
        eprintln!("[ERROR] cc binary not found.");
        std::process::exit(1);
    }

    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Compile {
            input,
            output,
            silent,
            force,
        } => {
            if !silent {
                println!("[INFO] Compiling file: {}", input);
                if let Some(ref out) = output {
                    println!("[INFO] Output: {}", out);
                }
            }
            let result = compile(input, output, false, silent, force);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }

        cli::Commands::CompileRun { input, output, force} => {
            let result = compile(input, output, true, true, force);
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

fn compile(input_path: String, outpath: Option<String>, run: bool, silent: bool, force: bool) -> CHSResult<()> {
    let file_path = PathBuf::from(&input_path);
    let ssa_path = file_path.with_extension("ssa");
    let asm_path = file_path.with_extension("s");
    let out_path = outpath
        .map(PathBuf::from)
        .unwrap_or_else(|| file_path.with_extension(""));

    if !file_changed(&file_path, &asm_path) && !force {
        log!(silent && !run, "[INFO] Skipping rebuild, using cached output.");
        if run {
            run_exe(out_path)?;
        }
        return Ok(());
    }

    log!(silent, "[INFO] Reading module from file: {}", input_path);
    let raw_module = RawModule::new(chs_ast::read_file(&input_path), input_path);

    log!(silent, "[INFO] Parsing module...");
    let module = Parser::new(&raw_module).parse()?;

    log!(silent, "[INFO] Converting to HIR...");
    let module = HIRModule::from_ast(module);

    log!(silent, "[INFO] Running type checker...");
    let mut checker = TypeChecker::new(module.raw_module);
    checker.check_module(&module)?;

    log!(silent, "[INFO] Converting to MIR...");
    let tenv = checker.env();
    let module = MIRModule::from_hir(module, tenv);

    log!(silent, "[INFO] Running flow checker...");
    let checker = FlowChecker::new(&module);
    checker.check_module().map_err(|errors| {
        CHSError(
            errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

    log!(silent, "[INFO] Generating code using QBE backend...");
    let mut backend = QBEBackend::new(&raw_module);
    backend.generate_module(module);
    let module = backend.finish();

    fs::write(&ssa_path, module.to_string())
        .map_err(|e| chs_error!("Failed to write SSA file: {}", e))?;

    log!(
        silent,
        "[INFO] CMD: qbe -o {} {}",
        asm_path.display(),
        ssa_path.display()
    );

    let output = Command::new("qbe")
        .arg("-o")
        .arg(&asm_path)
        .arg(&ssa_path)
        .output()
        .map_err(|e| chs_error!("Failed to run qbe: {}", e))?;

    if !output.status.success() {
        return_chs_error!(
            "qbe failed to generate assembly\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    log!(
        silent,
        "[INFO] CMD: cc -o {} {}",
        out_path.display(),
        asm_path.display()
    );
    let output = Command::new("cc")
        .arg("-o")
        .arg(&out_path)
        .arg(&asm_path)
        .output()
        .map_err(|e| chs_error!("Failed to run cc: {}", e))?;

    if !output.status.success() {
        return_chs_error!(
            "cc failed to generate final executable\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    log!(silent, "[INFO] Compilation completed successfully!");

    if run {
        run_exe(out_path)?;
    }

    Ok(())
}

fn run_exe(out_path: PathBuf) -> Result<(), CHSError> {
    println!("[INFO] Running executable...");
    Command::new(&out_path)
        .status()
        .map_err(|e| chs_error!("Failed to execute binary: {}", e))?
        .success()
        .then_some(())
        .ok_or_else(|| chs_error!("Execution failed"))?;
    Ok(())
}
