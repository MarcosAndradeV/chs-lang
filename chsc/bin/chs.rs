use std::{fs, path::PathBuf, process::Command};

use chsc::{
    chs_ast::{
        self, flow_checker::FlowChecker, hir::HIRModule, mir::MIRModule, parser::Parser, typechecker::TypeChecker, RawModule
    },
    chs_codegen::Backend,
    chs_error,
    chs_util::{binary_exists, file_changed, CHSError, CHSResult},
    cli,
    config::Config,
    return_chs_error,
};
use clap::Parser as _;

fn main() {
    let _chs_config = Config::default();

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
            compiler_flags,
            silent,
            force,
            keep,
        } => {
            if !silent {
                println!("[INFO] Compiling file: {}", input);
                if let Some(ref out) = output {
                    println!("[INFO] Output: {}", out);
                }
            }
            let result = compile(input, output, compiler_flags, false, silent, force, keep);
            if let Err(err) = result {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        }

        cli::Commands::CompileRun {
            input,
            output,
            force,
            compiler_flags,
        } => {
            let result = compile(input, output, compiler_flags, true, true, force, false);
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

fn compile(
    input_path: String,
    outpath: Option<String>,
    compiler_flags: Vec<String>,
    run: bool,
    silent: bool,
    force: bool,
    keep: bool,
) -> CHSResult<()> {
    let file_path = PathBuf::from(&input_path);
    let ssa_path = file_path.with_extension("ssa");
    let asm_path = file_path.with_extension("s");
    let out_path = outpath
        .map(PathBuf::from)
        .unwrap_or_else(|| file_path.with_extension(""));

    if !file_changed(&file_path, &out_path) && !force {
        log!(
            silent && !run,
            "[INFO] Skipping rebuild, using cached output."
        );
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
    let mut module = HIRModule::from_ast(module);

    log!(silent, "[INFO] Running type checker...");
    let mut checker = TypeChecker::new(module.raw_module);
    checker.check_module(&mut module)?;

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

    let backend = Backend::Qbe;
    log!(silent, "[INFO] Generating code using {backend} backend...");
    let gen_code = backend.generate_module(module)?;

    fs::write(&ssa_path, gen_code.to_string())
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
        "[INFO] CMD: cc -o {} {} {}",
        out_path.display(),
        asm_path.display(),
        compiler_flags.join(" ")
    );
    let mut cc_command = Command::new("cc");
    cc_command.arg("-o").arg(&out_path).arg(&asm_path);

    if !compiler_flags.is_empty() {
        cc_command.args(&compiler_flags);
    }

    let output = cc_command
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

    if !keep {
        log!(silent, "[INFO] Cleaning up temporary files...");
        let paths = &[&ssa_path, &asm_path];
        cleanup_files(silent, paths);
    }

    Ok(())
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
