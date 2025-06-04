use std::path::PathBuf;

use chsc::{
    chs_ast::{self, RawModule, hir::HIRModule, parser::Parser, typechecker::TypeChecker},
    chs_codegen, chs_error,
    chs_mir::MIRModule,
    chs_util::{CHSError, CHSResult, run_cc, run_exe, run_fasm},
    cli,
    config::{Arch, Backend, Config, Os, Target},
};
use clap::Parser as _;

fn main() {
    let chs_config = Config::default();

    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Compile {
            input,
            output,
            compiler_flags,
        } => match compile(chs_config, input, output, compiler_flags) {
            Ok(out) => {
                println!("[INFO] Compiled to {}", out.display())
            }
            Err(err) => {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        },

        cli::Commands::CompileRun {
            input,
            output,
            compiler_flags,
        } => match compile(chs_config, input, output, compiler_flags) {
            Ok(exe) => {
                println!("[INFO] Compiled to {}", exe.display());
                println!("[INFO] Running {}", exe.display());

                match run_exe(exe) {
                    Ok(e) => {
                        std::process::exit(e.code().unwrap_or(0));
                    }
                    Err(err) => {
                        eprintln!("[ERROR] {}", err);
                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                eprintln!("[ERROR] {}", err);
                std::process::exit(1);
            }
        },
        cli::Commands::Version => {
            println!("chs version {} {}", chs_config.version, chs_config.target);
        }
    }
}

#[allow(unused)]
fn compile(
    chs_config: Config,
    input_path: String,
    output_path: Option<String>,
    compiler_flags: Vec<String>,
) -> CHSResult<PathBuf> {
    let input_path = PathBuf::from(&input_path);
    let output_path = output_path
        .map(PathBuf::from)
        .unwrap_or_else(|| input_path.with_extension(""));

    println!("[INFO] Reading module from file: {}", input_path.display());
    let raw_module = RawModule::new(
        chs_ast::read_file(&input_path),
        input_path.display().to_string(),
    );
    let mut lexer = chslexer::PeekableLexer::new(&raw_module.source);

    println!("[INFO] Parsing module...");

    let module = Parser::new(&mut lexer)
        .parse()
        .map_err(|err| chs_error!("{}", err))?;

    println!("[INFO] Converting to HIR...");
    let mut module = HIRModule::from_ast(module);

    println!("[INFO] Running type checker...");
    let mut checker = TypeChecker::new(&raw_module);
    checker.check_module(&mut module)?;
    let module = MIRModule::from_hir(module);

    match chs_config.target {
        Target(Backend::FASM, Arch::X86_64, Os::LINUX) => {
            let asm_path = output_path.with_extension("asm");
            let o_path = output_path.with_extension("o");
            chs_codegen::fasm_x86_64_linux::generate(&asm_path, module);
            run_fasm(&asm_path, &o_path)?;
            run_cc(compiler_flags, &o_path, &output_path)?;
        }
    }

    Ok(output_path)
}
