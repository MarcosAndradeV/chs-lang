use clap::{Parser, Subcommand};

use crate::config::VERSION;

#[derive(Parser)]
#[command(name = "chsc")]
#[command(author = "Marcos V. Andrade Almeida <mastermarcos1212@hotmail.com>")]
#[command(version = VERSION)]
#[command(about = "Compile and run chs files", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Compile a source file
    Compile {
        /// Source file to compile
        input: String,

        /// Output file name
        #[arg(short, long)]
        output: Option<String>,

        /// Silences compiler output
        #[arg(short, long)]
        silent: bool,

        /// Force compilation even if the source file has not changed
        #[arg(short, long)]
        force: bool,

        /// Keep the intermediate files
        #[arg(long)]
        keep: bool,

        /// Pass flags to the CC compiler (can be specified multiple times)
        #[arg(short = 'C', long = "compiler-flag", num_args = 1)]
        compiler_flags: Vec<String>,
    },

    /// Compile and run the executable
    CompileRun {
        /// Source file to compile
        input: String,

        /// Output file name
        #[arg(short, long)]
        output: Option<String>,

        /// Force compilation even if the source file has not changed
        #[arg(short, long)]
        force: bool,

        /// Pass flags to the CC compiler (can be specified multiple times)
        #[arg(short = 'C', long = "compiler-flag", num_args = 1)]
        compiler_flags: Vec<String>,
    },
}
