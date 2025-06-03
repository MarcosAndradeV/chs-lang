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

        /// Pass flags to the CC compiler (can be specified multiple times)
        #[arg(short = 'C', long = "compiler-flag", num_args = 1)]
        compiler_flags: Vec<String>,
    },

    /// Print chs version
    Version,
}
