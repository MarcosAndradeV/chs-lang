use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "chsc")]
#[command(author = "Marcos V. Andrade <>")]
#[command(version = "0.1")]
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
    },
}
