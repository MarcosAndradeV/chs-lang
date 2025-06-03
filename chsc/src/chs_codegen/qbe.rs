use std::path::PathBuf;

use crate::{chs_mir::MIRModule, chs_util::{binary_exists, CHSResult}};


pub fn generate(m: MIRModule, input_path: PathBuf, output_path: PathBuf)-> CHSResult<()> {
    if !binary_exists("qbe") {
        eprintln!("[ERROR] qbe binary not found. Please install it.");
        std::process::exit(1);
    }
    todo!()
}
