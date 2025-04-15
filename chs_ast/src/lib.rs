pub mod nodes;
pub mod parser;

#[derive(Debug)]
pub struct RawModule {
    source: String,
    pub file_path: String,
}

impl RawModule {
    pub fn new(source: String, file_path: String) -> Self {
        Self { source, file_path }
    }
}


use std::{fs, path::Path, process::exit};
pub fn read_flie<P: AsRef<Path>>(file_path: P) -> String {
    match fs::read_to_string(file_path) {
        Ok(ok) => ok,
        Err(err) => {
            eprintln!("[ERROR] Cannot read file: {err}");
            exit(-1);
        }
    }
}
