use std::{fs, ops, path::Path, process::exit};

use crate::chs_lexer::{Span, Token};

pub mod flow_checker;
pub mod hir;
pub mod mir;
pub mod nodes;
pub mod parser;
pub mod typechecker;
pub mod ast;
pub mod new_parser;

#[derive(Debug)]
pub struct RawModule {
    pub source: String,
    pub file_path: String,
}

impl RawModule {
    pub fn new(source: String, file_path: String) -> Self {
        Self { source, file_path }
    }
}

// TODO: Use CHSResult<String> instead of String
pub fn read_file<P: AsRef<Path>>(file_path: P) -> String {
    match fs::read_to_string(file_path) {
        Ok(ok) => ok,
        Err(err) => {
            eprintln!("[ERROR] Cannot read file: {err}");
            exit(-1);
        }
    }
}
