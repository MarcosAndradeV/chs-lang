pub mod nodes;
pub mod parser;
pub mod hir;
pub mod mir;
pub mod typechecker;

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

impl<T> ops::Index<&Span<T>> for RawModule {
    type Output = str;

    fn index(&self, index: &Span<T>) -> &Self::Output {
        &self.source[index.start..index.end]
    }
}

impl ops::Index<&Token> for RawModule {
    type Output = str;

    fn index(&self, index: &Token) -> &Self::Output {
        &self.source[index.source.start..index.source.end]
    }
}


use std::{fs, ops, path::Path, process::exit};

use chs_lexer::{Span, Token};
pub fn read_flie<P: AsRef<Path>>(file_path: P) -> String {
    match fs::read_to_string(file_path) {
        Ok(ok) => ok,
        Err(err) => {
            eprintln!("[ERROR] Cannot read file: {err}");
            exit(-1);
        }
    }
}
