use std::{fs, ops, path::Path, process::exit};

use crate::chs_lexer::{Span, Token};

pub mod hir;
pub mod nodes;
pub mod parser;
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

pub trait ModuleImpl<'src> {
    fn get_span_str<T>(&self, span: &Span<T>) -> &'src str;
    fn get_token_str(&self, token: &Token) -> &'src str;
    fn get_file_path(&self) -> &'src str;
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
