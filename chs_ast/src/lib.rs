use std::{fs, path::PathBuf};

use chs_lexer::Lexer;
use chs_util::{chs_error, CHSResult};
use parser::Parser;
use nodes::Module;

pub mod nodes;
pub mod parser;


pub fn parse_file(file_path: String) -> CHSResult<Module> {
    match fs::read(&file_path) {
        Ok(input) => Parser::new(Lexer::new(PathBuf::from(file_path), input)).parse(),
        Err(err) => chs_error!("ERROR: {}", err)
    }
}
