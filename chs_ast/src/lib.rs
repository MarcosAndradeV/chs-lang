use std::path::PathBuf;

use chs_lexer::Lexer;
use chs_util::CHSResult;
use nodes::Module;
use parser::Parser;

pub mod nodes;
pub mod parser;

pub fn parse_file(file_path: String) -> CHSResult<Module> {
    Parser::new(
        Lexer::new(PathBuf::from(file_path))?
    ).parse(None)
}
