
use std::path::PathBuf;

use chs_lexer::{Lexer, read_flie};
use chs_util::CHSResult;
use nodes::Module;
use parser::Parser;

pub mod nodes;
pub mod parser;

pub fn parse_file(file_path: PathBuf) -> CHSResult<Module> {
    let lexer = Lexer::new(read_flie(&file_path), file_path);
    Parser::new(lexer).parse(None)
}
