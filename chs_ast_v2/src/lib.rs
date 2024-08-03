mod lexer;
use std::process::exit;

use lexer::*;

pub fn parse(filename: String, input: Vec<u8>) {
    let mut lex: Lexer  = lexer(input.into_iter().peekable());
    while let Some(token) = lex.peek() {
        match token.kind {
            TokenKind::Word if token == "fn" => {
                // fn <name> { }
                todo!()
            }
            _ => {
                eprintln!("Parsing Error: {:?} is not imoplemeted in the top level.", token);
                exit(-1);
            }
        }
    }
}


