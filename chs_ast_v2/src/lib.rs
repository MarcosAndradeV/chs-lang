mod lexer;
use lexer::lex;

pub fn parse(filename: String, input: Vec<u8>) {
    // let mut data_iter: std::iter::Peekable<std::vec::IntoIter<u8>> = input.into_iter().peekable();
    let tokens: Vec<lexer::Token> = lex(input.into_iter().peekable());
    
}


