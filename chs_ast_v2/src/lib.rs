#![allow(unused)]

use std::process::exit;

use lexer::{lexer_file, Lexer, Token, TokenKind};
use nodes::{Module, TopLevel};

mod lexer;
mod nodes;

struct Parser {
    pub lex: Lexer,
    pub filename: String
}

use lexer::TokenKind::*;
pub fn parse(filename: String, input: Vec<u8>) -> Module {
    let mut iter_lexer: Lexer = lexer_file(input);
    let mut parser = Parser {
        lex: iter_lexer,
        filename
    };
    let mut program = vec![];
    while let Some(token) = parser.lex.next() {
        match token.kind {
            KeyWord if token == *"fn" => program.push(parse_fn(&mut parser)),
            _ => todo!(), // unsuported top level
        }
    }
    Module { filesource: parser.filename, program }
}

fn parse_fn(parser: &mut Parser) -> TopLevel {
    let name = expect_kind(parser, Word);
    expect_punctuation(parser, "{");
    let mut body = vec![];
    expect_punctuation(parser, "}");
    TopLevel::Fn(name.value, body.into())
}

fn expect_kind(parser: &mut Parser, kind: TokenKind) -> Token {
    match parser.lex.next() {
        Some(t) if t.kind == kind => t,
        Some(t) => {
            eprintln!("Parsing Error: Unexpected Token found {:?}", t.kind);
            eprintln!("    {}{}", parser.filename, t.loc);
            exit(-1);
        }
        _ => {
            eprintln!("Parsing Error: Unexpected Token found EOF");
            exit(-1);
        }
    }
}

fn expect_punctuation(parser: &mut Parser, punctuation: &str) -> Token {
    match parser.lex.next() {
        Some(t) if t.kind == Punctuation && t == *punctuation => t,
        Some(t) => {
            eprintln!("Parsing Error: `{}` Token found {:?}", punctuation, t.kind);
            eprintln!("    {}{}", parser.filename, t.loc);
            exit(-1);
        }
        _ => {
            eprintln!("Parsing Error: `{}` Token found EOF", punctuation);
            exit(-1);
        }
    }
}
