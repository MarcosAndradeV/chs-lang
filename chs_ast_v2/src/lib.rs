#![allow(unused)]

use std::process::exit;

use lexer::{lexer_file, Lexer, Token, TokenKind};
use nodes::{Expr, Module, TopLevel};

mod lexer;
mod nodes;

struct Parser {
    pub lex: Lexer,
    pub filename: String,
}

use lexer::TokenKind::*;
pub fn parse(filename: String, input: Vec<u8>) -> Module {
    let mut iter_lexer: Lexer = lexer_file(input);
    let mut parser = Parser {
        lex: iter_lexer,
        filename,
    };
    let mut program = vec![];
    while let Some(token) = parser.lex.next() {
        match token.kind {
            KeyWord if token == *"fn" => program.push(parse_fn(&mut parser)),
            _ => todo!(), // unsuported top level
        }
    }
    Module {
        filesource: parser.filename,
        program,
    }
}

fn parse_fn(parser: &mut Parser) -> TopLevel {
    let name = expect_kind(parser, Word);
    let args: Vec<String> = parse_args(parser);
    expect_punctuation(parser, "{");
    let mut body = vec![];
    expect_punctuation(parser, "}");
    TopLevel::Fn(name.value, args.into(), body.into())
}

fn parse_body(parser: &mut Parser) -> Vec<Expr> {
    let mut exprs = vec![];
    loop {
        if let Some(arg) = parser.lex.next_if(|t| t != "}") {
            match arg.kind {
                KeyWord => todo!(),
                Word => todo!(),
                Punctuation => todo!(),
                Number => todo!(),
            }
            continue;
        }
        break;
    }

    exprs
}

fn parse_args(parser: &mut Parser) -> Vec<String> {
    let mut args = vec![];
    loop {
        if let Some(arg) = parser.lex.next_if(|t| t.kind == TokenKind::Word) {
            args.push(arg.value);
            continue;
        }
        break;
    }
    args
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
