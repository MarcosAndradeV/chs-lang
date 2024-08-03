use std::{fmt, iter::Peekable};

use chs_util::Loc;

type RawData = std::iter::Peekable<std::vec::IntoIter<u8>>;

fn skip_white_space(data: &mut RawData, loc: &mut Loc) {
    while let Some(c) = data.peek() {
        if c.is_ascii_whitespace() {
            *loc = loc.next(*c);
            data.next();
            continue;
        }
        break;
    }
}

pub type Lexer = Peekable<std::vec::IntoIter<Token>>;

pub fn lexer(mut data: RawData) -> Lexer {
    let mut tokens = vec![];
    let mut loc = Loc::new(1, 1);
    'loop1: while let Some(ch) = data.peek() {
        match ch {
            ch if ch.is_ascii_digit() => {
                // numbers
                // TODO: suport for floats. => .0 | .f | .0f
                // TODO: suport for diferet bases. (like erlang) => 2:010 | 16:FF
                let mut number = String::new();
                while let Some(c) = data.peek() {
                    if c.is_ascii_digit() {
                        loc = loc.next(*c);
                        if let Some(ch) = data.next() {
                            number.push(ch as char)
                        }
                        continue;
                    }
                    break;
                }
                tokens.push(Token::new(number, TokenKind::Number, loc))
            }
            b'{' | b'}' | b'[' | b']' | b'(' | b')' => tokens.push(Token::new(
                String::from(*ch as char),
                TokenKind::Punctuation,
                loc,
            )), // punctuation
            ch if ch.is_ascii_whitespace() => {
                skip_white_space(&mut data, &mut loc);
            }
            ch if *ch == b'/' => {
                if data.peek().is_some_and(|a| a == &b'/') {
                    while let Some(c) = data.peek() {
                        if *c != b'\n' {
                            loc = loc.next(*c);
                            data.next();
                            continue;
                        }
                        break;
                    }
                }
            }
            _ => {
                let mut word = String::new();
                while let Some(c) = data.peek() {
                    if !(c.is_ascii_whitespace()
                        || matches!(c, b'{' | b'}' | b'[' | b']' | b'(' | b')'))
                    {
                        loc = loc.next(*c);
                        if let Some(ch) = data.next() {
                            word.push(ch as char)
                        }
                        continue;
                    }
                    break;
                }
                if matches!(
                    word.as_str(),
                    KEYWORD_FN | KEYWORD_IF | KEYWORD_WHILE | KEYWORD_ASSING
                ) {
                    tokens.push(Token::new(word, TokenKind::KeyWord, loc))
                } else {
                    tokens.push(Token::new(word, TokenKind::Word, loc))
                }
                continue;
            }
        }
        data.next();
    }
    return tokens.into_iter().peekable();
}

const KEYWORD_FN: &'static str = "fn";
const KEYWORD_IF: &'static str = "if";
const KEYWORD_WHILE: &'static str = "while";
const KEYWORD_ASSING: &'static str = ":=";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    KeyWord,
    Word,
    Punctuation,
    Number,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub loc: Loc,
}
impl Token {
    pub fn new(value: String, kind: TokenKind, loc: Loc) -> Self {
        Self { kind, value, loc }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({}) at {}", self.kind, self.value, self.loc)
    }
}

impl PartialEq<str> for Token {
    fn eq(&self, other: &str) -> bool {
        self.value.eq(other)
    }
    fn ne(&self, other: &str) -> bool {
        self.value.ne(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_words() {
        let mut res = lexer(String::from("fn main").into_bytes().into_iter().peekable());
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("fn"));
            assert!(t.kind == TokenKind::KeyWord);
        }
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("main"));
            assert!(t.kind == TokenKind::Word);
        }
    }

    #[test]
    fn test_punctuation() {
        let mut res: Lexer = lexer(String::from("{}").into_bytes().into_iter().peekable());
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("{"));
            assert!(t.kind == TokenKind::Punctuation);
        }
        if let Some(t) = res.next() {
            assert!(t.eq("}"));
            assert!(t.kind == TokenKind::Punctuation);
        }
    }
    #[test]
    fn test_punctuation_words() {
        let mut res: Lexer = lexer(String::from("main{}").into_bytes().into_iter().peekable());
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("main"));
            assert!(t.kind == TokenKind::Word);
        }
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("{"));
            assert!(t.kind == TokenKind::Punctuation);
        }
        if let Some(t) = res.next() {
            dbg!(&t);
            assert!(t.eq("}"));
            assert!(t.kind == TokenKind::Punctuation);
        }
    }

    #[test]
    fn test_numbers() {
        let mut res: Lexer = lexer(String::from("10").into_bytes().into_iter().peekable());
        if let Some(t) = res.next() {
            assert!(t.eq("10"));
            assert!(t.kind == TokenKind::Number);
        }
    }

    #[test]
    fn test_comments() {
        let mut res: Lexer = lexer(
            String::from(
                "a // a
        ",
            )
            .into_bytes()
            .into_iter()
            .peekable(),
        );
        if let Some(t) = res.next() {
            assert!(t.eq("a"));
            assert!(t.kind == TokenKind::Word);
        }
        assert!(res.next().is_none());
    }
}
