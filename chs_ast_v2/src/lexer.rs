use std::{fmt, iter::Peekable};

use chs_util::Loc;

type Data = std::iter::Peekable<std::vec::IntoIter<u8>>;
pub type Lexer = Peekable<std::vec::IntoIter<Token>>;

const KEYWORDS: [&'static str; 4] = ["fn", "if", "while", ":="];

fn skip_white_space(data: &mut Data, loc: &mut Loc) {
    while let Some(c) = data.peek() {
        if c.is_ascii_whitespace() {
            advance(data, loc);
            continue;
        }
        break;
    }
}

fn advance(data: &mut Data, loc: &mut Loc) {
    if let Some(ch) = data.next() {
        *loc = loc.next(ch);
    }
}

fn skip_comments(data: &mut Data, loc: &mut Loc) {
    while let Some(c) = data.peek() {
        if *c != b'\n' {
            advance(data, loc);
            continue;
        }
        break;
    }
}
fn is_punctuation(x: u8) -> bool {
    matches!(x, b'{' | b'}' | b'[' | b']' | b'(' | b')')
}

fn is_number(x: u8) -> bool {
    matches!(x, b'0'..=b'9')
}

fn is_word(x: u8) -> bool {
    matches!(x, b'a'..=b'z' | b'A'..=b'Z')
}

pub fn lexer_file(data: Vec<u8>) -> Lexer {
    let mut data = data.into_iter().peekable();
    let mut loc = Loc::new(1, 1);
    let mut tokens = vec![];
    while let Some(curr_char) = data.peek() {
        match *curr_char {
            ch if ch.is_ascii_whitespace() => skip_white_space(&mut data, &mut loc),
            ch if is_number(ch) => {
                let start_loc = loc;
                let mut value = String::from(ch as char);
                advance(&mut data, &mut loc);
                loop {
                    match data.peek() {
                        Some(ch) if is_number(*ch) => value.push(*ch as char),
                        _ => break,
                    }
                    advance(&mut data, &mut loc);
                }
                tokens.push(Token::new(value, TokenKind::Number, start_loc));
            }
            ch if is_punctuation(ch) => {
                tokens.push(Token::new(
                    String::from(ch as char),
                    TokenKind::Punctuation,
                    loc,
                ));
                advance(&mut data, &mut loc);
            }
            b'/' => {
                if data.next_if_eq(&b'/').is_some() {
                    skip_comments(&mut data, &mut loc);
                    continue;
                }
                let start_loc = loc;
                advance(&mut data, &mut loc);
                advance(&mut data, &mut loc);
                tokens.push(Token::new("/".to_string(), TokenKind::Word, start_loc))
            }
            ch if !is_punctuation(ch) && !is_number(ch) => {
                let start_loc = loc;
                let mut value = String::from(ch as char);
                advance(&mut data, &mut loc);
                loop {
                    match data.peek() {
                        Some(ch) if !ch.is_ascii_whitespace() && !is_punctuation(*ch) => {
                            value.push(*ch as char)
                        }
                        _ => break,
                    }
                    advance(&mut data, &mut loc);
                }
                if KEYWORDS.contains(&value.as_str()) {
                    tokens.push(Token::new(value, TokenKind::KeyWord, start_loc));
                } else {
                    tokens.push(Token::new(value, TokenKind::Word, start_loc));
                }
            }

            _ => todo!(),
        }
    }

    tokens.into_iter().peekable()
}

///
/// NUMBERS
/// TODO: suport for floats. => .0 | .f | .0f
/// TODO: suport for diferet bases. (like erlang) => 2:010 | 16:FF
///
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
    fn test_lexer() {
        dbg!(lexer_file(" { [ ( ) ] }".to_string().into_bytes()));
    }

    #[test]
    fn test_word() {
        dbg!(lexer_file(" := ".to_string().into_bytes()));
    }
}
