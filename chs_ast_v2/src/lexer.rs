use chs_util::Loc;

fn skip_white_space(data: &mut std::iter::Peekable<std::vec::IntoIter<u8>>) {
    while let Some(c) = data.peek() {
        if c.is_ascii_whitespace() {
            data.next();
            continue;
        }
        break;
    }
}

pub fn lex(mut data: std::iter::Peekable<std::vec::IntoIter<u8>>) -> Vec<Token> {
    let mut tokens = vec![];
    while let Some(ch) = data.peek() {
        match ch {
            ch if ch.is_ascii_digit() => {
                // numbers
                let mut number = String::from(*ch as char);
                while let Some(c) = data.peek() {
                    if c.is_ascii_digit() {
                        if let Some(ch) = data.next() {
                            number.push(ch as char)
                        }
                        continue;
                    }
                    break;
                }
                tokens.push(Token::new(number, TokenKind::Number, Loc::new(0, 0)))
            }
            b'{' | b'}' | b'[' | b']' | b'(' | b')' => {
                tokens.push(Token::new(String::from(*ch as char), TokenKind::Punctuation, Loc::new(0, 0)))
            } // punctuation
            ch if ch.is_ascii_whitespace() => {
                skip_white_space(&mut data);
            }
            _ => {
                let mut word = String::new();
                while let Some(c) = data.peek() {
                    if !c.is_ascii_whitespace() {
                        if let Some(ch) = data.next() {
                            word.push(ch as char)
                        }
                        continue;
                    }
                    break;
                }
                tokens.push(Token::new(word, TokenKind::Word, Loc::new(0, 0)))
            }
        }
        data.next();
    }
    return tokens;
}

#[derive(Debug, Clone)]
pub enum TokenKind {
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