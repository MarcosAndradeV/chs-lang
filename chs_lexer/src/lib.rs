use std::fmt;
use std::rc::Rc;

pub struct Lexer<'src> {
    source: &'src str,
    data: &'src [u8],
    pos: usize,
    loc: Loc,
}

impl<'src> Lexer<'src> {
    pub const KEYWORDS: &'static [&'static str] = &["fn", "end", "if", "else", "return"];
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            data: source.as_bytes(),
            loc: Loc::new(1, 1),
            pos: 0,
        }
    }

    fn advance(&mut self) -> u8 {
        let ch = self.read_char();
        self.pos += 1;
        self.loc.next(ch);
        ch
    }

    fn read_char(&mut self) -> u8 {
        let pos = self.pos;
        if pos >= self.data.len() {
            0
        } else {
            self.data[pos]
        }
    }

    pub fn next(&mut self) -> Token {
        while self.pos <= self.data.len() {
            let begin = self.pos;
            let ch = self.advance();
            let loc = self.loc;
            match ch {
                b'/' if self.read_char() == b'/' => {
                    while self.advance() != b'\n' {}
                    continue;
                }
                b'-' if self.read_char() == b'>' => {
                    self.advance();
                    return Token::new(TokenKind::Arrow, loc, &self.source[begin..self.pos]);
                }
                b'=' if self.read_char() == b'=' => {
                    self.advance();
                    return Token::new(TokenKind::Eq, loc, &self.source[begin..self.pos]);
                }
                // b'=' if self.read_char() == b'=' => {
                //     self.advance();
                //     Token::from_string(loc, TokenKind::Eq, "==".to_string())
                // }
                // b'!' if self.read_char() == b'=' => {
                //     self.advance();
                //     Token::from_string(loc, TokenKind::NotEq, "!=".to_string())
                // }
                // b'|' if self.read_char() == b'|' => {
                //     self.advance();
                //     Token::from_string(loc, TokenKind::DoublePipe, "!=".to_string())
                // }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return self.lex_identfier_or_keyword(begin);
                }
                b'0'..=b'9' => return self.lex_number(begin),
                b'"' => return self.lex_string(begin),
                // b'\'' => self.lex_char(),

                // b'.' => Token::from_u8(loc, TokenKind::Dot, ch),
                b',' => return Token::new(TokenKind::Comma, loc, &self.source[begin..self.pos]),
                b';' => {
                    return Token::new(TokenKind::SemiColon, loc, &self.source[begin..self.pos]);
                }
                b':' => return Token::new(TokenKind::Colon, loc, &self.source[begin..self.pos]),
                b'=' => return Token::new(TokenKind::Assign, loc, &self.source[begin..self.pos]),
                b'<' => return Token::new(TokenKind::Lt, loc, &self.source[begin..self.pos]),
                b'>' => return Token::new(TokenKind::Gt, loc, &self.source[begin..self.pos]),

                b'!' => return Token::new(TokenKind::Bang, loc, &self.source[begin..self.pos]),
                b'+' => return Token::new(TokenKind::Plus, loc, &self.source[begin..self.pos]),
                b'-' => return Token::new(TokenKind::Minus, loc, &self.source[begin..self.pos]),
                b'*' => return Token::new(TokenKind::Asterisk, loc, &self.source[begin..self.pos]),
                b'/' => return Token::new(TokenKind::Slash, loc, &self.source[begin..self.pos]),
                b'%' => return Token::new(TokenKind::Mod, loc, &self.source[begin..self.pos]),
                // b'&' => Token::from_u8(loc, TokenKind::Ampersand, ch),
                // b'|' => Token::from_u8(loc, TokenKind::Pipe, ch),
                b'(' => {
                    return Token::new(TokenKind::OpenParen, loc, &self.source[begin..self.pos]);
                }
                b')' => {
                    return Token::new(TokenKind::CloseParen, loc, &self.source[begin..self.pos]);
                }
                b'[' => {
                    return Token::new(TokenKind::OpenSquare, loc, &self.source[begin..self.pos]);
                }
                b']' => {
                    return Token::new(TokenKind::CloseSquare, loc, &self.source[begin..self.pos]);
                }
                // b'{' => Token::from_u8(loc, TokenKind::CurlyOpen, ch),
                // b'}' => Token::from_u8(loc, TokenKind::CurlyClose, ch),

                // b'[' => Token::from_u8(loc, TokenKind::SquareOpen, ch),
                // b']' => Token::from_u8(loc, TokenKind::SquareClose, ch),
                ch if ch.is_ascii_whitespace() => continue,
                0 => return Token::new(TokenKind::EOF, self.loc, &self.source[begin..begin]),
                _ => return Token::new(TokenKind::Invalid, loc, &self.source[begin..self.pos]),
            }
        }
        Token::new(TokenKind::EOF, self.loc, &self.source[self.pos..self.pos])
    }

    fn lex_identfier_or_keyword(&mut self, begin: usize) -> Token {
        let loc = self.loc;
        loop {
            let ch = self.read_char();
            match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => (),
                _ => break,
            }
            self.advance();
        }
        match &self.source[begin..self.pos] {
            "end" => Token::new(TokenKind::KeywordEnd, loc, &self.source[begin..self.pos]),
            "fn" => Token::new(TokenKind::KeywordFn, loc, &self.source[begin..self.pos]),
            "extern" => Token::new(TokenKind::KeywordExtern, loc, &self.source[begin..self.pos]),
            "set" => Token::new(TokenKind::KeywordSet, loc, &self.source[begin..self.pos]),
            "if" => Token::new(TokenKind::KeywordIf, loc, &self.source[begin..self.pos]),
            "else" => Token::new(TokenKind::KeywordElse, loc, &self.source[begin..self.pos]),
            "while" => Token::new(TokenKind::KeywordWhile, loc, &self.source[begin..self.pos]),
            "true" => Token::new(TokenKind::KeywordTrue, loc, &self.source[begin..self.pos]),
            "false" => Token::new(TokenKind::KeywordFalse, loc, &self.source[begin..self.pos]),
            "cast" => Token::new(TokenKind::KeywordCast, loc, &self.source[begin..self.pos]),
            "syscall" => Token::new(TokenKind::KeywordSyscall, loc, &self.source[begin..self.pos]),
            _ => Token::new(TokenKind::Identifier, loc, &self.source[begin..self.pos]),
        }
    }

    fn lex_number(&mut self, begin: usize) -> Token {
        let mut kind = TokenKind::IntegerNumber;
        let loc = self.loc();
        loop {
            let ch = self.read_char();
            match ch {
                b'0'..=b'9' => (),
                b'.' if kind != TokenKind::RealNumber => {
                    kind = TokenKind::RealNumber;
                }
                _ => break,
            }
            self.advance();
        }
        Token::new(kind, loc, &self.source[begin..self.pos])
    }

    fn lex_string(&mut self, begin: usize) -> Token {
        let mut buffer = String::new();
        let kind = TokenKind::StringLiteral;
        let loc = self.loc();
        loop {
            let ch = self.read_char();
            match ch {
                b'"' => {
                    self.advance();
                    break;
                }
                b'\0' => {
                    return Token::new(TokenKind::Invalid, loc, &self.source[begin..self.pos])
                }
                b'\\' => {
                    self.advance();
                    let ch = self.read_char();
                    match ch {
                        b'r' => buffer.push('\r'),
                        b'n' => buffer.push('\n'),
                        b'\\' => buffer.push('\\'),
                        b'0' => buffer.push('\0'),
                        _ => {
                            return Token::new(TokenKind::Invalid, loc, &self.source[begin..self.pos])
                        }
                    }
                }
                _ => buffer.push(ch as char),
            }
            self.advance();
        }
        Token::new(kind, loc, &buffer)
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub source: Rc<str>, // TODO: use Range<usize> instead of Rc<str>
}

impl Token {
    pub fn new(kind: TokenKind, loc: Loc, source: &str) -> Self {
        Self {
            kind,
            loc,
            source: source.into(),
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }

    pub fn is_invalid(&self) -> bool {
        matches!(self.kind, TokenKind::Invalid)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    #[default]
    EOF,
    Invalid,

    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,

    Identifier,
    Keyword,
    KeywordFn,
    KeywordEnd,
    KeywordExtern,
    KeywordSet,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordTrue,
    KeywordFalse,
    KeywordCast,
    KeywordSyscall,

    IntegerNumber,
    RealNumber,
    StringLiteral,
    CharacterLiteral,

    Dot,
    Comma,
    Colon,
    SemiColon,
    Arrow,

    Assign,
    Bang,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Eq,
    NotEq,
    Gt,
    Lt,
    Mod,
    Ampersand,
    Pipe,
    DoubleAmpersand,
    DoublePipe,
}
impl TokenKind {
    pub fn is_binop(&self) -> bool {
        use TokenKind::*;
        matches!(self, Plus|Minus|Asterisk|Slash|Eq|NotEq|Gt|Lt|Mod|Ampersand|Pipe|DoubleAmpersand|DoublePipe)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Loc {
    line: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Loc {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn next_column(&mut self) {
        self.col += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn next(&mut self, c: u8) {
        match c {
            b'\n' => self.next_line(),
            b'\t' => {
                let ts = 8;
                self.col = (self.col / ts) * ts + ts;
            }
            c if c.is_ascii_control() => {}
            _ => self.next_column(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_kinds() {
        let source = "fn main() end";
        let mut lex = Lexer::new(source);
        assert!(lex.next().kind == TokenKind::Keyword);
        assert!(lex.next().kind == TokenKind::Identifier);
        assert!(lex.next().kind == TokenKind::OpenParen);
        assert!(lex.next().kind == TokenKind::CloseParen);
        assert!(lex.next().kind == TokenKind::Keyword);
    }

    #[test]
    fn test_lexer_source() {
        let source = "fn main() 123 end";
        let mut lex = Lexer::new(source);
        assert!(*lex.next().source == *"fn");
        assert!(*lex.next().source == *"main");
        assert!(*lex.next().source == *"(");
        assert!(*lex.next().source == *")");
        assert!(*lex.next().source == *"123");
        assert!(*lex.next().source == *"end");
    }
}
