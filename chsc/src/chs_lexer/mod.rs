use std::fmt;
use std::marker::PhantomData;

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

    fn peek_suffix(&self) -> Option<String> {
        let mut buf = String::new();
        let mut i = self.pos;
        while i < self.data.len() && self.data[i].is_ascii_alphanumeric() {
            buf.push(self.data[i] as char);
            i += 1;
            if buf.len() > 3 {
                break;
            }
        }
        match buf.as_str() {
            "i32" | "u32" | "i64" | "u64" => Some(buf),
            _ => None,
        }
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn read_char(&mut self) -> u8 {
        let pos = self.pos;
        if pos >= self.data.len() {
            0
        } else {
            self.data[pos]
        }
    }

    pub fn next_token(&mut self) -> Token {
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
                    return Token::new(TokenKind::Arrow, loc, begin, self.pos);
                }
                b'=' if self.read_char() == b'=' => {
                    self.advance();
                    return Token::new(TokenKind::Eq, loc, begin, self.pos);
                }
                b'!' if self.read_char() == b'=' => {
                    self.advance();
                    return Token::new(TokenKind::NotEq, loc, begin, self.pos);
                }
                b'&' if self.read_char() == b'&' => {
                    self.advance();
                    return Token::new(TokenKind::DoubleAmpersand, loc, begin, self.pos);
                }
                b'|' if self.read_char() == b'|' => {
                    self.advance();
                    return Token::new(TokenKind::DoublePipe, loc, begin, self.pos);
                }
                b'.' if self.read_char() == b'*' => {
                    self.advance();
                    return Token::new(TokenKind::Deref, loc, begin, self.pos);
                }
                b'.' if self.read_char() == b'.' && self.read_char() == b'.' => {
                    self.advance();
                    self.advance();
                    return Token::new(TokenKind::Splat, loc, begin, self.pos);
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return self.lex_identfier_or_keyword(begin);
                }
                b'0'..=b'9' => return self.lex_number(begin),
                b'"' => return self.lex_string(begin),
                b'@' => return self.lex_macro(begin),
                // b'\'' => self.lex_char(),

                // b'.' => Token::from_u8(loc, TokenKind::Dot, ch),
                b',' => return Token::new(TokenKind::Comma, loc, begin, self.pos),
                b';' => {
                    return Token::new(TokenKind::SemiColon, loc, begin, self.pos);
                }
                b':' => return Token::new(TokenKind::Colon, loc, begin, self.pos),
                b'=' => return Token::new(TokenKind::Assign, loc, begin, self.pos),
                b'<' => return Token::new(TokenKind::Lt, loc, begin, self.pos),
                b'>' => return Token::new(TokenKind::Gt, loc, begin, self.pos),

                b'!' => return Token::new(TokenKind::Bang, loc, begin, self.pos),
                b'+' => return Token::new(TokenKind::Plus, loc, begin, self.pos),
                b'-' => return Token::new(TokenKind::Minus, loc, begin, self.pos),
                b'*' => return Token::new(TokenKind::Asterisk, loc, begin, self.pos),
                b'/' => return Token::new(TokenKind::Slash, loc, begin, self.pos),
                b'%' => return Token::new(TokenKind::Mod, loc, begin, self.pos),
                b'$' => return Token::new(TokenKind::Dollar, loc, begin, self.pos),
                b'&' => return Token::new(TokenKind::Ampersand, loc, begin, self.pos),
                b'|' => return Token::new(TokenKind::Pipe, loc, begin, self.pos),
                b'(' => {
                    return Token::new(TokenKind::OpenParen, loc, begin, self.pos);
                }
                b')' => {
                    return Token::new(TokenKind::CloseParen, loc, begin, self.pos);
                }
                b'[' => {
                    return Token::new(TokenKind::OpenSquare, loc, begin, self.pos);
                }
                b']' => {
                    return Token::new(TokenKind::CloseSquare, loc, begin, self.pos);
                }
                b'{' => {
                    return Token::new(TokenKind::OpenBrace, loc, begin, self.pos);
                }
                b'}' => {
                    return Token::new(TokenKind::CloseBrace, loc, begin, self.pos);
                }
                ch if ch.is_ascii_whitespace() => continue,
                0 => return Token::new(TokenKind::EOF, self.loc, 0, 0),
                _ => return Token::new(TokenKind::Invalid, loc, begin, self.pos),
            }
        }
        Token::new(TokenKind::EOF, self.loc, self.pos, self.pos)
    }

    fn lex_identfier_or_keyword(&mut self, begin: usize) -> Token {
        let loc = self.loc;
        loop {
            let ch = self.read_char();
            match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => (),
                b'0'..=b'9' => (),
                _ => break,
            }
            self.advance();
        }
        match &self.source[begin..self.pos] {
            "end" => Token::new(TokenKind::KeywordEnd, loc, begin, self.pos),
            "fn" => Token::new(TokenKind::KeywordFn, loc, begin, self.pos),
            "extern" => Token::new(TokenKind::KeywordExtern, loc, begin, self.pos),
            "if" => Token::new(TokenKind::KeywordIf, loc, begin, self.pos),
            "else" => Token::new(TokenKind::KeywordElse, loc, begin, self.pos),
            "while" => Token::new(TokenKind::KeywordWhile, loc, begin, self.pos),
            "true" => Token::new(TokenKind::KeywordTrue, loc, begin, self.pos),
            "false" => Token::new(TokenKind::KeywordFalse, loc, begin, self.pos),
            "cast" => Token::new(TokenKind::KeywordCast, loc, begin, self.pos),
            "syscall" => Token::new(TokenKind::KeywordSyscall, loc, begin, self.pos),
            "return" => Token::new(TokenKind::KeywordReturn, loc, begin, self.pos),
            _ => Token::new(TokenKind::Identifier, loc, begin, self.pos),
        }
    }

    fn lex_number(&mut self, begin: usize) -> Token {
        let loc = self.loc();
        let mut kind = TokenKind::Integer;

        while let b'0'..=b'9' = self.read_char() {
            self.advance();
        }

        let suffix_start = self.pos;
        let suffix = self.peek_suffix();

        match suffix.as_deref() {
            Some("i32") => {
                kind = TokenKind::IntegerNumber;
                self.advance_n(3);
            }
            Some("u32") => {
                kind = TokenKind::UnsignedIntegerNumber;
                self.advance_n(3);
            }
            Some("i64") => {
                kind = TokenKind::LongIntegerNumber;
                self.advance_n(3);
            }
            Some("u64") => {
                kind = TokenKind::LongUnsignedIntegerNumber;
                self.advance_n(3);
            }
            Some(_) => {
                return Token::new(TokenKind::Invalid, loc, begin, self.pos);
            }
            None => (),
        }

        Token::new(kind, loc, begin, suffix_start)
    }

    // TODO: Fix the lexer. `lex_string` should handle escape sequences or RawModule should handle escape sequences
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
                b'\0' => return Token::new(TokenKind::Invalid, loc, begin, self.pos),
                b'\\' => {
                    self.advance();
                    let ch = self.read_char();
                    match ch {
                        b'r' => buffer.push('\r'),
                        b'n' => buffer.push('\n'),
                        b'\\' => buffer.push('\\'),
                        b'0' => buffer.push('\0'),
                        _ => return Token::new(TokenKind::Invalid, loc, begin, self.pos),
                    }
                }
                _ => buffer.push(ch as char),
            }
            self.advance();
        }
        Token::new(kind, loc, begin + 1, self.pos - 1)
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }

    fn lex_macro(&mut self, begin: usize) -> Token {
        let mut buffer = String::new();
        let mut kind = TokenKind::Macro;
        let loc = self.loc();
        loop {
            let ch = self.read_char();
            match ch {
                _ if ch.is_ascii_whitespace() => {
                    self.advance();
                    break;
                }
                b'(' => {
                    kind = TokenKind::MacroWithArgs;
                    self.advance();
                    loop {
                        let ch = self.read_char();
                        if ch == b')' {
                            self.advance();
                            break;
                        }
                        self.advance();
                    }
                    break;
                }
                b'\0' => return Token::new(TokenKind::Invalid, loc, begin, self.pos),
                _ => buffer.push(ch as char),
            }
            self.advance();
        }
        Token::new(kind, loc, begin + 1, self.pos)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Span<T> {
    _marker: PhantomData<T>,
    pub loc: Loc,
    pub start: usize,
    pub end: usize,
}

impl<T> Span<T> {
    pub fn to_span<E>(self) -> Span<E> {
        Span { _marker: PhantomData, loc: self.loc, start: self.start, end: self.end }
    }
}

impl<T> Span<T> {
    pub fn new(loc: Loc, start: usize, end: usize) -> Self {
        Self { _marker: PhantomData, loc, start, end }
    }
}

impl<T> From<Token> for Span<T> {
    fn from(value: Token) -> Self {
        Span {
            _marker: PhantomData,
            loc: value.loc,
            start: value.source.start,
            end: value.source.end,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub source: Span<String>,
}

impl Token {
    pub fn new(kind: TokenKind, loc: Loc, start: usize, end: usize) -> Self {
        Self {
            kind,
            loc,
            source: Span {
                _marker: PhantomData,
                loc,
                start,
                end,
            },
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
    OpenBrace,
    CloseBrace,

    Macro,
    MacroWithArgs,

    Identifier,
    Keyword,
    KeywordFn,
    KeywordEnd,
    KeywordExtern,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordTrue,
    KeywordFalse,
    KeywordCast,
    KeywordSyscall,
    KeywordReturn,

    RealNumber,
    Integer,
    IntegerNumber,
    LongIntegerNumber,
    UnsignedIntegerNumber,
    LongUnsignedIntegerNumber,
    StringLiteral,
    CharacterLiteral,

    Dot,
    Splat,
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

    Dollar,
    Deref,
}
impl TokenKind {
    pub fn is_binop(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Assign
                | Plus
                | Minus
                | Asterisk
                | Slash
                | Eq
                | NotEq
                | Gt
                | Lt
                | Mod
                | Ampersand
                | Pipe
                | DoubleAmpersand
                | DoublePipe
        )
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
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
        assert!(lex.next_token().kind == TokenKind::KeywordFn);
        assert!(lex.next_token().kind == TokenKind::Identifier);
        assert!(lex.next_token().kind == TokenKind::OpenParen);
        assert!(lex.next_token().kind == TokenKind::CloseParen);
        assert!(lex.next_token().kind == TokenKind::KeywordEnd);
    }

    #[test]
    fn test_lexer_numbers() {
        let source = "9000000000000000000u64";
        let mut lex = Lexer::new(source);
        assert!(lex.next_token().kind == TokenKind::LongUnsignedIntegerNumber);
    }
}
