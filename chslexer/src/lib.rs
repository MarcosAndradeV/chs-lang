use std::fmt;
#[cfg(test)]
mod tests;

fn is_keyword_default(ident: &str) -> bool {
    matches!(
        ident,
        "let" | "fn" | "if" | "else" | "return" | "while" | "for"
    )
}

fn is_binop_default(op: &TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        op,
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

pub type IsKeywordFn = fn(ident: &str) -> bool;
pub type IsBinopFn = fn(op: &TokenKind) -> bool;

pub struct Lexer<'src> {
    source: &'src str,
    data: &'src [u8],
    pos: usize,
    loc: Loc,
    is_keyword_fn: IsKeywordFn,
    is_binop_fn: IsBinopFn,
}

pub struct PeekableLexer<'src> {
    pub peeked: Option<Token>,
    pub lexer: Lexer<'src>,
}

impl<'src> PeekableLexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            peeked: None,
            lexer: Lexer::new(source),
        }
    }
    pub fn next_token(&mut self) -> Token {
        if let Some(peek) = self.peeked.take() {
            peek
        } else {
            self.lexer.next_token()
        }
    }
    pub fn peek_token(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    pub fn is_binop(&self, op: &TokenKind) -> bool {
        (self.lexer.is_binop_fn)(op)
    }

    pub fn set_is_keyword_fn(&mut self, f: IsKeywordFn) {
        self.lexer.set_is_keyword_fn(f);
    }

    pub fn set_is_binop_fn(&mut self, f: IsBinopFn) {
        self.lexer.set_is_binop_fn(f);
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            data: source.as_bytes(),
            loc: Loc::new(1, 1),
            pos: 0,
            is_keyword_fn: is_keyword_default,
            is_binop_fn: is_binop_default,
        }
    }

    pub fn set_is_keyword_fn(&mut self, f: IsKeywordFn) {
        self.is_keyword_fn = f;
    }

    pub fn set_is_binop_fn(&mut self, f: IsBinopFn) {
        self.is_binop_fn = f;
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
            "u8" | "i8" | "u16" | "i16" | "i32" | "u32" | "i64" | "u64" => Some(buf),
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

            let tok = match ch {
                b'/' if self.read_char() == b'/' => {
                    while self.advance() != b'\n' {}
                    continue;
                }
                b'-' if self.read_char() == b'>' => {
                    self.advance();
                    Token::new(TokenKind::Arrow, loc, self.source[begin..self.pos].into())
                }
                b'=' if self.read_char() == b'=' => {
                    self.advance();
                    Token::new(TokenKind::Eq, loc, self.source[begin..self.pos].into())
                }
                b'!' if self.read_char() == b'=' => {
                    self.advance();
                    Token::new(TokenKind::NotEq, loc, self.source[begin..self.pos].into())
                }
                b'&' if self.read_char() == b'&' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoubleAmpersand,
                        loc,
                        self.source[begin..self.pos].into(),
                    )
                }
                b'|' if self.read_char() == b'|' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoublePipe,
                        loc,
                        self.source[begin..self.pos].into(),
                    )
                }
                b':' if self.read_char() == b':' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoubleColon,
                        loc,
                        self.source[begin..self.pos].into(),
                    )
                }
                b'.' if self.read_char() == b'.' && self.read_char() == b'.' => {
                    self.advance();
                    self.advance();
                    Token::new(TokenKind::Splat, loc, self.source[begin..self.pos].into())
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => return self.lex_identfier_or_keyword(begin),
                b'0'..=b'9' => return self.lex_number(begin),
                b'"' => return self.lex_string(begin),
                b'@' => return self.lex_macro(begin),

                b',' => Token::new(TokenKind::Comma, loc, self.source[begin..self.pos].into()),
                b';' => Token::new(
                    TokenKind::SemiColon,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b':' => Token::new(TokenKind::Colon, loc, self.source[begin..self.pos].into()),
                b'\\' => Token::new(
                    TokenKind::BackSlash,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'=' => Token::new(TokenKind::Assign, loc, self.source[begin..self.pos].into()),
                b'<' => Token::new(TokenKind::Lt, loc, self.source[begin..self.pos].into()),
                b'>' => Token::new(TokenKind::Gt, loc, self.source[begin..self.pos].into()),
                b'!' => Token::new(TokenKind::Bang, loc, self.source[begin..self.pos].into()),
                b'+' => Token::new(TokenKind::Plus, loc, self.source[begin..self.pos].into()),
                b'-' => Token::new(TokenKind::Minus, loc, self.source[begin..self.pos].into()),
                b'.' => Token::new(TokenKind::Dot, loc, self.source[begin..self.pos].into()),
                b'*' => Token::new(
                    TokenKind::Asterisk,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'/' => Token::new(TokenKind::Slash, loc, self.source[begin..self.pos].into()),
                b'%' => Token::new(TokenKind::Mod, loc, self.source[begin..self.pos].into()),
                b'$' => Token::new(TokenKind::Dollar, loc, self.source[begin..self.pos].into()),
                b'&' => Token::new(
                    TokenKind::Ampersand,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'|' => Token::new(TokenKind::Pipe, loc, self.source[begin..self.pos].into()),
                b'(' => Token::new(
                    TokenKind::OpenParen,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b')' => Token::new(
                    TokenKind::CloseParen,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'[' => Token::new(
                    TokenKind::OpenBracket,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b']' => Token::new(
                    TokenKind::CloseBracket,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'{' => Token::new(
                    TokenKind::OpenBrace,
                    loc,
                    self.source[begin..self.pos].into(),
                ),
                b'}' => Token::new(
                    TokenKind::CloseBrace,
                    loc,
                    self.source[begin..self.pos].into(),
                ),

                ch if ch.is_ascii_whitespace() => continue,
                0 => return Token::new(TokenKind::EOF, self.loc, "\0".into()),
                _ => {
                    return Token::new(
                        TokenKind::UnexpectedCharacter,
                        self.loc,
                        format!("{:?}", ch as char),
                    );
                }
            };
            return tok;
        }

        Token::new(TokenKind::EOF, self.loc, "".into())
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
        let ident = &self.source[begin..self.pos];
        let kind = if (self.is_keyword_fn)(ident) {
            TokenKind::Keyword
        } else {
            TokenKind::Identifier
        };
        Token::new(kind, loc, ident.into())
    }

    fn lex_number(&mut self, begin: usize) -> Token {
        let loc = self.loc();
        let mut kind = TokenKind::IntegerNumber;

        while let b'0'..=b'9' = self.read_char() {
            self.advance();
        }

        let suffix_start = self.pos;
        let suffix = self.peek_suffix();

        match suffix.as_deref() {
            Some("i8") => {
                kind = TokenKind::I8Number;
                self.advance_n(3);
            }
            Some("u8") => {
                kind = TokenKind::U8Number;
                self.advance_n(3);
            }
            Some("i16") => {
                kind = TokenKind::I16Number;
                self.advance_n(3);
            }
            Some("u16") => {
                kind = TokenKind::U16Number;
                self.advance_n(3);
            }
            Some("i32") => {
                kind = TokenKind::I32Number;
                self.advance_n(3);
            }
            Some("u32") => {
                kind = TokenKind::U32Number;
                self.advance_n(3);
            }
            Some("i64") => {
                kind = TokenKind::I64Number;
                self.advance_n(3);
            }
            Some("u64") => {
                kind = TokenKind::U64Number;
                self.advance_n(3);
            }
            Some(_) => {
                kind = TokenKind::UnknowIntergerLiteral;
            }
            None => (),
        }

        Token::new(kind, loc, self.source[begin..suffix_start].into())
    }

    fn lex_string(&mut self, _begin: usize) -> Token {
        let mut buffer = String::new();
        let loc = self.loc();
        loop {
            let ch = self.read_char();
            match ch {
                b'"' => {
                    self.advance();
                    break;
                }
                b'\0' => {
                    return Token::new(
                        TokenKind::UnterminatedStringLiteral,
                        loc,
                        "unterminated string literal".into(),
                    );
                }
                b'\\' => {
                    self.advance();
                    let esc = self.read_char();
                    match esc {
                        b'r' => buffer.push('\r'),
                        b'n' => buffer.push('\n'),
                        b'"' => buffer.push('"'),
                        b'\'' => buffer.push('\''),
                        b'\\' => buffer.push('\\'),
                        b'0' => buffer.push('\0'),
                        _ => {
                            return Token::new(
                                TokenKind::InvalidEscapeSequence,
                                loc,
                                format!("invalid escape sequence: \\{}", esc as char),
                            );
                        }
                    }
                }
                _ => buffer.push(ch as char),
            }
            self.advance();
        }

        Token::new(TokenKind::StringLiteral, loc, buffer)
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }

    fn lex_macro(&mut self, _begin: usize) -> Token {
        let mut buffer = String::new();
        let mut kind = TokenKind::MacroCall;
        let loc = self.loc();

        loop {
            let ch = self.read_char();
            match ch {
                _ if ch.is_ascii_whitespace() => {
                    self.advance();
                    break;
                }
                b'(' => {
                    kind = TokenKind::MacroCallWithArgs;
                    buffer.push('(');
                    self.advance();
                    let mut depth = 1;

                    while depth > 0 {
                        let ch = self.read_char();
                        match ch {
                            b'\0' => {
                                return Token::new(
                                    TokenKind::UnterminatedMacroArguments,
                                    loc,
                                    buffer,
                                );
                            }
                            b'(' => {
                                depth += 1;
                            }
                            b')' => {
                                depth -= 1;
                            }
                            _ => {}
                        }
                        buffer.push(ch as char);
                        self.advance();
                    }
                    break;
                }
                b'\0' => {
                    return Token::new(TokenKind::UnterminatedMacro, loc, buffer);
                }
                _ => buffer.push(ch as char),
            }
            self.advance();
        }

        Token::new(kind, loc, buffer)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Span {
    pub loc: Loc,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(loc: Loc, start: usize, end: usize) -> Self {
        Self { loc, start, end }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub source: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Invalid => write!(f, "Invalid({})", self.source),
            TokenKind::UnexpectedCharacter => write!(f, "Unexpected Character `{}`", self.source),
            TokenKind::UnterminatedMacroArguments => todo!(),
            TokenKind::UnterminatedMacro => todo!(),
            TokenKind::InvalidEscapeSequence => todo!(),
            TokenKind::UnterminatedStringLiteral => todo!(),
            TokenKind::UnknowIntergerLiteral => todo!(),
            TokenKind::MacroCall | TokenKind::MacroCallWithArgs => {
                write!(f, "Macro({})", self.source)
            }
            TokenKind::Keyword => write!(f, "Keyword({})", self.source),
            TokenKind::StringLiteral => write!(f, "{:?}", self.source),
            TokenKind::CharacterLiteral => write!(f, "{:?}", self.source),
            _ => write!(f, "{}", self.source),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, loc: Loc, source: String) -> Self {
        Self { kind, loc, source }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }

    pub fn is_invalid(&self) -> bool {
        matches!(self.kind, TokenKind::Invalid)
    }

    pub fn is_macro(&self) -> bool {
        matches!(self.kind, TokenKind::MacroCall)
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, TokenKind::Identifier)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    #[default]
    EOF,
    Invalid,
    UnexpectedCharacter,
    UnterminatedMacroArguments,
    UnterminatedMacro,
    InvalidEscapeSequence,
    UnterminatedStringLiteral,
    UnknowIntergerLiteral,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    MacroCall,
    MacroCallWithArgs,

    Identifier,
    Keyword,

    RealNumber,
    IntegerNumber,
    U8Number,
    I8Number,
    U16Number,
    I16Number,
    U32Number,
    I32Number,
    U64Number,
    I64Number,
    StringLiteral,
    CharacterLiteral,

    Dot,
    Splat,
    Comma,
    Colon,
    DoubleColon,
    SemiColon,
    Arrow,
    BackSlash,

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
