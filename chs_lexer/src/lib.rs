use chs_util::Loc;
use std::fmt;
use std::path::PathBuf;
use std::{fs, path::Path, process::exit};
pub fn read_flie<P: AsRef<Path>>(file_path: P) -> Vec<u8> {
    match fs::read(file_path) {
        Ok(ok) => ok,
        Err(err) => {
            eprintln!("[ERROR] Cannot read file: {err}");
            exit(-1);
        }
    }
}
pub struct Lexer {
    pub data: Vec<u8>,
    pub pos: usize,
    pub loc: Loc,
    peeked: Option<Token>,
}

impl Lexer {
    pub const KEYWORDS: &'static [&'static str] = &[
        "fn", "end", "use", "set", "syscall", "if", "else", "while", "array", "cast", "const",
        "type", "true", "false",
    ];
    pub fn new(data: Vec<u8>, file_path: PathBuf) -> Self {
        Self {
            data,
            loc: Loc::new(file_path, 1, 1),
            pos: 0,
            peeked: None,
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

    pub fn get_filename(&self) -> &Path {
        self.loc.filepath.as_path()
    }

    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        self.peeked.as_ref().unwrap()
    }

    pub fn next(&mut self) -> Token {
        if self.pos >= self.data.len() {
            Token::from_u8(self.loc.clone(), TokenKind::EOF, b'\0')
        } else {
            let ch = self.advance();
            let loc = self.loc.clone();
            match ch {
                b'/' if self.read_char() == b'/' => {
                    while self.advance() != b'\n' {}
                    Token::from_string(loc, TokenKind::Comment, "//".to_string())
                }
                b'-' if self.read_char() == b'>' => {
                    self.advance();
                    Token::from_string(loc, TokenKind::Arrow, "->".to_string())
                }
                b'=' if self.read_char() == b'=' => {
                    self.advance();
                    Token::from_string(loc, TokenKind::Eq, "==".to_string())
                }
                b'!' if self.read_char() == b'=' => {
                    self.advance();
                    Token::from_string(loc, TokenKind::NotEq, "!=".to_string())
                }
                b'|' if self.read_char() == b'|' => {
                    self.advance();
                    Token::from_string(loc, TokenKind::DoublePipe, "!=".to_string())
                }

                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_identfier_or_keyword(ch),
                b'0'..=b'9' => self.lex_number(ch),
                b'"' => self.lex_string(),
                b'\'' => self.lex_char(),

                b'.' => Token::from_u8(loc, TokenKind::Dot, ch),
                b',' => Token::from_u8(loc, TokenKind::Comma, ch),
                b';' => Token::from_u8(loc, TokenKind::SemiColon, ch),
                b':' => Token::from_u8(loc, TokenKind::Colon, ch),
                b'=' => Token::from_u8(loc, TokenKind::Assign, ch),

                b'!' => Token::from_u8(loc, TokenKind::Bang, ch),
                b'+' => Token::from_u8(loc, TokenKind::Plus, ch),
                b'-' => Token::from_u8(loc, TokenKind::Minus, ch),
                b'*' => Token::from_u8(loc, TokenKind::Asterisk, ch),
                b'/' => Token::from_u8(loc, TokenKind::Slash, ch),
                b'>' => Token::from_u8(loc, TokenKind::Gt, ch),
                b'<' => Token::from_u8(loc, TokenKind::Lt, ch),
                b'%' => Token::from_u8(loc, TokenKind::Mod, ch),
                b'&' => Token::from_u8(loc, TokenKind::Ampersand, ch),
                b'|' => Token::from_u8(loc, TokenKind::Pipe, ch),

                b'(' => Token::from_u8(loc, TokenKind::ParenOpen, ch),
                b')' => Token::from_u8(loc, TokenKind::ParenClose, ch),

                b'{' => Token::from_u8(loc, TokenKind::CurlyOpen, ch),
                b'}' => Token::from_u8(loc, TokenKind::CurlyClose, ch),

                b'[' => Token::from_u8(loc, TokenKind::SquareOpen, ch),
                b']' => Token::from_u8(loc, TokenKind::SquareClose, ch),

                b'\n' => Token::new(loc, TokenKind::LineFeed, "\\n"),
                ch if ch.is_ascii_whitespace() => Token::new(loc, TokenKind::WhiteSpace, " "),
                _ => Token::from_string(
                    loc,
                    TokenKind::Invalid,
                    format!("Unknown token '{}'", ch as char),
                ),
            }
        }
    }

    fn lex_identfier_or_keyword(&mut self, ch: u8) -> Token {
        let mut buffer = String::from(ch as char);
        let mut kind = TokenKind::Identifier;
        loop {
            let ch = self.read_char();
            match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => buffer.push(ch as char),
                _ => break,
            }
            self.advance();
        }
        if Self::KEYWORDS.contains(&buffer.as_str()) {
            kind = TokenKind::Keyword;
        }
        Token::from_string(self.loc.clone(), kind, buffer)
    }

    fn lex_number(&mut self, ch: u8) -> Token {
        let mut buffer = String::from(ch as char);
        let mut kind = TokenKind::IntegerNumber;
        loop {
            let ch = self.read_char();
            match ch {
                b'0'..=b'9' => buffer.push(ch as char),
                b'.' if kind != TokenKind::RealNumber => {
                    buffer.push(ch as char);
                    kind = TokenKind::RealNumber;
                }
                _ => break,
            }
            self.advance();
        }
        Token::from_string(self.loc.clone(), kind, buffer)
    }

    fn lex_string(&mut self) -> Token {
        let mut buffer = String::new();
        let kind = TokenKind::StringLiteral;
        loop {
            let ch = self.read_char();
            match ch {
                b'"' => {
                    self.advance();
                    break;
                }
                b'\0' => {
                    return Token::new(self.loc.clone(), TokenKind::Invalid, "Unclosed string")
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
                            return Token::new(
                                self.loc.clone(),
                                TokenKind::Invalid,
                                "Unknown escape sequecence",
                            )
                        }
                    }
                }
                _ => buffer.push(ch as char),
            }
            self.advance();
        }
        Token::from_string(self.loc.clone(), kind, buffer)
    }

    fn lex_char(&mut self) -> Token {
        let mut buffer = String::new();
        let kind = TokenKind::CharacterLiteral;
        let ch = self.read_char();
        match ch {
            b'\0' => {
                return Token::new(
                    self.loc.clone(),
                    TokenKind::Invalid,
                    "Unclosed char literal",
                )
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
                        return Token::new(
                            self.loc.clone(),
                            TokenKind::Invalid,
                            "Unknown escape sequecence",
                        )
                    }
                }
            }
            _ => buffer.push(ch as char),
        }
        self.advance();
        if self.advance() != b'\'' {
            return Token::new(
                self.loc.clone(),
                TokenKind::Invalid,
                "Unclosed char literal",
            );
        }
        Token::from_string(self.loc.clone(), kind, buffer)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Loc,
    pub kind: TokenKind,
    pub value: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Keyword
            | TokenKind::IntegerNumber
            | TokenKind::RealNumber
            | TokenKind::Identifier
            | TokenKind::Invalid => write!(f, "{:?}({})", self.kind, self.value),
            TokenKind::StringLiteral => {
                write!(f, "{:?}(\"{}\")", self.kind, self.value.escape_default())
            }
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

impl Token {
    pub fn new(loc: Loc, kind: TokenKind, value: &str) -> Self {
        Self {
            loc,
            kind,
            value: value.to_string(),
        }
    }
    pub fn from_string(loc: Loc, kind: TokenKind, value: String) -> Self {
        Self { loc, kind, value }
    }
    pub fn from_u8(loc: Loc, kind: TokenKind, value: u8) -> Self {
        Self {
            loc,
            kind,
            value: (value as char).to_string(),
        }
    }

    pub fn is_invalid(&self) -> bool {
        matches!(self.kind, TokenKind::Invalid)
    }

    pub fn val_eq(&self, arg: &str) -> bool {
        self.value.as_str() == arg
    }

    pub fn is_whitespace(&self, line_feed: bool, comment: bool) -> bool {
        match self.kind {
            TokenKind::WhiteSpace => true,
            TokenKind::LineFeed if line_feed => true,
            TokenKind::Comment if comment => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    EOF,
    Invalid,
    WhiteSpace,
    LineFeed,
    Comment,

    Identifier,
    Keyword,

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

    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
}
impl TokenKind {
    pub fn is_op(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Bang | Plus
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
                | DoublePipe
                | DoubleAmpersand
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
