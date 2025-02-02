use chs_util::{CHSError, CHSResult, Loc};
use core::fmt;
use std::{fs, path::PathBuf};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Invalid,
    EOF,
    Comment,

    Integer,
    Keyword,
    Ident,
    String,

    Assign,
    Comma,
    Semicolon,
    Colon,
    Dot,
    Asterisk,
    Ampersand,
    DoubleAmpersand,
    Arrow,
    Plus,
    Slash,
    Minus,
    Mod,

    Eq,
    NotEq,
    Gt,
    Lt,

    Bang,

    Pipe,
    DoublePipe,

    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
}

impl Default for TokenKind {
    fn default() -> Self {
        Self::Invalid
    }
}

impl TokenKind {
    fn from_word_or_keyword(value: &str) -> Self {
        match value {
            "fn" | "if" | "else" | "while" | "true" | "false" | "use" | "distinct" | "set"
            | "type" | "end" | "len" | "syscall" => Self::Keyword,
            _ => Self::Ident,
        }
    }
    pub fn is_eof(&self) -> bool {
        *self == TokenKind::EOF
    }
    pub fn is_op(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Plus | Asterisk
                | Slash
                | Minus
                | Eq
                | NotEq
                | Lt
                | Gt
                | Mod
                | Pipe
                | Ampersand
                | DoublePipe
                | DoubleAmpersand
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Invalid => write!(f, "Invalid"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Ident => write!(f, "Ident"),
            TokenKind::Integer => write!(f, "Integer"),
            TokenKind::Keyword => write!(f, "Keyword"),
            TokenKind::Assign => write!(f, "Assign"),
            TokenKind::Comma => write!(f, "Comma"),
            TokenKind::Semicolon => write!(f, "Semicolon"),
            TokenKind::Colon => write!(f, "Colon"),
            TokenKind::ParenOpen => write!(f, "ParenOpen"),
            TokenKind::ParenClose => write!(f, "ParenClose"),
            TokenKind::CurlyOpen => write!(f, "CurlyOpen"),
            TokenKind::CurlyClose => write!(f, "CurlyClose"),
            TokenKind::SquareOpen => write!(f, "SquareOpen"),
            TokenKind::SquareClose => write!(f, "SquareClose"),
            TokenKind::Dot => write!(f, "Dot"),
            TokenKind::Asterisk => write!(f, "Asterisk"),
            TokenKind::Arrow => write!(f, "Arrow"),
            TokenKind::Minus => write!(f, "Minus"),
            TokenKind::Ampersand => write!(f, "Ampersand"),
            TokenKind::NotEq => write!(f, "NotEq"),
            TokenKind::Bang => write!(f, "Bang"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Plus => write!(f, "Plus"),
            TokenKind::Slash => write!(f, "Slash"),
            TokenKind::Eq => write!(f, "Eq"),
            TokenKind::Comment => write!(f, "Comment"),
            TokenKind::Mod => write!(f, "Mod"),
            TokenKind::Gt => write!(f, "Gt"),
            TokenKind::Lt => write!(f, "Lt"),
            TokenKind::DoubleAmpersand => write!(f, "DoubleAmpersand"),
            TokenKind::Pipe => write!(f, "Pipe"),
            TokenKind::DoublePipe => write!(f, "DoublePipe"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub loc: Loc,
}

impl Token {
    pub fn val_eq(&self, value: &str) -> bool {
        self.value == value
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.kind == TokenKind::String {
            write!(f, "{} {:?}(ESCAPE THE STRINGS)", self.loc, self.kind)
        } else {
            write!(f, "{} {:?}({})", self.loc, self.kind, self.value)
        }
    }
}

#[derive(Default)]
pub struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,
    loc: Loc,
    file_path: PathBuf,
}

impl Lexer {
    pub fn get_filename(&self) -> PathBuf {
        self.file_path.clone()
    }
    pub fn new(file_path: PathBuf) -> CHSResult<Self> {
        let input = fs::read(&file_path).map_err(|err| CHSError(format!("ERROR: Cannot read file {err}")))?;
        let mut lex = Self {
            input,
            loc: Loc::new(file_path.clone(), 1, 1),
            file_path,
            ..Default::default()
        };
        lex.read_char();
        Ok(lex)
    }
    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
        self.loc.next(self.ch);
    }

    fn peek_char(&mut self) -> u8 {
        if self.pos >= self.input.len() {
            0
        } else {
            self.input[self.read_pos]
        }
    }
    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;
        self.skip_whitespace();
        match self.ch {
            b'#' => {
                self.skip_comment();
                self.make_token(Comment, "")
            }
            b'-' => {
                if self.peek_char() == b'>' {
                    self.read_char();
                    self.make_token(Arrow, "->")
                } else {
                    self.make_token(Minus, "-")
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(NotEq, "!=")
                } else {
                    self.make_token(Bang, "!")
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(Invalid, "<=")
                } else {
                    self.make_token(Lt, "<")
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(Invalid, ">=")
                } else {
                    self.make_token(Gt, ">")
                }
            }
            b':' => self.make_token(Colon, ":"),
            b'.' => self.make_token(Dot, "."),
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(Eq, "==")
                } else {
                    self.make_token(Assign, "=")
                }
            }
            b'*' => self.make_token(Asterisk, "*"),
            b'/' => self.make_token(Slash, "/"),
            b'%' => self.make_token(Mod, "%"),
            b'+' => self.make_token(Plus, "+"),
            b'&' => {
                if self.peek_char() == b'&' {
                    self.read_char();
                    self.make_token(DoubleAmpersand, "&&")
                } else {
                    self.make_token(Ampersand, "&")
                }
            }
            b'|' => {
                if self.peek_char() == b'|' {
                    self.read_char();
                    self.make_token(DoublePipe, "||")
                } else {
                    self.make_token(Pipe, "|")
                }
            }
            b',' => self.make_token(Comma, ","),
            b';' => self.make_token(Semicolon, ";"),
            b'(' => self.make_token(ParenOpen, "("),
            b')' => self.make_token(ParenClose, ")"),
            b'{' => self.make_token(CurlyOpen, "{"),
            b'}' => self.make_token(CurlyClose, "}"),
            b'[' => self.make_token(SquareOpen, "["),
            b']' => self.make_token(SquareClose, "]"),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.word(),
            b'"' => self.string(),
            b'0'..=b'9' => self.number(),
            0 => self.make_token(EOF, "\0"),
            ch => self.make_token(Invalid, &format!("{}", ch as char)),
        }
    }

    fn make_token(&mut self, kind: TokenKind, value: &str) -> Token {
        let loc = self.loc.clone();
        self.read_char();
        Token {
            kind,
            value: value.into(),
            loc,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !self.ch.is_ascii_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        loop {
            self.read_char();
            if matches!(self.ch, b'\n' | b'\0') {
                break;
            }
        }
    }

    fn number(&mut self) -> Token {
        let start_pos = self.pos;
        let loc = self.loc.clone();

        loop {
            if !self.ch.is_ascii_digit() {
                break;
            }
            self.read_char();
        }
        let value: String = String::from_utf8_lossy(&self.input[start_pos..self.pos]).into();

        Token {
            kind: TokenKind::Integer,
            value,
            loc,
        }
    }

    fn string(&mut self) -> Token {
        let start_loc = self.loc.clone();
        let mut buf = String::new();
        loop {
            self.read_char();
            match self.ch {
                b'\"' => break self.read_char(),
                b'\0' => return self.make_token(TokenKind::Invalid, &buf),
                b'\\' => {
                    match self.peek_char() {
                        b'n' => buf.push('\n'),
                        b'\\' => buf.push('\\'),
                        b'0' => buf.push('\0'),
                        _ => return self.make_token(TokenKind::Invalid, &buf),
                    }
                    self.read_char();
                }
                a => buf.push(a as char),
            }
        }
        Token {
            value: buf,
            kind: TokenKind::String,
            loc: start_loc,
        }
    }

    fn word(&mut self) -> Token {
        let start_pos = self.pos;
        let loc = self.loc.clone();

        loop {
            if !matches!(self.ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') {
                break;
            }
            self.read_char();
        }
        let value: String = String::from_utf8_lossy(&self.input[start_pos..self.pos]).into();
        Token {
            kind: TokenKind::from_word_or_keyword(&value),
            value,
            loc,
        }
    }
}
