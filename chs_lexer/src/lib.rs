use chs_util::Loc;
use core::fmt;
use std::path::PathBuf;

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
    Arrow,
    Plus,
    Slash,
    Minus,
    Eq,
    NotEq,
    Gt,
    Lt,
    Bang,
    Or,
    And,
    Mod,

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
            "fn" | "if" | "else" | "while" | "true" | "false" | "return" | "distinct" | "set"
            | "type" | "end" | "len" | "syscall" => Self::Keyword,
            _ => Self::Ident,
        }
    }
    pub fn is_eof(&self) -> bool {
        *self == TokenKind::EOF
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
            TokenKind::Or => write!(f, "Or"),
            TokenKind::And => write!(f, "And"),
            TokenKind::Mod => write!(f, "Mod"),
            TokenKind::Gt => write!(f, "Gt"),
            TokenKind::Lt => write!(f, "Lt"),
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
}

impl Lexer {
    pub fn get_filename(&self) -> PathBuf {
        self.loc.filepath.clone()
    }
    pub fn new(filepath: PathBuf, input: Vec<u8>) -> Self {
        let mut lex = Self {
            input,
            loc: Loc::new(filepath, 1, 1),
            ..Default::default()
        };
        lex.read_char();
        lex
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
                    self.make_token(And, "&&")
                } else {
                    self.make_token(Ampersand, "&")
                }
            }
            b'|' => {
                if self.peek_char() == b'|' {
                    self.read_char();
                    self.make_token(Or, "||")
                } else {
                    self.make_token(Invalid, "|")
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

#[cfg(test)]
mod tests {
    use super::*;

    struct LexTester {
        line: usize,
        col: usize,
    }
    impl LexTester {
        fn new() -> Self {
            LexTester { line: 1, col: 1 }
        }
        fn gen_token(&self, kind: TokenKind, value: &str) -> Token {
            Token {
                loc: Loc::new(file!().into(), self.line, self.col),
                kind,
                value: value.to_string(),
            }
        }
    }

    #[test]
    fn test_next_token_1() {
        use TokenKind::*;
        let input = "=+(){}".to_string();
        let tlex = LexTester::new();
        let tests = &[
            tlex.gen_token(Assign, "="),
            tlex.gen_token(Plus, "+"),
            tlex.gen_token(ParenOpen, "("),
            tlex.gen_token(ParenClose, ")"),
            tlex.gen_token(CurlyOpen, "{"),
            tlex.gen_token(CurlyClose, "}"),
            tlex.gen_token(EOF, "\0"),
        ];
        let mut lex = Lexer::new(file!().into(), input.into_bytes());
        for tt in tests {
            assert_eq!(lex.next_token().kind, tt.kind)
        }
    }

    #[test]
    fn test_next_token_2() {
        use TokenKind::*;
        let input = r#"
            x := 5;
            y := 10;
            x + y;
            "#
        .to_string();
        let tlex = LexTester::new();
        let tests = &[
            tlex.gen_token(Ident, "x"),
            tlex.gen_token(Colon, ":"),
            tlex.gen_token(Assign, "="),
            tlex.gen_token(Integer, "5"),
            tlex.gen_token(Semicolon, ";"),
            tlex.gen_token(Ident, "y"),
            tlex.gen_token(Colon, ":"),
            tlex.gen_token(Assign, "="),
            tlex.gen_token(Integer, "5"),
            tlex.gen_token(Semicolon, ";"),
            tlex.gen_token(Ident, "x"),
            tlex.gen_token(Plus, "+"),
            tlex.gen_token(Ident, "y"),
            tlex.gen_token(Semicolon, ";"),
        ];
        let mut lex = Lexer::new(file!().into(), input.into_bytes());
        for tt in tests {
            assert_eq!(lex.next_token().kind, tt.kind)
        }
    }

    #[test]
    fn test_next_token_3() {
        use TokenKind::*;
        let input = r#"
            ! - / * 5;
            "#
        .to_string();
        let tlex = LexTester::new();
        let tests = &[
            tlex.gen_token(Bang, "!"),
            tlex.gen_token(Minus, "-"),
            tlex.gen_token(Slash, "/"),
            tlex.gen_token(Asterisk, "*"),
            tlex.gen_token(Integer, "5"),
            tlex.gen_token(Semicolon, ";"),
            tlex.gen_token(EOF, "\0"),
        ];
        let mut lex = Lexer::new(file!().into(), input.into_bytes());
        for tt in tests {
            assert_eq!(lex.next_token().kind, tt.kind)
        }
    }

    #[test]
    fn test_next_token_4() {
        use TokenKind::*;
        let input = r#"
            false != true;
            "#
        .to_string();
        let tlex = LexTester::new();
        let tests = &[
            tlex.gen_token(Keyword, "false"),
            tlex.gen_token(NotEq, "!="),
            tlex.gen_token(Keyword, "true"),
            tlex.gen_token(Semicolon, ";"),
            tlex.gen_token(EOF, "\0"),
        ];
        let mut lex = Lexer::new(file!().into(), input.into_bytes());
        for tt in tests {
            assert_eq!(lex.next_token().kind, tt.kind)
        }
    }
}
