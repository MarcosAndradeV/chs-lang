use core::fmt;
use std::{path::Path, time::SystemTime};
pub struct CHSError(pub String);
pub type CHSResult<T> = Result<T, CHSError>;

impl fmt::Debug for CHSError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CHSError").field(&self.0).finish()
    }
}

impl fmt::Display for CHSError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[macro_export]
macro_rules! return_chs_error {
    ($message: expr, $($field: expr),*) => {
        return Err(CHSError (format!($message, $($field),*)))
    };

    ($message: expr) => {
        return Err(CHSError ($message.to_string()))
    }
}

#[macro_export]
macro_rules! chs_error {
    ($message: expr, $($field: expr),*) => {
        CHSError (format!($message, $($field),*))
    };

    ($message: expr) => {
        CHSError ($message.to_string())
    }
}

#[derive(Debug, Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, Default)]
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
            c if (c as char).is_control() => {}
            _ => self.next_column(),
        }
    }
}

pub fn binary_exists(bin: &str) -> bool {
    which::which(bin).is_ok()
}

pub fn file_changed(src: &Path, artifact: &Path) -> bool {
    use std::fs;

    match (fs::metadata(src), fs::metadata(artifact)) {
        (Ok(src_meta), Ok(art_meta)) => {
            src_meta.modified().unwrap_or(SystemTime::now())
                > art_meta.modified().unwrap_or(SystemTime::now())
        }
        _ => true, // Force rebuild if files are missing or error
    }
}
