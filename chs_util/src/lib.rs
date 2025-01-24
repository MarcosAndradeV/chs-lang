use core::fmt;
use std::path::PathBuf;
pub struct CHSError(pub String);
pub type CHSResult<T> = Result<T, CHSError>;

impl fmt::Debug for CHSError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CHSError").field(&self.0).finish()
    }
}

impl fmt::Display for CHSError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ERROR: {}", self.0)
    }
}

#[macro_export]
macro_rules! chs_error {
    ($message: expr, $($field: expr),*) => {
        return Err(chs_util::CHSError (format!($message, $($field),*)))
    };

    ($message: expr) => {
        return Err(chs_util::CHSError ($message.to_string()))
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialEq, PartialOrd, Default)]
pub struct Loc {
    pub filepath: PathBuf,
    line: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filepath.display(), self.line, self.col)
    }
}

impl Loc {
    pub fn new(filepath: PathBuf, line: usize, col: usize) -> Self {
        Self {
            filepath,
            line,
            col,
        }
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
