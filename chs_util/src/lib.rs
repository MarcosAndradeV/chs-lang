use core::fmt;
pub struct CHSError(pub String);

impl fmt::Debug for CHSError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CHSError").field(&self.0).finish()
    }
}

#[macro_export]
macro_rules! chs_error {
    ($message: expr, $($field: expr),*) => {
        return Err(CHSError (format!($message, $($field),*)))
    };

    ($message: expr) => {
        return Err(CHSError ($message.to_string()))
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Default)]
pub struct Loc{
    // file: &'a str,
    line: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ":{}:{}", self.line, self.col)
    }
}

impl Loc {
    pub fn new(line: usize, col: usize) -> Self {
        Self {line, col }
    }
    pub fn next_column(&self) -> Self {
        Self {
            line: self.line,
            col: self.col + 1,
        }
    }
    pub fn next_line(&self) -> Self {
        Self {
            line: self.line + 1,
            col: 1,
        }
    }
    pub fn next(&self, c: u8) -> Self {
		match c {
			b'\n' => self.next_line(),
			b'\t' => {
				let ts = 8;
				Self {
					line: self.line,
					col: (self.col / ts) * ts + ts,
				}
			}
			c if (c as char).is_control() => *self,
			_ => self.next_column()
		}
	}
}