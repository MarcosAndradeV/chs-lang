use core::fmt;
use std::{ffi::OsStr, fmt::Debug, fs, path::Path, process::Command, time::SystemTime};
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
        _ => true,
    }
}

pub fn cleanup_files<P: AsRef<Path> + Debug>(paths: &[P]) {
    for temp_file in paths {
        if let Err(e) = fs::remove_file(temp_file) {
            eprintln!("[WARN] Failed to remove temp file {:?}: {}", temp_file, e);
        }
    }
}

pub fn run_exe<P: AsRef<OsStr> + Debug>(exe: P) -> CHSResult<()> {
    println!("[INFO] Running executable...");
    let output = Command::new(exe)
        .status()
        .map_err(|e| chs_error!("Failed to execute binary: {}", e))?;

    if output.success() {
        Ok(())
    } else {
        Err(chs_error!("Execution failed"))
    }
}

pub fn run_cc<P: AsRef<OsStr> + Debug>(
    compiler_flags: Vec<String>,
    asm_path: P,
    out_path: P,
) -> Result<(), CHSError> {
    let mut cc_command = Command::new("cc");
    cc_command.arg("-o").arg(out_path).arg(asm_path);
    if !compiler_flags.is_empty() {
        cc_command.args(&compiler_flags);
    }
    let output = cc_command
        .output()
        .map_err(|e| chs_error!("Failed to run cc: {}", e))?;
    Ok(if !output.status.success() {
        return_chs_error!(
            "cc failed to generate final executable\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    })
}

pub fn run_qbe<P: AsRef<OsStr> + Debug>(ssa_path: P, asm_path: P) -> Result<(), CHSError> {
    let output = Command::new("qbe")
        .arg("-o")
        .arg(asm_path)
        .arg(ssa_path)
        .output()
        .map_err(|e| chs_error!("Failed to run qbe: {}", e))?;
    Ok(if !output.status.success() {
        return_chs_error!(
            "qbe failed to generate assembly\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    })
}

pub fn run_fasm<P: AsRef<OsStr> + Debug>(src_path: P, out_path: P) -> Result<(), CHSError> {
    let output = Command::new("fasm")
        .arg(&src_path)
        .arg(out_path)
        .output()
        .map_err(|e| chs_error!("Failed to run fasm: {}", e))?;
    Ok(if !output.status.success() {
        return_chs_error!(
            "fasm failed to generate\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    })
}
