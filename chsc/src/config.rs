use std::{fmt, fs::File};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
/// Configuration for the chsc tool.
pub struct Config {
    pub stdlib_path: String,
    pub version: &'static str,
    pub target: Target,
}

impl Config {
    pub fn write_to_file(&self, path: &str) -> std::io::Result<()> {
        use std::io::Write as _;
        let mut file = File::create(path)?;
        writeln!(file, "export CHS_STDLIB_PATH=\"{}\"", self.stdlib_path);
        writeln!(file, "export CHS_BACKEND=\"{}\"", self.target.backend());
        writeln!(file, "export CHS_ARCH=\"{}\"", self.target.arch());
        writeln!(file, "export CHS_OS=\"{}\"", self.target.os());
        Ok(())
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            stdlib_path: std::env::var("CHS_STDLIB_PATH").unwrap_or("stdlib".to_string()),
            version: VERSION,
            target: Target::from_env(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Target(Backend, Arch, Os);
impl Target {
    pub fn from_env() -> Self {
        let backend = std::env::var("CHS_BACKEND").unwrap_or_default();
        let arch = std::env::var("CHS_ARCH").unwrap_or_default();
        let os = std::env::var("CHS_OS").unwrap_or_default();
        Self(
            Backend::from_str_or_default(backend),
            Arch::from_str_or_default(arch),
            Os::from_str_or_default(os),
        )
    }
    pub fn backend(&self) -> &Backend {
        &self.0
    }
    pub fn arch(&self) -> &Arch {
        &self.1
    }
    pub fn os(&self) -> &Os {
        &self.2
    }
}
impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}-{}", self.0, self.1, self.2)
    }
}

#[derive(Debug, Default)]
pub enum Backend {
    #[default]
    FASM,
}

impl Backend {
    pub fn from_str_or_default(s: String) -> Self {
        match s.to_lowercase().as_str() {
            "fasm" => Self::FASM,
            _ => Self::default(),
        }
    }
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Backend::FASM => write!(f, "fasm"),
        }
    }
}

#[derive(Debug, Default)]
pub enum Arch {
    #[default]
    X86_64,
}

impl Arch {
    pub fn from_str_or_default(s: String) -> Self {
        match s.as_str() {
            "x86_64" | "amd64" => Arch::X86_64,
            _ => Self::default(),
        }
    }
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arch::X86_64 => write!(f, "x86_64"),
        }
    }
}

#[derive(Debug, Default)]
pub enum Os {
    #[default]
    LINUX,
}

impl Os {
    pub fn from_str_or_default(s: String) -> Self {
        match s.as_str() {
            "linux" => Os::LINUX,
            _ => Self::default(),
        }
    }
}

impl fmt::Display for Os {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Os::LINUX => write!(f, "linux"),
        }
    }
}
