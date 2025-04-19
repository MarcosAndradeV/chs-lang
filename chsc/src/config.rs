
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
/// Configuration for the chsc tool.
pub struct Config {
    pub stdlib_path: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            stdlib_path: std::env::var("CHS_STDLIB_PATH").unwrap_or("stdlib".to_string()),
        }
    }
}
