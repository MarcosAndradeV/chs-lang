
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
/// Configuration for the chsc tool.
pub struct Config {}

impl Config {
    pub fn new() -> Self {
        Config {}
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {}
    }
}
