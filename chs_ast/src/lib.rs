use std::{fs, path::PathBuf};

use chs_lexer::Lexer;
use chs_util::{chs_error, CHSResult};
use parser::Parser;
use nodes::Module;

pub mod nodes;
pub mod parser;


pub fn parse_file(file_path: String) -> CHSResult<Module> {
    match fs::read(&file_path) {
        Ok(input) => Parser::new(Lexer::new(PathBuf::from(file_path), input)).parse(),
        Err(err) => chs_error!("{}", err)
    }
}


#[cfg(test)]
mod tests {
    use nodes::TypedModule;

    use super::*;

    #[test]
    fn check_simple_file() {
        match Parser::new(Lexer::new(
            file!().into(),
            r#"
                type v void
                type print_int fn(int) -> v
                fn main()
                    print_int(100)
                end
            "#
            .into(),
        ))
        .parse()
        {
            Ok(m) => {
                let res = TypedModule::from_module(m);
                assert!(res.is_ok(), "{}", res.unwrap_err());
            }
            Err(err) => {
                assert!(false, "{err}");
            }
        }
    }
}
