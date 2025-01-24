#![allow(unused)]

use std::{env, process::{exit, ExitCode}};

use cli::{usage, COMMANDS};
mod cli;

fn main() -> ExitCode {
    let mut args = env::args();
    let program = args.next().expect("Program always provided.");
    if let Some(cmd) = args.next() {
        if let Some(cmd) = COMMANDS.iter().find(|c| c.name == cmd) {
           (cmd.run)(&program, &mut args)
        } else {
            println!("Invalid command.");
            usage(&program);
            ExitCode::FAILURE
        }
    } else {
        println!("Expect command.");
        usage(&program);
        ExitCode::FAILURE
    }
}
