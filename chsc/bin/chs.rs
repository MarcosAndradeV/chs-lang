#![allow(unused)]

use std::{
    env,
    process::{exit, ExitCode},
};

use cli::{usage, COMMANDS};
mod cli;

fn main() {
    let mut args = env::args();
    let program = args.next().expect("Program always provided.");
    if let Some(cmd) = args.next() {
        if let Some(cmd) = COMMANDS.iter().find(|c| c.name == cmd) {
            match (cmd.run)(&program, &mut args) {
                Err(err) => eprintln!("{err}"),
                _ => (),
            };
        } else {
            println!("Invalid command.");
            usage(&program);
        }
    } else {
        println!("Expect command.");
        usage(&program);
    }
}
