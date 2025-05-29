use std::{
    fs::{self, File},
    io::Write,
    path::{self, Path, PathBuf},
    process,
};

use clap::{Arg, ArgAction, Command};

mod rere;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = Command::new("test_maker")
        .version(VERSION)
        .author("Marcos V. Andrade Almeida <mastermarcos1212@hotmail.com>")
        .about("Test recording and replay CLI tool")
        .arg(
            Arg::new("test-folder")
                .long("test-folder")
                .value_name("FOLDER")
                .help("Set test folder (default: ./tests)")
                .required(false),
        )
        .arg(
            Arg::new("test-file")
                .long("test-file")
                .value_name("FILE")
                .help("Switch to single file mode")
                .required(false),
        )
        .arg(
            Arg::new("record")
                .long("record")
                .short('r')
                .help("Record tests")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("replay")
                .long("replay")
                .short('p')
                .help("Replay tests")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("write")
                .long("write")
                .short('w')
                .help("Write tests")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("reset")
                .long("reset")
                .help("Reset tests")
                .action(ArgAction::SetTrue),
        )
        .get_matches();

    let mut test_file = false;
    let test_path = if let Some(file) = matches.get_one::<String>("test-file") {
        test_file = true;
        PathBuf::from(file)
    } else if let Some(folder) = matches.get_one::<String>("test-folder") {
        PathBuf::from(folder)
    } else {
        PathBuf::from("tests")
    };

    if !test_path.exists() {
        return Err("[ERROR] Specified test path does not exist".into());
    }

    match (
        matches.get_flag("record"),
        matches.get_flag("replay"),
        matches.get_flag("write"),
        matches.get_flag("reset"),
    ) {
        (true, false, false, false) => {
            if test_file {
                record_test(&test_path.with_extension("list"));
            } else {
                record_tests(&test_path);
            }
        }
        (false, true, false, false) => {
            if test_file {
                replay_test(&test_path.with_extension("list"));
            } else {
                replay_tests(&test_path);
            }
        }
        (false, false, true, false) => {
            if test_file {
                write_test(&test_path.with_extension("chs"));
            } else {
                write_tests(&test_path);
            }
        }
        (false, false, false, true) => {
            if test_file {
                reset_test(&test_path.with_extension("bi"));
            } else {
                reset_tests(&test_path);
            }
        }
        (false, false, false, false) => {
            eprintln!("[ERROR] No task provided");
            usage();
            return Err("Missing task".into());
        }
        _ => {
            eprintln!("[ERROR] You can only specify one task at a time.");
            usage();
            return Err("Multiple tasks specified".into());
        }
    }

    Ok(())
}

fn usage() {
    println!("Usage: test_maker [OPTIONS] <TASK>");
    println!("Options:");
    println!("  --test-folder <FOLDER> Set test folder. Default: ./tests");
    println!("  --test-file <FILE>     Switch to single file mode");
    println!("Tasks:");
    println!("  --record, -r           Record tests");
    println!("  --replay, -p           Replay tests");
    println!("  --write, -w            Write tests");
    println!("  --reset                Reset tests");
    println!("  --help, -h             Print this help message");
}

fn reset_tests(test_folder: &path::PathBuf) {
    println!("[INFO] Reading tests directory.");
    for entry in read_dir_safe(test_folder) {
        let file_path = entry.path();
        if has_extension(&file_path, "bi") {
            reset_test(&file_path);
        }
    }
}

#[inline]
fn reset_test(file_path: &Path) {
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        println!("[INFO] Removing test file: {}", file_path.display());
        let res = fs::remove_file(file_path);
        if res.is_err() {
            eprintln!("[ERROR] Failed to remove test file: {}", res.err().unwrap());
            process::exit(1);
        }
    }
}

fn replay_tests(test_folder: &Path) {
    for entry in read_dir_safe(test_folder) {
        let file_path = entry.path();
        if has_extension(&file_path, "list") {
            replay_test(&file_path);
        }
    }
}

#[inline]
fn replay_test(file_path: &Path) {
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        match rere::replay(file_path) {
            Ok(_) => {
                // NOTE We need to remove the executable
                // let remove_file = fs::remove_file(file_path.with_extension(""));
                // if remove_file.is_err() {
                //     eprintln!(
                //         "[ERROR] Failed to remove test file `{}`:\n {}",
                //         file_path.display(),
                //         remove_file.err().unwrap()
                //     );
                //     process::exit(1);
                // }
                // process::exit(0);
            }
            Err(err) => {
                eprintln!(
                    "[ERROR] Failed to replay test file `{}` :\n{}",
                    file_path.display(),
                    err
                );
                process::exit(1);
            }
        }
    }
}

fn record_tests(test_folder: &Path) {
    for entry in read_dir_safe(test_folder) {
        let file_path = entry.path();
        if has_extension(&file_path, "list") {
            record_test(&file_path);
        }
    }
}

#[inline]
fn record_test(file_path: &Path) {
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        match rere::record(file_path) {
            Ok(_) => {
                // NOTE We need to remove the executable
                // let remove_file = fs::remove_file(file_path.with_extension(""));
                // if remove_file.is_err() {
                //     eprintln!(
                //         "[ERROR] Failed to remove test file `{}`:\n {}",
                //         file_path.display(),
                //         remove_file.err().unwrap()
                //     );
                //     process::exit(1);
                // }
                // process::exit(0);
            }
            Err(err) => {
                eprintln!(
                    "[ERROR] Failed to record test file `{}`:\n {}",
                    file_path.display(),
                    err
                );
                process::exit(1);
            }
        };
    }
}

fn write_tests(test_folder: &Path) {
    println!("[INFO] Reading tests directory.");
    for entry in read_dir_safe(test_folder) {
        let file_path = entry.path();
        if has_extension(&file_path, "chs") {
            write_test(&file_path);
        }
    }
}

#[inline]
fn write_test(file_path: &Path) {
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        let res = create_test_file(&file_path);
        let mut f = match res {
            Ok(file) => file,
            Err(_) => return,
        };
        println!("[INFO] Writing test file: {}", file_path.display());
        let res = write_tests_to_file(&mut f, &file_path);
        if res.is_err() {
            process::exit(1);
        }
    }
}

fn create_test_file(file_path: &Path) -> Result<File, ()> {
    let file_path = file_path.with_extension("list");
    let mut f = File::create(file_path).map_err(|e| {
        eprintln!("[ERROR] Failed to create file: {}", e);
    })?;
    f.write_all(b"").map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    Ok(f)
}

fn write_tests_to_file(f: &mut File, test_file_path: &Path) -> Result<(), ()> {
    let run_cmd = format!(
        "cargo run -q --bin chs -- compile-run {}\n",
        test_file_path.display()
    );
    // NOTE Support OS-specific file deletion commands
    #[cfg(unix)]
    let rm_cmd = format!("rm {}\n", test_file_path.with_extension("").display());
    #[cfg(windows)]
    let rm_cmd = format!("del {}\n", test_file_path.with_extension("").display());
    f.write_all(run_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    f.write_all(rm_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    Ok(())
}

fn read_dir_safe(path: &Path) -> Vec<std::fs::DirEntry> {
    fs::read_dir(path)
        .unwrap_or_else(|e| {
            eprintln!("[ERROR] Failed to read directory {}: {}", path.display(), e);
            process::exit(1);
        })
        .filter_map(|entry| match entry {
            Ok(e) => Some(e),
            Err(e) => {
                eprintln!("[ERROR] Failed to read entry: {}", e);
                None
            }
        })
        .collect()
}

fn has_extension(path: &Path, ext: &str) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .map(|e| e.eq_ignore_ascii_case(ext))
        .unwrap_or(false)
}
