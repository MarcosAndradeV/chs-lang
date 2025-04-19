use std::{
    fs::{self, File},
    io::Write,
    path::{self, Path},
    process::{self, Command},
};

const RERE_PATH: &str = "./rere.py";
fn main() {
    const TESTS_PATH: &str = "tests";
    let mut test_path = path::PathBuf::from(TESTS_PATH);
    if !test_path.exists() {
        eprintln!("[ERROR] Tests directory does not exist");
        process::exit(1);
    }
    let mut test_file: bool = false;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                usage();
                process::exit(0);
            }
            "--test-file" => {
                test_file = true;
                let test_file_path = args.next().unwrap_or_else(|| {
                    eprintln!("[ERROR] Missing argument for --test-folder");
                    process::exit(1);
                });
                test_path = path::PathBuf::from(test_file_path);
                if !test_path.exists() {
                    eprintln!("[ERROR] Test file does not exist");
                    process::exit(1);
                }
            }
            "--record" | "-r" => {
                if test_file {
                    record_test(&test_path);
                } else {
                    record_tests(&test_path);
                }
                process::exit(0);
            }
            "--replay" | "-p" => {
                if test_file {
                    replay_test(&test_path);
                } else {
                    replay_tests(&test_path);
                }
                process::exit(0);
            }
            "--reset" => {
                if test_file {
                    reset_test(&test_path);
                } else {
                    reset_tests(&test_path);
                }
                process::exit(0);
            }
            "--write" | "-w" => {
                if test_file {
                    write_test(&test_path);
                } else {
                    write_tests(&test_path);
                }
                process::exit(0);
            }
            "--test-folder" => {
                let test_folder_path = args.next().unwrap_or_else(|| {
                    eprintln!("[ERROR] Missing argument for --test-folder");
                    process::exit(1);
                });
                test_path = path::PathBuf::from(test_folder_path);
                if !test_path.exists() {
                    eprintln!("[ERROR] Test folder does not exist");
                    process::exit(1);
                }
            }
            _ => {
                println!("[ERROR] Unknown option {}", arg);
                usage();
                process::exit(1);
            }
        }
    }

    println!("[ERROR] No task provided");
    usage();
    process::exit(1);
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
    for entry in fs::read_dir(test_folder).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        reset_test(&file_path);
    }
}

#[inline]
fn reset_test(file_path: &Path) {
    let file_path = file_path.with_extension("list.bi");
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
    for entry in fs::read_dir(test_folder).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        replay_test(&file_path);
    }
}

#[inline]
fn replay_test(file_path: &Path) {
    let file_path = file_path.with_extension("list");
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        let output = Command::new(RERE_PATH)
            .arg("replay")
            .arg(&file_path)
            .output()
            .expect("Failed to execute command");
        println!("[INFO] Replay test file: {}.bi", file_path.display());
        if !output.status.success() {
            println!(
                "[ERROR] Failed to record test file: {}",
                file_path.display()
            );
            print!("[INFO] stdout: {}", String::from_utf8_lossy(&output.stdout));
            print!("[INFO] stderr: {}", String::from_utf8_lossy(&output.stderr));
        } else {
            print!(
                "[INFO] stdout:\n{}",
                String::from_utf8_lossy(&output.stdout)
            );
        }
    }
}

fn record_tests(test_folder: &Path) {
    for entry in fs::read_dir(test_folder).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        record_test(&file_path);
    }
}

#[inline]
fn record_test(file_path: &Path) {
    let file_path = file_path.with_extension("list");
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        let output = Command::new(RERE_PATH)
            .arg("record")
            .arg(&file_path)
            .output()
            .expect("Failed to execute command");
        println!("[INFO] Recorded test file: {}.bi", file_path.display());
        if !output.status.success() {
            println!(
                "[ERROR] Failed to record test file: {}",
                file_path.display()
            );
            print!("[INFO] stdout: {}", String::from_utf8_lossy(&output.stdout));
            print!("[INFO] stderr: {}", String::from_utf8_lossy(&output.stderr));
        } else {
            print!(
                "[INFO] stdout:\n{}",
                String::from_utf8_lossy(&output.stdout)
            );
        }
    }
}

fn write_tests(test_folder: &Path) {
    println!("[INFO] Reading tests directory.");
    for entry in fs::read_dir(test_folder).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        if file_path.is_file() && file_path.extension().unwrap() == "chs" {
            write_test(&file_path);
        }
    }
}

#[inline]
fn write_test(file_path: &Path) {
    if file_path.is_file() {
        println!("[INFO] Found test file: {}", file_path.display());
        let res = create_test_file(&file_path);
        if res.is_err() {
            process::exit(1);
        }
        let mut f = res.unwrap();
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
        "cargo run -q --bin chs -- compile-run {} --force\n",
        test_file_path.display()
    );
    let rm_cmd = format!("rm {}\n", test_file_path.with_extension("").display());
    f.write_all(run_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    f.write_all(rm_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    Ok(())
}
