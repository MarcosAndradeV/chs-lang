use std::{
    fs::{self, File},
    io::Write,
    path::{self, Path},
    process::{self, Command},
};

const TESTS_PATH: &str = "tests";
fn main() {
    if !path::Path::new(TESTS_PATH).exists() {
        eprintln!("[ERROR] Tests directory does not exist");
        process::exit(1);
    }
    let mut args = std::env::args().skip(1);
    if let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                usage();
                process::exit(0);
            }
            "--record" | "-r" => {
                record_tests();
                process::exit(0);
            }
            "--replay" | "-p" => {
                replay_tests();
                process::exit(0);
            }
            "--write" | "-w" => {
                write_tests();
                process::exit(0);
            }
            _ => {
                println!("[ERROR] Unknown option");
                usage();
                process::exit(1);
            }
        }
    } else {
        println!("[ERROR] No option provided");
        usage();
        process::exit(1);
    }
}

fn usage() {
    println!("Usage: test_maker <TASK>");
    println!("Tasks:");
    println!("  --help, -h    Print this help message");
    println!("  --record, -r  Record tests");
    println!("  --replay, -p  Replay tests");
    println!("  --write, -w   Write tests");
}

fn replay_tests() {
    let rere = format!("{}/{}", TESTS_PATH, "rere.py");
        for entry in fs::read_dir(TESTS_PATH).unwrap() {
            let entry = entry.unwrap();
            let file_path = entry.path();
            if file_path.is_file() && file_path.extension().is_some_and(|e| e == "list") {
                println!("[INFO] Found test file: {}", file_path.display());
                let output = Command::new(&rere).arg("replay").arg(&file_path)
                    .output().expect("Failed to execute command");
                println!("[INFO] Replay test file: {}.bi", file_path.display());
                if !output.status.success() {
                    println!("[ERROR] Failed to record test file: {}", file_path.display());
                    print!("[INFO] stdout: {}", String::from_utf8_lossy(&output.stdout));
                    print!("[INFO] stderr: {}", String::from_utf8_lossy(&output.stderr));
                } else {
                    print!("[INFO] stdout:\n{}", String::from_utf8_lossy(&output.stdout));
                }
            }
        }
}

fn record_tests() {
    let rere = format!("{}/{}", TESTS_PATH, "rere.py");
    for entry in fs::read_dir(TESTS_PATH).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        if file_path.is_file() && file_path.extension().is_some_and(|e| e == "list") {
            println!("[INFO] Found test file: {}", file_path.display());
            let output = Command::new(&rere).arg("record").arg(&file_path)
                .output().expect("Failed to execute command");
            println!("[INFO] Recorded test file: {}.bi", file_path.display());
            if !output.status.success() {
                println!("[ERROR] Failed to record test file: {}", file_path.display());
                print!("[INFO] stdout: {}", String::from_utf8_lossy(&output.stdout));
                print!("[INFO] stderr: {}", String::from_utf8_lossy(&output.stderr));
            } else {
                print!("[INFO] stdout:\n{}", String::from_utf8_lossy(&output.stdout));
            }
        }
    }
}

fn write_tests() {
    println!("[INFO] Reading tests directory.");
    for entry in fs::read_dir(TESTS_PATH).unwrap() {
        let entry = entry.unwrap();
        let file_path = entry.path();
        if file_path.is_file() && file_path.extension().is_some_and(|e| e == "chs") {
            println!("[INFO] Found test file: {}", file_path.display());
            let res = create_test_file_if_not_exists(&file_path);
            if res.is_err() {
                process::exit(1);
            }
            if let Some(mut f) = res.unwrap() {
                println!("[INFO] Writing test file: {}", file_path.display());
                let res = write_tests_to_file(&mut f, &file_path);
                if res.is_err()  {
                    process::exit(1);
                }
            }
        }
    }
}

fn create_test_file_if_not_exists(file_path: &Path) -> Result<Option<File>, ()> {
    let file_path = file_path.with_extension("list");
    if !file_path.exists() {
        let mut f = File::create(file_path).map_err(|e| {
            eprintln!("[ERROR] Failed to create file: {}", e);
        })?;
        f.write_all(b"").map_err(|e| {
            eprintln!("[ERROR] Failed to write to file: {}", e);
        })?;
        Ok(Some(f))
    } else {
        Ok(None)
    }
}

fn write_tests_to_file(f: &mut File, test_file_path: &Path) -> Result<(), ()> {
    let run_cmd = format!("cargo run -q --bin chs -- compile {} -r -s\n", test_file_path.display());
    let rm_cmd = format!("rm {}\n", test_file_path.with_extension("").display());
    f.write_all(run_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    f.write_all(rm_cmd.as_bytes()).map_err(|e| {
        eprintln!("[ERROR] Failed to write to file: {}", e);
    })?;
    Ok(())
}
