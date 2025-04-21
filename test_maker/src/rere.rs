/*
# Translated by Marcos V. Andrade Almeida <mastermarcos1212@hotmail.com>
# From https://github.com/tsoding/rere.py
# Copyright 2024 Alexey Kutepov <reximkut@gmail.com>

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use std::{
    fs::File, io::{self, BufRead, BufReader, Write}, path::Path, process::Command
};

fn read_blob_field(reader: &mut dyn BufRead, name: &[u8]) -> io::Result<Vec<u8>> {
    let mut line = Vec::new();
    reader.read_until(b'\n', &mut line)?;
    let field_prefix = [b":b ", name, b" "].concat();
    assert!(line.starts_with(&field_prefix));
    let size = String::from_utf8_lossy(&line[field_prefix.len()..line.len() - 1])
        .parse::<usize>()
        .unwrap();
    let mut blob = vec![0u8; size];
    reader.read_exact(&mut blob)?;
    let mut newline = [0];
    reader.read_exact(&mut newline)?;
    assert_eq!(newline[0], b'\n');
    Ok(blob)
}

fn read_int_field(reader: &mut dyn BufRead, name: &[u8]) -> io::Result<i32> {
    let mut line = Vec::new();
    reader.read_until(b'\n', &mut line)?;
    let field_prefix = [b":i ", name, b" "].concat();
    assert!(line.starts_with(&field_prefix));
    let value = String::from_utf8_lossy(&line[field_prefix.len()..line.len() - 1])
        .parse::<i32>()
        .unwrap();
    Ok(value)
}

fn write_int_field(writer: &mut dyn Write, name: &[u8], value: i32) -> io::Result<()> {
    write!(writer, ":i {} {}\n", String::from_utf8_lossy(name), value)
}

fn write_blob_field(writer: &mut dyn Write, name: &[u8], blob: &[u8]) -> io::Result<()> {
    write!(
        writer,
        ":b {} {}\n",
        String::from_utf8_lossy(name),
        blob.len()
    )?;
    writer.write_all(blob)?;
    writer.write_all(b"\n")
}

fn capture(shell: &str) -> io::Result<Snapshot> {
    println!("CAPTURING: {}", shell);
    let output = Command::new("sh").arg("-c").arg(shell).output()?;
    Ok(Snapshot {
        shell: shell.to_string(),
        returncode: output.status.code().unwrap_or(-1),
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

fn load_list(path: &Path) -> io::Result<Vec<String>> {
    BufReader::new(File::open(path)?)
        .lines()
        .collect::<Result<Vec<_>, _>>()
}

fn dump_snapshots(path: &str, snapshots: &[Snapshot]) -> io::Result<()> {
    let mut f = File::create(path)?;
    write_int_field(&mut f, b"count", snapshots.len() as i32)?;
    for s in snapshots {
        write_blob_field(&mut f, b"shell", s.shell.as_bytes())?;
        write_int_field(&mut f, b"returncode", s.returncode)?;
        write_blob_field(&mut f, b"stdout", &s.stdout)?;
        write_blob_field(&mut f, b"stderr", &s.stderr)?;
    }
    Ok(())
}

fn load_snapshots(path: &str) -> io::Result<Vec<Snapshot>> {
    let mut f = BufReader::new(File::open(path)?);
    let count = read_int_field(&mut f, b"count")?;
    let mut snapshots = Vec::new();
    for _ in 0..count {
        let shell = read_blob_field(&mut f, b"shell")?;
        let returncode = read_int_field(&mut f, b"returncode")?;
        let stdout = read_blob_field(&mut f, b"stdout")?;
        let stderr = read_blob_field(&mut f, b"stderr")?;
        snapshots.push(Snapshot {
            shell: String::from_utf8(shell).unwrap(),
            returncode,
            stdout,
            stderr,
        });
    }
    Ok(snapshots)
}

#[derive(Debug)]
struct Snapshot {
    shell: String,
    returncode: i32,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

pub fn record(test_list:&Path) -> Result<(), io::Error> {
    let shells = load_list(test_list)?;
    let snapshots = shells
        .iter()
        .map(|s| capture(s))
        .collect::<Result<Vec<_>, _>>()?;
    dump_snapshots(&format!("{}.bi", test_list.display()), &snapshots)?;
    Ok(())
}

pub fn replay(test_list: &Path) -> Result<(), io::Error> {
    let shells = load_list(&test_list)?;
    let snapshots = load_snapshots(&format!("{}.bi", test_list.display()))?;
    if shells.len() != snapshots.len() {
        eprintln!("UNEXPECTED: Amount of shell commands in {}", test_list.display());
        eprintln!("    EXPECTED: {}", snapshots.len());
        eprintln!("    ACTUAL:   {}", shells.len());
        eprintln!(
            "NOTE: You may want to do record {} to update {}.bi",
            test_list.display(), test_list.display()
        );
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "mismatched shell count",
        ));
    }
    for (shell, snapshot) in shells.iter().zip(snapshots.iter()) {
        println!("REPLAYING: {}", shell);
        if shell != &snapshot.shell {
            eprintln!("UNEXPECTED: shell command");
            eprintln!("    EXPECTED: {}", snapshot.shell);
            eprintln!("    ACTUAL:   {}", shell);
            return Err(io::Error::new(io::ErrorKind::Other, "shell mismatch"));
        }
        let output = Command::new("sh").arg("-c").arg(shell).output()?;
        let mut failed = false;

        if output.status.code().unwrap_or(-1) != snapshot.returncode {
            eprintln!("UNEXPECTED: return code");
            eprintln!("    EXPECTED: {}", snapshot.returncode);
            eprintln!("    ACTUAL:   {}", output.status.code().unwrap_or(-1));
            failed = true;
        }
        if output.stdout != snapshot.stdout {
            eprintln!("UNEXPECTED: stdout");
            print_diff("expected", "actual", &snapshot.stdout, &output.stdout);
            failed = true;
        }
        if output.stderr != snapshot.stderr {
            eprintln!("UNEXPECTED: stderr");
            print_diff("expected", "actual", &snapshot.stderr, &output.stderr);
            failed = true;
        }
        if failed {
            return Err(io::Error::new(io::ErrorKind::Other, "mismatch"));
        }
    }
    println!("OK");
    Ok(())
}

fn print_diff(from: &str, to: &str, expected: &[u8], actual: &[u8]) {
    let expected = String::from_utf8_lossy(expected);
    let expected_lines: Vec<_> = expected.lines().collect();
    let actual = String::from_utf8_lossy(actual);
    let actual_lines: Vec<_> = actual.lines().collect();
    let max_len = expected_lines.len().max(actual_lines.len());

    println!("--- {}", from);
    println!("+++ {}", to);

    for i in 0..max_len {
        let expected_line = expected_lines.get(i);
        let actual_line = actual_lines.get(i);

        match (expected_line, actual_line) {
            (Some(e), Some(a)) if e != a => {
                println!("-{}", e);
                println!("+{}", a);
            }
            (Some(e), None) => {
                println!("-{}", e);
            }
            (None, Some(a)) => {
                println!("+{}", a);
            }
            _ => {}
        }
    }
}
