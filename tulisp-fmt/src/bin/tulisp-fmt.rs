//! `tulisp-fmt` — format Emacs Lisp / tulisp source files.
//!
//! Usage:
//!
//!   tulisp-fmt [FILE...]      format each FILE, write result to stdout
//!   tulisp-fmt -w [FILE...]   write result back to each FILE in place
//!   tulisp-fmt --check FILE…  exit 1 if any FILE differs from formatted
//!   tulisp-fmt                read from stdin, write to stdout
//!   tulisp-fmt --help         print this help

use std::fs;
use std::io::{self, Read, Write};
use std::process::ExitCode;

#[derive(Default)]
struct Args {
    write_in_place: bool,
    check: bool,
    paths: Vec<String>,
}

fn main() -> ExitCode {
    let parsed = match parse_args() {
        Ok(a) => a,
        Err(msg) => {
            eprintln!("tulisp-fmt: {msg}");
            return ExitCode::from(2);
        }
    };

    if parsed.paths.is_empty() {
        return run_stdin();
    }

    let mut differed = false;
    let mut had_error = false;
    for path in &parsed.paths {
        match run_path(path, parsed.write_in_place, parsed.check) {
            Outcome::Ok => {}
            Outcome::Differs => differed = true,
            Outcome::Error => had_error = true,
        }
    }

    if had_error {
        ExitCode::from(2)
    } else if differed {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn parse_args() -> Result<Args, String> {
    let mut out = Args::default();
    let mut after_separator = false;
    for arg in std::env::args().skip(1) {
        if after_separator {
            out.paths.push(arg);
            continue;
        }
        match arg.as_str() {
            "-h" | "--help" => {
                print_help();
                std::process::exit(0);
            }
            "-w" | "--write" => out.write_in_place = true,
            "--check" => out.check = true,
            "--" => after_separator = true,
            other if other.starts_with('-') => {
                return Err(format!("unknown option: {other}"));
            }
            _ => out.paths.push(arg),
        }
    }
    if out.write_in_place && out.check {
        return Err("--write and --check are mutually exclusive".to_string());
    }
    if (out.write_in_place || out.check) && out.paths.is_empty() {
        return Err("--write / --check require at least one FILE".to_string());
    }
    Ok(out)
}

enum Outcome {
    Ok,
    Differs,
    Error,
}

fn run_path(path: &str, write_in_place: bool, check: bool) -> Outcome {
    let src = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("tulisp-fmt: {path}: {e}");
            return Outcome::Error;
        }
    };
    let formatted = match tulisp_fmt::format(&src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("tulisp-fmt: {path}: parse error: {}", e.message);
            return Outcome::Error;
        }
    };
    if check {
        if formatted != src {
            println!("{path}");
            return Outcome::Differs;
        }
        return Outcome::Ok;
    }
    if write_in_place {
        if formatted == src {
            return Outcome::Ok;
        }
        if let Err(e) = fs::write(path, &formatted) {
            eprintln!("tulisp-fmt: {path}: {e}");
            return Outcome::Error;
        }
        return Outcome::Ok;
    }
    if let Err(e) = io::stdout().write_all(formatted.as_bytes()) {
        eprintln!("tulisp-fmt: stdout: {e}");
        return Outcome::Error;
    }
    Outcome::Ok
}

fn run_stdin() -> ExitCode {
    let mut src = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut src) {
        eprintln!("tulisp-fmt: stdin: {e}");
        return ExitCode::from(2);
    }
    match tulisp_fmt::format(&src) {
        Ok(s) => {
            if let Err(e) = io::stdout().write_all(s.as_bytes()) {
                eprintln!("tulisp-fmt: stdout: {e}");
                return ExitCode::from(2);
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("tulisp-fmt: parse error: {}", e.message);
            ExitCode::from(1)
        }
    }
}

fn print_help() {
    println!("Usage: tulisp-fmt [OPTIONS] [FILE...]");
    println!();
    println!("Format Emacs Lisp / tulisp source files.");
    println!();
    println!("With no FILE argument, reads from stdin and writes to stdout.");
    println!("With FILE arguments, formats each file and writes the result to stdout");
    println!("by default. Pass `-w` to overwrite the file in place.");
    println!();
    println!("Options:");
    println!("  -w, --write     write the formatted result back to each FILE");
    println!("      --check     exit 1 (and list filenames) if any FILE is unformatted");
    println!("  -h, --help      print this help and exit");
}
