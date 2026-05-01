//! `tulisp-fmt` — format Emacs Lisp / tulisp source files.
//!
//! Usage:
//!
//!   tulisp-fmt [FILE...]      format each FILE, write result to stdout
//!   tulisp-fmt -w [FILE...]   write result back to each FILE in place
//!   tulisp-fmt --check FILE…  exit 1 if any FILE differs from formatted
//!   tulisp-fmt                read from stdin, write to stdout
//!   tulisp-fmt --help         print this help
//!   tulisp-fmt --version      print version information

use std::ffi::OsString;
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::{SystemTime, UNIX_EPOCH};

mod diff;

struct Args {
    write_in_place: bool,
    check: bool,
    diff: bool,
    width: usize,
    paths: Vec<String>,
}

impl Default for Args {
    fn default() -> Self {
        Self {
            write_in_place: false,
            check: false,
            diff: false,
            width: tulisp_fmt::render::DEFAULT_WIDTH,
            paths: Vec::new(),
        }
    }
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
        return run_stdin(parsed.width, parsed.diff);
    }

    let mut differed = false;
    let mut had_error = false;
    for path in &parsed.paths {
        match run_path(path, &parsed) {
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
    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
        if after_separator {
            out.paths.push(arg);
            continue;
        }
        match arg.as_str() {
            "-h" | "--help" => {
                print_help();
                std::process::exit(0);
            }
            "-V" | "--version" => {
                println!("tulisp-fmt {}", env!("CARGO_PKG_VERSION"));
                std::process::exit(0);
            }
            "-w" | "--write" => out.write_in_place = true,
            "--check" => out.check = true,
            "-d" | "--diff" => out.diff = true,
            "--width" => {
                let v = iter
                    .next()
                    .ok_or_else(|| "--width requires a value".to_string())?;
                out.width = parse_width(&v)?;
            }
            s if s.starts_with("--width=") => {
                out.width = parse_width(&s["--width=".len()..])?;
            }
            "--" => after_separator = true,
            other if other.starts_with('-') => {
                return Err(format!("unknown option: {other}"));
            }
            _ => out.paths.push(arg),
        }
    }
    let mode_count =
        usize::from(out.write_in_place) + usize::from(out.check) + usize::from(out.diff);
    if mode_count > 1 {
        return Err("--write, --check, and --diff are mutually exclusive".to_string());
    }
    if (out.write_in_place || out.check) && out.paths.is_empty() {
        return Err("--write / --check require at least one FILE".to_string());
    }
    Ok(out)
}

fn parse_width(s: &str) -> Result<usize, String> {
    let n: usize = s
        .parse()
        .map_err(|_| format!("--width: not a non-negative integer: {s}"))?;
    if n < 8 {
        return Err(format!("--width: must be at least 8, got {n}"));
    }
    Ok(n)
}

enum Outcome {
    Ok,
    Differs,
    Error,
}

fn run_path(path: &str, args: &Args) -> Outcome {
    let src = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("tulisp-fmt: {path}: {e}");
            return Outcome::Error;
        }
    };
    let formatted = match tulisp_fmt::format_with_width(&src, args.width) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.render(&src, Some(path)));
            return Outcome::Error;
        }
    };
    if args.check {
        if formatted != src {
            println!("{path}");
            return Outcome::Differs;
        }
        return Outcome::Ok;
    }
    if args.diff {
        let d = diff::unified_diff(path, &src, &formatted);
        if d.is_empty() {
            return Outcome::Ok;
        }
        if let Err(e) = io::stdout().write_all(d.as_bytes()) {
            eprintln!("tulisp-fmt: stdout: {e}");
            return Outcome::Error;
        }
        return Outcome::Differs;
    }
    if args.write_in_place {
        if formatted == src {
            return Outcome::Ok;
        }
        if let Err(e) = atomic_write(path, &formatted) {
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

/// Write `contents` to `path` atomically: stage to a sibling temp
/// file, fsync it, then `rename` over the target. A crash mid-write
/// leaves the original file intact (the rename is the commit point).
/// The temp file lives in the same directory so the rename stays on
/// one filesystem and is atomic on POSIX.
fn atomic_write(path: &str, contents: &str) -> io::Result<()> {
    let target = Path::new(path);
    let dir = target.parent().filter(|p| !p.as_os_str().is_empty());
    let file_name = target.file_name().ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidInput, "path has no file name")
    })?;

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.subsec_nanos())
        .unwrap_or(0);
    let mut tmp_name = OsString::from(".");
    tmp_name.push(file_name);
    tmp_name.push(format!(".tulisp-fmt.{}.{nanos}.tmp", std::process::id()));

    let tmp_path: PathBuf = match dir {
        Some(d) => d.join(&tmp_name),
        None => PathBuf::from(&tmp_name),
    };

    let written = (|| -> io::Result<()> {
        let mut f = fs::File::create(&tmp_path)?;
        f.write_all(contents.as_bytes())?;
        f.sync_all()?;
        Ok(())
    })();
    if let Err(e) = written {
        let _ = fs::remove_file(&tmp_path);
        return Err(e);
    }
    if let Err(e) = fs::rename(&tmp_path, target) {
        let _ = fs::remove_file(&tmp_path);
        return Err(e);
    }
    Ok(())
}

fn run_stdin(width: usize, diff_mode: bool) -> ExitCode {
    let mut src = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut src) {
        eprintln!("tulisp-fmt: stdin: {e}");
        return ExitCode::from(2);
    }
    let formatted = match tulisp_fmt::format_with_width(&src, width) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.render(&src, Some("<stdin>")));
            return ExitCode::from(1);
        }
    };
    let payload = if diff_mode {
        let d = diff::unified_diff("<stdin>", &src, &formatted);
        if d.is_empty() {
            return ExitCode::SUCCESS;
        }
        d
    } else {
        formatted
    };
    if let Err(e) = io::stdout().write_all(payload.as_bytes()) {
        eprintln!("tulisp-fmt: stdout: {e}");
        return ExitCode::from(2);
    }
    if diff_mode {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
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
    println!("  -w, --write       write the formatted result back to each FILE");
    println!("      --check       exit 1 (and list filenames) if any FILE is unformatted");
    println!("  -d, --diff        print a unified diff of source vs. formatted; exit 1 if any differ");
    println!("      --width N     wrap lists past column N (default {})", tulisp_fmt::render::DEFAULT_WIDTH);
    println!("  -h, --help        print this help and exit");
    println!("  -V, --version     print version and exit");
}
