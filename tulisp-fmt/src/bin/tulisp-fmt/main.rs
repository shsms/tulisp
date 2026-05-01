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

mod config;
mod diff;

#[derive(Default)]
struct Args {
    write_in_place: bool,
    check: bool,
    diff: bool,
    style: tulisp_fmt::Style,
    style_set: config::SetFlags,
    range: Option<(usize, usize)>,
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
        return run_stdin(&parsed);
    }

    let mut expanded: Vec<String> = Vec::new();
    let mut walk_error = false;
    for raw in &parsed.paths {
        match expand_path(raw, &mut expanded) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("tulisp-fmt: {raw}: {e}");
                walk_error = true;
            }
        }
    }

    let mut differed = false;
    let mut had_error = walk_error;
    for path in &expanded {
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
                out.style.width = parse_width(&v)?;
                out.style_set.width = true;
            }
            s if s.starts_with("--width=") => {
                out.style.width = parse_width(&s["--width=".len()..])?;
                out.style_set.width = true;
            }
            "--use-tabs" => {
                out.style.use_tabs = true;
                out.style_set.use_tabs = true;
            }
            "--range" => {
                let v = iter
                    .next()
                    .ok_or_else(|| "--range requires START:END".to_string())?;
                out.range = Some(parse_range(&v)?);
            }
            s if s.starts_with("--range=") => {
                out.range = Some(parse_range(&s["--range=".len()..])?);
            }
            "--indent-width" => {
                let v = iter
                    .next()
                    .ok_or_else(|| "--indent-width requires a value".to_string())?;
                out.style.indent_width = parse_indent(&v)?;
                out.style_set.indent_width = true;
            }
            s if s.starts_with("--indent-width=") => {
                out.style.indent_width = parse_indent(&s["--indent-width=".len()..])?;
                out.style_set.indent_width = true;
            }
            "--tab-width" => {
                let v = iter
                    .next()
                    .ok_or_else(|| "--tab-width requires a value".to_string())?;
                out.style.tab_width = parse_tab_width(&v)?;
                out.style_set.tab_width = true;
            }
            s if s.starts_with("--tab-width=") => {
                out.style.tab_width = parse_tab_width(&s["--tab-width=".len()..])?;
                out.style_set.tab_width = true;
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

fn parse_indent(s: &str) -> Result<usize, String> {
    let n: usize = s
        .parse()
        .map_err(|_| format!("--indent-width: not a non-negative integer: {s}"))?;
    if n == 0 {
        return Err("--indent-width: must be at least 1".to_string());
    }
    Ok(n)
}

fn parse_tab_width(s: &str) -> Result<usize, String> {
    let n: usize = s
        .parse()
        .map_err(|_| format!("--tab-width: not a non-negative integer: {s}"))?;
    if n == 0 {
        return Err("--tab-width: must be at least 1".to_string());
    }
    Ok(n)
}

fn parse_range(s: &str) -> Result<(usize, usize), String> {
    let (a, b) = s
        .split_once(':')
        .ok_or_else(|| format!("--range: expected START:END, got `{s}`"))?;
    let start: usize = a
        .parse()
        .map_err(|_| format!("--range: bad START `{a}`"))?;
    let end: usize = b
        .parse()
        .map_err(|_| format!("--range: bad END `{b}`"))?;
    if end < start {
        return Err(format!("--range: END < START ({end} < {start})"));
    }
    Ok((start, end))
}

/// File extensions we consider "lisp source" when an arg is a
/// directory. Files passed explicitly are formatted regardless of
/// extension — extension filtering only kicks in during recursive
/// directory traversal.
const LISP_EXTS: &[&str] = &["lisp", "el"];

/// Push every formattable file under `arg` onto `out`. If `arg` is
/// a regular file (or symlink to one), it's pushed as-is. If it's a
/// a directory, the tree is walked and every `*.lisp` / `*.el` file
/// is collected. Hidden directories (those whose names start with
/// `.`) are skipped to avoid descending into `.git/` and friends.
fn expand_path(arg: &str, out: &mut Vec<String>) -> io::Result<()> {
    let p = Path::new(arg);
    let meta = fs::symlink_metadata(p)?;
    if !meta.file_type().is_dir() {
        out.push(arg.to_string());
        return Ok(());
    }
    walk_dir(p, out)
}

fn walk_dir(dir: &Path, out: &mut Vec<String>) -> io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<Result<_, _>>()?;
    // Stable order so output is deterministic across runs and
    // platforms — `read_dir` order is not specified.
    entries.sort_by_key(|e| e.file_name());
    for entry in entries {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with('.') {
            continue;
        }
        let ftype = entry.file_type()?;
        let path = entry.path();
        if ftype.is_dir() {
            walk_dir(&path, out)?;
        } else if ftype.is_file() {
            let ext = path
                .extension()
                .and_then(|s| s.to_str())
                .unwrap_or_default();
            if LISP_EXTS.contains(&ext)
                && let Some(s) = path.to_str()
            {
                out.push(s.to_string());
            }
        }
    }
    Ok(())
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
    let style = match resolve_style(args, Some(Path::new(path))) {
        Ok(s) => s,
        Err(msg) => {
            eprintln!("tulisp-fmt: {msg}");
            return Outcome::Error;
        }
    };
    let formatted = match format_input(&src, &style, args.range) {
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

fn run_stdin(args: &Args) -> ExitCode {
    let style = match resolve_style(args, None) {
        Ok(s) => s,
        Err(msg) => {
            eprintln!("tulisp-fmt: {msg}");
            return ExitCode::from(2);
        }
    };
    let mut src = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut src) {
        eprintln!("tulisp-fmt: stdin: {e}");
        return ExitCode::from(2);
    }
    let formatted = match format_input(&src, &style, args.range) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.render(&src, Some("<stdin>")));
            return ExitCode::from(1);
        }
    };
    let payload = if args.diff {
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
    if args.diff {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

/// Compute the per-file style: start from the CLI-set base, then
/// fold in any `.tulisp-fmt.toml` discovered upward from the file
/// (or from the current working directory when there's no file).
/// Fields the CLI explicitly set are protected from being clobbered.
fn resolve_style(args: &Args, file: Option<&Path>) -> Result<tulisp_fmt::Style, String> {
    let mut style = args.style;
    match file {
        Some(p) => config::load_for(p, &mut style, args.style_set)?,
        None => config::load_for_cwd(&mut style, args.style_set)?,
    }
    Ok(style)
}

/// Format `src` either in full or — when `--range` is set —
/// restricted to top-level forms overlapping the given byte range.
fn format_input(
    src: &str,
    style: &tulisp_fmt::Style,
    range: Option<(usize, usize)>,
) -> Result<String, tulisp_fmt::parse::ParseError> {
    match range {
        Some((s, e)) => tulisp_fmt::format_range(src, s, e, style),
        None => tulisp_fmt::format_with_style(src, style),
    }
}

fn print_help() {
    println!("Usage: tulisp-fmt [OPTIONS] [FILE|DIR...]");
    println!();
    println!("Format Emacs Lisp / tulisp source files.");
    println!();
    println!("With no FILE argument, reads from stdin and writes to stdout.");
    println!("With FILE arguments, formats each file and writes the result to stdout");
    println!("by default. Pass `-w` to overwrite the file in place. Directory");
    println!("arguments are walked recursively for *.lisp and *.el files; entries");
    println!("starting with `.` (e.g. .git) are skipped.");
    println!();
    println!(
        "Style settings can also live in a .tulisp-fmt.toml file discovered by"
    );
    println!("walking up from the input. CLI flags always override the file.");
    println!();
    println!("Options:");
    println!("  -w, --write          write the formatted result back to each FILE");
    println!("      --check          exit 1 (and list filenames) if any FILE is unformatted");
    println!("  -d, --diff           print a unified diff of source vs. formatted; exit 1 if any differ");
    println!(
        "      --width N        wrap lists past column N (default {})",
        tulisp_fmt::render::DEFAULT_WIDTH,
    );
    println!(
        "      --indent-width N body-indent step in columns (default {})",
        tulisp_fmt::render::DEFAULT_INDENT_WIDTH,
    );
    println!("      --use-tabs       emit tab characters for indents instead of spaces");
    println!(
        "      --tab-width N    columns per tab when --use-tabs is set (default {})",
        tulisp_fmt::render::DEFAULT_TAB_WIDTH,
    );
    println!("      --range S:E      format only top-level forms overlapping byte range [S, E)");
    println!("  -h, --help           print this help and exit");
    println!("  -V, --version        print version and exit");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn walks_lisp_extensions_skips_dotdirs() {
        let tmp = tempdir();
        // Layout:
        //   <tmp>/a.lisp        — pick
        //   <tmp>/sub/b.el      — pick
        //   <tmp>/sub/c.txt     — skip (wrong ext)
        //   <tmp>/.hidden/d.el  — skip (hidden parent)
        fs::create_dir(tmp.join("sub")).unwrap();
        fs::create_dir(tmp.join(".hidden")).unwrap();
        fs::write(tmp.join("a.lisp"), "()").unwrap();
        fs::write(tmp.join("sub/b.el"), "()").unwrap();
        fs::write(tmp.join("sub/c.txt"), "()").unwrap();
        fs::write(tmp.join(".hidden/d.el"), "()").unwrap();

        let mut out = Vec::new();
        expand_path(tmp.to_str().unwrap(), &mut out).unwrap();
        out.sort();
        let rels: Vec<_> = out
            .iter()
            .map(|p| Path::new(p).strip_prefix(&tmp).unwrap().to_owned())
            .collect();
        assert_eq!(
            rels,
            vec![PathBuf::from("a.lisp"), PathBuf::from("sub/b.el")],
        );
    }

    #[test]
    fn explicit_file_keeps_extension() {
        let tmp = tempdir();
        let f = tmp.join("oddly_named.txt");
        fs::write(&f, "()").unwrap();
        let mut out = Vec::new();
        expand_path(f.to_str().unwrap(), &mut out).unwrap();
        assert_eq!(out, vec![f.to_str().unwrap().to_string()]);
    }

    /// Minimal `mktemp -d` substitute. Returns a directory path
    /// scoped to a unique nanosecond/pid suffix; we don't bother
    /// removing it — `/tmp` is fine for test detritus and keeping
    /// the failing-test artefacts around helps debugging.
    fn tempdir() -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.subsec_nanos())
            .unwrap_or(0);
        let p = std::env::temp_dir().join(format!(
            "tulisp-fmt-walk-{}-{nanos}",
            std::process::id(),
        ));
        fs::create_dir_all(&p).unwrap();
        p
    }
}
