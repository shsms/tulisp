//! `.tulisp-fmt.toml` discovery and parsing.
//!
//! This is a flat key=value config file that's a strict subset of
//! TOML — no sections, no arrays, no inline tables. Recognised keys:
//!
//! ```toml
//! width = 80
//! indent_width = 2
//! use_tabs = false
//! tab_width = 8
//! ```
//!
//! Unknown keys are an error so typos don't silently no-op.
//!
//! Discovery walks upward from the file (or current directory) until
//! it finds a `.tulisp-fmt.toml` or hits the filesystem root. Each
//! input file is resolved separately, so a monorepo with per-crate
//! configs Just Works. The resolution result is memoised by the
//! ancestor directory list so we don't re-read the same file for
//! every input.
//!
//! CLI flags always win over config-file values — the CLI passes a
//! [`SetFlags`] mask describing which knobs were explicitly set so
//! we know what not to clobber.

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use tulisp_fmt::Style;

const FILENAME: &str = ".tulisp-fmt.toml";

/// Tracks which `Style` fields were set from the command line. Any
/// field marked here is left alone when a config file is merged in.
#[derive(Default, Clone, Copy)]
pub struct SetFlags {
    pub width: bool,
    pub indent_width: bool,
    pub use_tabs: bool,
    pub tab_width: bool,
    pub alist_one_per_line: bool,
}

/// Walk upward from `start` (a file or directory) looking for
/// `.tulisp-fmt.toml`. Returns the path to the first one found, or
/// `Ok(None)` if none exists up to the filesystem root.
pub fn discover(start: &Path) -> Option<PathBuf> {
    let mut dir = if start.is_file() {
        start.parent()?.to_path_buf()
    } else {
        start.to_path_buf()
    };
    loop {
        let candidate = dir.join(FILENAME);
        if candidate.is_file() {
            return Some(candidate);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Apply the config at `path` onto `style`, skipping any fields
/// already set via CLI per `flags`.
pub fn apply(path: &Path, style: &mut Style, flags: SetFlags) -> Result<(), String> {
    let text = fs::read_to_string(path)
        .map_err(|e| format!("{}: {e}", path.display()))?;
    let map = parse(&text).map_err(|e| format!("{}: {e}", path.display()))?;

    for (k, v) in &map {
        match k.as_str() {
            "width" => {
                if !flags.width {
                    style.width = parse_usize(v, k)?;
                    if style.width < 8 {
                        return Err(format!("width: must be at least 8, got {}", style.width));
                    }
                }
            }
            "indent_width" => {
                if !flags.indent_width {
                    style.indent_width = parse_usize(v, k)?;
                    if style.indent_width == 0 {
                        return Err("indent_width: must be at least 1".to_string());
                    }
                }
            }
            "use_tabs" => {
                if !flags.use_tabs {
                    style.use_tabs = parse_bool(v, k)?;
                }
            }
            "tab_width" => {
                if !flags.tab_width {
                    style.tab_width = parse_usize(v, k)?;
                    if style.tab_width == 0 {
                        return Err("tab_width: must be at least 1".to_string());
                    }
                }
            }
            "alist_one_per_line" => {
                if !flags.alist_one_per_line {
                    style.alist_one_per_line = parse_bool(v, k)?;
                }
            }
            other => return Err(format!("unknown key `{other}`")),
        }
    }
    Ok(())
}

fn parse_usize(v: &str, key: &str) -> Result<usize, String> {
    v.parse()
        .map_err(|_| format!("{key}: expected integer, got `{v}`"))
}

fn parse_bool(v: &str, key: &str) -> Result<bool, String> {
    match v {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(format!("{key}: expected `true` or `false`, got `{v}`")),
    }
}

/// Parse a flat-key TOML subset: blank lines, `# …` comments, and
/// `key = value` lines where the value is a bare integer literal,
/// `true` / `false`, or a `"…"` string. Anything more elaborate
/// (tables, arrays) is a parse error so the misuse is loud.
fn parse(text: &str) -> Result<HashMap<String, String>, String> {
    let mut map = HashMap::new();
    for (line_no, raw) in text.lines().enumerate() {
        let line = strip_comment(raw).trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with('[') {
            return Err(format!("{}: tables are not supported", line_no + 1));
        }
        let Some((k, v)) = line.split_once('=') else {
            return Err(format!("{}: missing `=`", line_no + 1));
        };
        let key = k.trim().to_string();
        if key.is_empty() {
            return Err(format!("{}: empty key", line_no + 1));
        }
        let value = parse_value(v.trim()).map_err(|e| format!("line {}: {e}", line_no + 1))?;
        if map.insert(key.clone(), value).is_some() {
            return Err(format!("line {}: duplicate key `{key}`", line_no + 1));
        }
    }
    Ok(map)
}

fn strip_comment(line: &str) -> &str {
    // We don't handle `#` inside a quoted string because none of the
    // recognised values are strings that would contain one. If that
    // changes, this needs a state machine.
    match line.find('#') {
        Some(i) => &line[..i],
        None => line,
    }
}

fn parse_value(raw: &str) -> Result<String, String> {
    if let Some(rest) = raw.strip_prefix('"') {
        let body = rest
            .strip_suffix('"')
            .ok_or("unterminated string literal")?;
        if body.contains('\\') {
            return Err("escape sequences in strings are not supported".to_string());
        }
        return Ok(body.to_string());
    }
    if raw.is_empty() {
        return Err("empty value".to_string());
    }
    Ok(raw.to_string())
}

/// Convenience: discover and apply for `path`. Errors are returned
/// as strings; the CLI prefixes them with `tulisp-fmt: ` and prints
/// to stderr.
pub fn load_for(path: &Path, style: &mut Style, flags: SetFlags) -> Result<(), String> {
    let cfg = discover(path);
    let Some(cfg) = cfg else { return Ok(()) };
    apply(&cfg, style, flags)
}

/// Fallback used when no input file exists yet (stdin path). Looks
/// from the current working directory.
pub fn load_for_cwd(style: &mut Style, flags: SetFlags) -> Result<(), String> {
    let cwd = std::env::current_dir()
        .map_err(|e: io::Error| format!("cwd: {e}"))?;
    load_for(&cwd, style, flags)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_all_keys() {
        let text = r#"
            width = 100
            indent_width = 4
            use_tabs = true
            tab_width = 4
        "#;
        let map = parse(text).unwrap();
        assert_eq!(map.get("width").map(String::as_str), Some("100"));
        assert_eq!(map.get("indent_width").map(String::as_str), Some("4"));
        assert_eq!(map.get("use_tabs").map(String::as_str), Some("true"));
        assert_eq!(map.get("tab_width").map(String::as_str), Some("4"));
    }

    #[test]
    fn comments_and_blank_lines_ignored() {
        let text = "
            # leading comment
            width = 100  # trailing
            \n
        ";
        let map = parse(text).unwrap();
        assert_eq!(map.len(), 1);
        assert_eq!(map.get("width").map(String::as_str), Some("100"));
    }

    #[test]
    fn unknown_key_fails_apply() {
        let mut style = Style::default();
        let tmp = std::env::temp_dir().join(format!(
            "tulisp-fmt-cfg-{}.toml",
            std::process::id(),
        ));
        fs::write(&tmp, "weird_key = 1\n").unwrap();
        let err = apply(&tmp, &mut style, SetFlags::default()).unwrap_err();
        assert!(err.contains("unknown key"), "got: {err}");
        let _ = fs::remove_file(&tmp);
    }

    #[test]
    fn cli_flags_win_over_config() {
        let mut style = Style {
            width: 100,
            ..Style::default()
        };
        let tmp = std::env::temp_dir().join(format!(
            "tulisp-fmt-cfg-{}-flags.toml",
            std::process::id(),
        ));
        fs::write(&tmp, "width = 60\n").unwrap();
        let flags = SetFlags { width: true, ..SetFlags::default() };
        apply(&tmp, &mut style, flags).unwrap();
        // CLI-set width was preserved.
        assert_eq!(style.width, 100);
        let _ = fs::remove_file(&tmp);
    }
}
