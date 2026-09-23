use crate::{Error, TulispContext};

mod comparison_of_strings;
mod conditionals;
mod core;
mod equality_predicates;
pub(crate) mod errors;
mod hash_table;
mod list_elements;
mod numbers;
mod sequences;
mod time_operations;

pub(crate) fn add(ctx: &mut TulispContext) {
    comparison_of_strings::add(ctx);
    conditionals::add(ctx);
    equality_predicates::add(ctx);
    errors::add(ctx);
    core::add(ctx);
    hash_table::add(ctx);
    list_elements::add(ctx);
    numbers::add(ctx);
    sequences::add(ctx);
    time_operations::add(ctx);
}

/// Write `text` to stdout, with a newline after it when `newline`
/// is set. A write error becomes a Lisp error instead of a panic; a
/// closed pipe gets its own kind, so the CLI can exit quietly like
/// other Unix filters.
pub(crate) fn print_to_stdout(text: &str, newline: bool) -> Result<(), Error> {
    use std::io::Write;
    let mut out = std::io::stdout().lock();
    let written = if newline {
        writeln!(out, "{text}")
    } else {
        // Nothing triggers the line-buffered flush without a
        // newline, so flush here to keep prompt-style partial lines
        // visible immediately.
        write!(out, "{text}").and_then(|_| out.flush())
    };
    written.map_err(|e| {
        if e.kind() == std::io::ErrorKind::BrokenPipe {
            Error::broken_pipe("print: broken pipe".to_string())
        } else {
            Error::os_error(format!("print: {e}"))
        }
    })
}
