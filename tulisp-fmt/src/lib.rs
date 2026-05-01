//! Source code formatter for tulisp / Emacs Lisp.
//!
//! This crate is intentionally independent of the `tulisp` interpreter
//! crate — it operates on source text alone, parsing into a concrete
//! syntax tree (CST) that retains comments, blank lines, and original
//! literal spelling, then renders back to source.
//!
//! The pipeline is:
//!
//! 1. [`lex`](lex::lex) — tokenize the source, classifying comments
//!    and blank lines as trivia kept alongside the structural tokens.
//! 2. [`parse`](parse::parse) — assemble the token stream into a
//!    [`Cst`](cst::Cst) tree.
//! 3. [`render`](render::render) — walk the CST and emit formatted
//!    source.
//!
//! Round-trip stability — `format(format(src)) == format(src)` — is
//! treated as an invariant; the integration tests enforce it across a
//! corpus.

pub mod cst;
pub mod lex;
pub mod parse;
pub mod render;

pub use cst::Cst;

/// Format a source string. Returns the formatted source on success, or
/// a parse error. The formatter is deterministic and idempotent for
/// any input it accepts.
pub fn format(source: &str) -> Result<String, parse::ParseError> {
    let tokens = lex::lex(source);
    let cst = parse::parse(&tokens)?;
    Ok(render::render(&cst))
}
