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
pub use render::Style;

/// Format a source string with the default style (80-column width
/// budget, two-space indents, no tabs). Returns the formatted source
/// on success, or a parse error. The formatter is deterministic and
/// idempotent for any input it accepts.
pub fn format(source: &str) -> Result<String, parse::ParseError> {
    format_with_style(source, &Style::default())
}

/// Format a source string with a custom width budget but otherwise
/// default style. Lists that don't fit within `width` columns at
/// their starting position are rendered multi-line; lists that fit
/// are kept on one line. User line breaks and comments are always
/// preserved (and force multi-line layout for the surrounding list).
pub fn format_with_width(
    source: &str,
    width: usize,
) -> Result<String, parse::ParseError> {
    format_with_style(
        source,
        &Style {
            width,
            ..Style::default()
        },
    )
}

/// Format a source string with a fully-specified [`Style`]. Use this
/// when you need control over indent step, tab vs. space, or tab
/// width on top of the column budget.
pub fn format_with_style(
    source: &str,
    style: &Style,
) -> Result<String, parse::ParseError> {
    let tokens = lex::lex(source);
    let cst = parse::parse_with_source(&tokens, Some(source))?;
    Ok(render::render_with_style(&cst, style))
}

/// Parse a source string into a [`Cst`] without rendering. Useful for
/// tooling that wants to inspect or transform the tree directly.
pub fn parse(source: &str) -> Result<Cst, parse::ParseError> {
    let tokens = lex::lex(source);
    parse::parse_with_source(&tokens, Some(source))
}
