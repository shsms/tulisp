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

/// Format only the top-level forms whose source span overlaps the
/// byte range `[start, end)`. Bytes outside the touched forms are
/// preserved verbatim. Useful for editor "format selection" actions
/// where the user wants to leave the rest of the file alone.
///
/// Forms that *partly* overlap the range are formatted in their
/// entirety — the formatter cannot safely format a fragment of a
/// list. The returned string has length `source.len() + (formatted -
/// original)`, so byte offsets after the affected region shift by
/// the formatting delta.
pub fn format_range(
    source: &str,
    start: usize,
    end: usize,
    style: &Style,
) -> Result<String, parse::ParseError> {
    let tokens = lex::lex(source);
    let cst = parse::parse_with_source(&tokens, Some(source))?;
    let nodes = &cst.nodes;

    let first = nodes.iter().position(|n| overlaps(n, start, end));
    let last = nodes.iter().rposition(|n| overlaps(n, start, end));
    let (i, j) = match (first, last) {
        (Some(i), Some(j)) => (i, j),
        _ => return Ok(source.to_string()),
    };

    let span_start = nodes[i].span().start;
    let span_end = nodes[j].span().end;

    let slice_cst = cst::Cst {
        nodes: nodes[i..=j].to_vec(),
    };
    let mut formatted = render::render_with_style(&slice_cst, style);
    // The full-render path always ensures a trailing newline; in a
    // partial render that's only correct if the original source had
    // one at `span_end`, otherwise we'd inject an extra `\n`.
    if formatted.ends_with('\n')
        && source[span_end..]
            .chars()
            .next()
            .is_some_and(|c| c == '\n')
    {
        formatted.pop();
    }

    let mut out = String::with_capacity(
        source.len() - (span_end - span_start) + formatted.len(),
    );
    out.push_str(&source[..span_start]);
    out.push_str(&formatted);
    out.push_str(&source[span_end..]);
    Ok(out)
}

fn overlaps(node: &cst::CstNode, start: usize, end: usize) -> bool {
    match node {
        cst::CstNode::LineBreak { .. } => false,
        _ => {
            let s = node.span();
            s.start < end && start < s.end
        }
    }
}

/// Parse a source string into a [`Cst`] without rendering. Useful for
/// tooling that wants to inspect or transform the tree directly.
pub fn parse(source: &str) -> Result<Cst, parse::ParseError> {
    let tokens = lex::lex(source);
    parse::parse_with_source(&tokens, Some(source))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn range_formats_only_overlapping_form() {
        // Two forms; the range covers the second only. The first
        // should be preserved verbatim, including its sloppy spacing.
        let src =
            "(  foo  bar  )\n(let ((x 1) (y 2) (z 3) (w 4)) body)\n";
        let style = Style {
            width: 20,
            ..Style::default()
        };
        let second_start = src.find("(let").unwrap();
        let second_end = src.len() - 1;
        let out = format_range(src, second_start, second_end, &style).unwrap();
        // The first form is unchanged.
        assert!(out.starts_with("(  foo  bar  )\n"), "got:\n{out}");
        // The second form is wrapped — bindings get one per line
        // because the bindings list itself doesn't fit.
        assert!(out.contains("(let ((x 1)\n"), "got:\n{out}");
    }

    #[test]
    fn range_outside_any_form_returns_source_unchanged() {
        let src = "\n\n;; just a comment\n";
        let style = Style::default();
        // Pick a range over the leading blank lines (no structural
        // node lives there).
        let out = format_range(src, 0, 1, &style).unwrap();
        assert_eq!(out, src);
    }

    #[test]
    fn range_covering_whole_file_matches_full_format() {
        let src = "(let ((x 1) (y 2) (z 3) (w 4)) body)\n";
        let style = Style {
            width: 25,
            ..Style::default()
        };
        let full = format_with_style(src, &style).unwrap();
        let range = format_range(src, 0, src.len(), &style).unwrap();
        assert_eq!(range, full);
    }
}
