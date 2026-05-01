//! CST → formatted source text.
//!
//! This is the bare renderer: it preserves the user's line breaks
//! one-for-one (a `LineBreak` in the CST produces a `\n` in the
//! output), preserves blank lines (a `LineBreak { count: 2 }` produces
//! one blank line), and indents the body of each list by 2 columns
//! per nesting level.
//!
//! That's deliberately *simpler* than Emacs's elisp-mode indentation
//! — Emacs aligns continuation lines under the second element of a
//! function call, and indents the body of special forms by 2. Those
//! refinements land in subsequent commits, layered on top of this
//! deterministic baseline. The baseline's job is to make
//! `format(format(s)) == format(s)` true for every valid input; later
//! commits change *what* the formatted output looks like without
//! breaking that property.

use crate::cst::{Cst, CstNode};

pub fn render(cst: &Cst) -> String {
    let mut out = String::new();
    render_top_level(&cst.nodes, &mut out);
    if !out.is_empty() && !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn render_top_level(nodes: &[CstNode], out: &mut String) {
    let mut at_line_start = true;
    for node in nodes {
        match node {
            CstNode::LineBreak { count } => {
                out.push('\n');
                if *count >= 2 {
                    out.push('\n');
                }
                at_line_start = true;
            }
            other => {
                if !at_line_start {
                    out.push(' ');
                }
                render_node(other, 0, out);
                at_line_start = false;
            }
        }
    }
}

fn render_node(node: &CstNode, parent_indent: usize, out: &mut String) {
    match node {
        CstNode::Atom { text, .. } => out.push_str(text),
        CstNode::Comment { text, .. } => out.push_str(text),
        CstNode::List { children, .. } => {
            out.push('(');
            render_list_body(children, parent_indent + 2, out);
            out.push(')');
        }
        CstNode::ReaderMacro { prefix, inner, .. } => {
            out.push_str(prefix.as_str());
            render_node(inner, parent_indent, out);
        }
        CstNode::LineBreak { .. } => {
            // Top-level / list-body sequences route line breaks; a
            // LineBreak should never reach here directly.
            debug_assert!(false, "LineBreak passed to render_node");
        }
    }
}

fn render_list_body(nodes: &[CstNode], indent: usize, out: &mut String) {
    let mut at_line_start = true;
    for node in nodes {
        match node {
            CstNode::LineBreak { count } => {
                out.push('\n');
                if *count >= 2 {
                    out.push('\n');
                }
                for _ in 0..indent {
                    out.push(' ');
                }
                at_line_start = true;
            }
            other => {
                if !at_line_start {
                    out.push(' ');
                }
                render_node(other, indent, out);
                at_line_start = false;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cst::{CstNode, ReaderPrefix};

    fn fmt(src: &str) -> String {
        crate::format(src).expect("parse")
    }

    #[test]
    fn empty_input_yields_empty_output() {
        assert_eq!(fmt(""), "");
    }

    #[test]
    fn flat_call_one_line() {
        assert_eq!(fmt("(foo a b)"), "(foo a b)\n");
    }

    #[test]
    fn collapses_extra_spaces() {
        assert_eq!(fmt("(  foo   a  b  )"), "(foo a b)\n");
    }

    #[test]
    fn preserves_user_line_breaks() {
        let src = "(foo\n  a\n  b)";
        let out = fmt(src);
        assert_eq!(out, "(foo\n  a\n  b)\n");
    }

    #[test]
    fn preserves_blank_lines_at_top_level() {
        let src = "(a)\n\n\n(b)\n";
        // Multiple blank lines collapse to one.
        assert_eq!(fmt(src), "(a)\n\n(b)\n");
    }

    #[test]
    fn reader_macros() {
        assert_eq!(fmt("'foo"), "'foo\n");
        assert_eq!(fmt("`(a ,b ,@c)"), "`(a ,b ,@c)\n");
        assert_eq!(fmt("#'fn"), "#'fn\n");
    }

    #[test]
    fn comment_inside_list() {
        let src = "(let ((x 1)) ;; bind\n  (+ x 2))";
        let out = fmt(src);
        assert_eq!(out, "(let ((x 1)) ;; bind\n  (+ x 2))\n");
    }

    #[test]
    fn comment_at_top_level() {
        let src = ";; module doc\n(defun f () 1)";
        assert_eq!(fmt(src), ";; module doc\n(defun f () 1)\n");
    }

    #[test]
    fn round_trip_idempotent() {
        // The round-trip invariant: format(format(s)) == format(s)
        // for every input the formatter accepts.
        let inputs = [
            "",
            "(foo)",
            "(foo a b c)",
            "(foo\n  a\n  b)",
            "(a)\n\n(b)",
            ";; doc\n(defun f (x)\n  (+ x 1))",
            "'(a b ,c ,@d)",
            "(let ((x 1)\n      (y 2))\n  (+ x y))",
            "  (  weird   spacing  )  ",
            "(a (b (c (d e))))",
        ];
        for src in inputs {
            let once = crate::format(src).expect("format once");
            let twice = crate::format(&once).expect("format twice");
            assert_eq!(
                once, twice,
                "round-trip not idempotent for input:\n{src}\nfirst:\n{once}\nsecond:\n{twice}"
            );
        }
    }

    #[test]
    fn renders_atom_text_verbatim() {
        // Hex / character / string literal forms round-trip.
        let cst = crate::parse("(#x1A ?\\n \"hi\")").unwrap();
        assert_eq!(cst.nodes.len(), 1);
        let CstNode::List { children, .. } = &cst.nodes[0] else {
            panic!()
        };
        let texts: Vec<&str> = children
            .iter()
            .filter_map(|n| match n {
                CstNode::Atom { text, .. } => Some(text.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(texts, vec!["#x1A", r#"?\n"#, r#""hi""#]);
        // And the formatted output preserves them.
        assert_eq!(crate::format("(#x1A ?\\n \"hi\")").unwrap(), "(#x1A ?\\n \"hi\")\n");
    }

    #[test]
    fn reader_macros_have_no_internal_space() {
        // The parser drops trivia between a reader-macro prefix and
        // its target, so the renderer always produces tight forms.
        assert_eq!(fmt("' foo"), "'foo\n");
        assert_eq!(fmt("` ( a ,b )"), "`(a ,b)\n");
    }

    #[test]
    fn unused_reader_prefix_helpers() {
        // Touch ReaderPrefix::as_str via the renderer so the all-prefixes
        // path is covered.
        for p in [
            ReaderPrefix::Quote,
            ReaderPrefix::Backquote,
            ReaderPrefix::Unquote,
            ReaderPrefix::Splice,
            ReaderPrefix::Sharpquote,
        ] {
            assert!(!p.as_str().is_empty());
        }
    }
}
