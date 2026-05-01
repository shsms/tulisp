//! CST → formatted source text.
//!
//! Indentation follows two rules:
//!
//! - **Function-call form (default)** — line breaks inside the list
//!   align the next child under the *second* element. So
//!   `(foo a\n  b)` becomes `(foo a\n     b)` with `b` aligned under
//!   `a`.
//! - **Special form** — recognised heads (`let`, `defun`, `cond`, …)
//!   indent body line breaks at `(open_paren_col + 2)`, matching the
//!   Emacs convention.
//!
//! User line breaks and blank lines are preserved verbatim — Emacs's
//! `indent-region` doesn't touch them, so neither do we. The output
//! always ends with a trailing `\n` if non-empty.

use crate::cst::{Cst, CstNode};

pub fn render(cst: &Cst) -> String {
    let mut r = Renderer::default();
    render_top_level(&cst.nodes, &mut r);
    if !r.out.is_empty() && !r.out.ends_with('\n') {
        r.out.push('\n');
    }
    r.out
}

#[derive(Default)]
struct Renderer {
    out: String,
    col: usize,
}

impl Renderer {
    fn write(&mut self, s: &str) {
        for c in s.chars() {
            if c == '\n' {
                self.col = 0;
            } else {
                self.col += 1;
            }
        }
        self.out.push_str(s);
    }

    fn newline_then_indent(&mut self, blank_lines: u32, indent: usize) {
        self.out.push('\n');
        for _ in 0..blank_lines {
            self.out.push('\n');
        }
        for _ in 0..indent {
            self.out.push(' ');
        }
        self.col = indent;
    }
}

fn render_top_level(nodes: &[CstNode], r: &mut Renderer) {
    let mut at_line_start = true;
    for node in nodes {
        match node {
            CstNode::LineBreak { count } => {
                r.newline_then_indent(count.saturating_sub(1), 0);
                at_line_start = true;
            }
            other => {
                if !at_line_start {
                    r.write(" ");
                }
                render_node(other, r);
                at_line_start = false;
            }
        }
    }
}

fn render_node(node: &CstNode, r: &mut Renderer) {
    match node {
        CstNode::Atom { text, .. } => r.write(text),
        CstNode::Comment { text, .. } => r.write(text),
        CstNode::List { children, .. } => {
            let open_col = r.col;
            r.write("(");
            render_list_body(children, open_col, r);
            r.write(")");
        }
        CstNode::ReaderMacro { prefix, inner, .. } => {
            r.write(prefix.as_str());
            render_node(inner, r);
        }
        CstNode::LineBreak { .. } => {
            debug_assert!(false, "LineBreak passed to render_node");
        }
    }
}

fn render_list_body(nodes: &[CstNode], open_col: usize, r: &mut Renderer) {
    let mut head_text: Option<String> = None;
    let mut second_col: Option<usize> = None;
    let mut struct_count: usize = 0;
    let mut at_line_start = true;

    for node in nodes {
        match node {
            CstNode::LineBreak { count } => {
                let indent = compute_indent(
                    head_text.as_deref(),
                    struct_count,
                    second_col,
                    open_col,
                );
                r.newline_then_indent(count.saturating_sub(1), indent);
                at_line_start = true;
            }
            CstNode::Comment { text, .. } => {
                if !at_line_start {
                    r.write(" ");
                }
                r.write(text);
                at_line_start = false;
            }
            structural => {
                if !at_line_start {
                    r.write(" ");
                }
                if struct_count == 0
                    && let CstNode::Atom { text, .. } = structural
                {
                    head_text = Some(text.clone());
                }
                if struct_count == 1 {
                    second_col = Some(r.col);
                }
                render_node(structural, r);
                struct_count += 1;
                at_line_start = false;
            }
        }
    }
}

/// Indent column for the next line inside a list body.
///
/// - Before any structural child has rendered: align under whatever
///   *would* have been the first child — column right after `(`.
/// - For special-form heads (see [`is_special_form`]): body indents
///   at `open_col + 2`.
/// - Otherwise (function-call form): align under the second
///   structural child if one has rendered; else fall back to
///   `open_col + 1`. Matching Emacs:
///
///   ```text
///   (foo bar          (foo
///        baz)               bar)
///   ```
///
///   When the line break is before any arg has rendered, Emacs uses
///   `open_col + 1` rather than estimating where the second arg
///   *would* have landed.
fn compute_indent(
    head: Option<&str>,
    struct_count: usize,
    second_col: Option<usize>,
    open_col: usize,
) -> usize {
    if struct_count == 0 {
        return open_col + 1;
    }
    if head.is_some_and(is_special_form) {
        return open_col + 2;
    }
    second_col.unwrap_or(open_col + 1)
}

/// Special forms whose body indents at `open_col + 2` rather than
/// aligning under the second element. The list mirrors Emacs's
/// elisp-mode defaults for the most common cases; user-defined
/// `(declare (indent N))` is not yet read.
fn is_special_form(head: &str) -> bool {
    matches!(
        head,
        "let"
            | "let*"
            | "lambda"
            | "defun"
            | "defmacro"
            | "defvar"
            | "defconst"
            | "defspecial"
            | "when"
            | "unless"
            | "while"
            | "dolist"
            | "dotimes"
            | "cond"
            | "if"
            | "if-let"
            | "if-let*"
            | "when-let"
            | "while-let"
            | "condition-case"
            | "progn"
            | "prog1"
            | "prog2"
            | "save-excursion"
            | "save-restriction"
            | "save-window-excursion"
            | "with-current-buffer"
            | "with-temp-buffer"
            | "with-temp-file"
            | "with-output-to-string"
            | "catch"
            | "unwind-protect"
            | "->"
            | "->>"
            | "thread-first"
            | "thread-last"
    )
}

#[cfg(test)]
mod tests {
    use crate::cst::CstNode;

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
    fn function_call_aligns_under_second() {
        // `b` aligns under `a` (column of the second element).
        assert_eq!(fmt("(foo a\n b)"), "(foo a\n     b)\n");
    }

    #[test]
    fn special_form_body_indents_two() {
        let src = "(when cond\nbody)";
        assert_eq!(fmt(src), "(when cond\n  body)\n");
    }

    #[test]
    fn let_body_indents_two() {
        let src = "(let ((x 1) (y 2))\n(+ x y))";
        assert_eq!(fmt(src), "(let ((x 1) (y 2))\n  (+ x y))\n");
    }

    #[test]
    fn defun_body_indents_two() {
        let src = "(defun f (x)\n(* x x))";
        assert_eq!(fmt(src), "(defun f (x)\n  (* x x))\n");
    }

    #[test]
    fn nested_special_and_call() {
        let src = "(let ((x 1))\n(foo x\ny))";
        assert_eq!(
            fmt(src),
            "(let ((x 1))\n  (foo x\n       y))\n"
        );
    }

    #[test]
    fn preserves_blank_lines_at_top_level() {
        // All blank lines round-trip — Emacs's `indent-region` doesn't
        // touch them, so we don't either.
        assert_eq!(fmt("(a)\n\n(b)\n"), "(a)\n\n(b)\n");
        assert_eq!(fmt("(a)\n\n\n(b)\n"), "(a)\n\n\n(b)\n");
    }

    #[test]
    fn reader_macros() {
        assert_eq!(fmt("'foo"), "'foo\n");
        assert_eq!(fmt("`(a ,b ,@c)"), "`(a ,b ,@c)\n");
        assert_eq!(fmt("#'fn"), "#'fn\n");
    }

    #[test]
    fn comment_inside_list() {
        let src = "(let ((x 1)) ;; bind\n(+ x 2))";
        assert_eq!(fmt(src), "(let ((x 1)) ;; bind\n  (+ x 2))\n");
    }

    #[test]
    fn comment_at_top_level() {
        assert_eq!(
            fmt(";; module doc\n(defun f () 1)"),
            ";; module doc\n(defun f () 1)\n"
        );
    }

    #[test]
    fn comment_block_at_top_level() {
        let src = ";; first\n;; second\n;; third\n(foo)";
        assert_eq!(fmt(src), ";; first\n;; second\n;; third\n(foo)\n");
    }

    #[test]
    fn trailing_single_semicolon() {
        assert_eq!(
            fmt("(foo) ; trailing\n(bar)"),
            "(foo) ; trailing\n(bar)\n"
        );
    }

    #[test]
    fn comment_then_body_when_broken_before_first_arg() {
        // No arg has rendered yet when the line break hits, so the
        // comment and body both indent to `open_col + 1` — matching
        // what Emacs's lisp-data-mode produces.
        let src = "(foo\n  ;; doc\n  body)";
        assert_eq!(fmt(src), "(foo\n ;; doc\n body)\n");
    }

    #[test]
    fn comment_then_body_in_special_form() {
        // Inside a special form, the body / comment indent at +2.
        let src = "(let ((x 1))\n  ;; doc\n  (use x))";
        assert_eq!(fmt(src), "(let ((x 1))\n  ;; doc\n  (use x))\n");
    }

    #[test]
    fn comment_with_no_trailing_newline() {
        // Source with no trailing `\n` still ends with one.
        assert_eq!(fmt(";; eof comment"), ";; eof comment\n");
    }

    #[test]
    fn round_trip_idempotent() {
        let inputs = [
            "",
            "(foo)",
            "(foo a b c)",
            "(foo a\n b)",
            "(foo\n  a\n  b)",
            "(a)\n\n(b)",
            ";; doc\n(defun f (x)\n  (+ x 1))",
            "'(a b ,c ,@d)",
            "(let ((x 1)\n      (y 2))\n  (+ x y))",
            "  (  weird   spacing  )  ",
            "(a (b (c (d e))))",
            "(when cond\n  body)",
            "(if cond\n  then\n  else)",
            "(cond ((= x 1) 'one)\n      ((= x 2) 'two))",
            ";; module doc\n;; with two lines\n(defun f () 1)",
            "(foo\n ;; doc\n body)",
            "(let ((x 1))\n  ;; explain\n  (use x))",
            "(foo a) ; trailing\n(bar)",
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
        let cst = crate::parse("(#x1A ?\\n \"hi\")").unwrap();
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
        assert_eq!(crate::format("(#x1A ?\\n \"hi\")").unwrap(), "(#x1A ?\\n \"hi\")\n");
    }
}
