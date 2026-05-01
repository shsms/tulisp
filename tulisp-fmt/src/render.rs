//! CST → formatted source text.
//!
//! Two-phase rendering:
//!
//! 1. **Fit-or-break decision per list.** Each list is measured for
//!    its one-line width. If it fits in the remaining width budget
//!    (default 80 columns) *and* nothing inside it requires
//!    multi-line layout (user line breaks, comments), it renders on
//!    one line. Otherwise it renders multi-line.
//!
//! 2. **Multi-line layout.** User line breaks are preserved
//!    one-for-one. Additional breaks are inserted between structural
//!    children where the head's "header arity" requires it — for a
//!    plain function call, args after the first land on their own
//!    lines; for `defun`-style two-arg-header forms the name and
//!    arglist stay with the head; for `progn`-style zero-header
//!    forms every body arg gets its own line.
//!
//! Indentation rules (which column to use for the inserted or
//! preserved line breaks) match Emacs's lisp-data-mode for the
//! common cases: function calls align continuation lines under the
//! second element, `let`/`defun`/`when`/etc. indent the body at
//! `open_col + 2`, `progn`/`cond` switch from `+2` to align-under-
//! first-arg once an arg renders on the head's line.
//!
//! The default 80-column budget can be overridden via
//! [`render_with_width`] / [`crate::format_with_width`].

use crate::cst::{Cst, CstNode};

/// Default width budget (columns).
pub const DEFAULT_WIDTH: usize = 80;

pub fn render(cst: &Cst) -> String {
    render_with_width(cst, DEFAULT_WIDTH)
}

pub fn render_with_width(cst: &Cst, width: usize) -> String {
    let mut r = Renderer {
        out: String::new(),
        col: 0,
        budget: width,
    };
    render_top_level(&cst.nodes, &mut r);
    if !r.out.is_empty() && !r.out.ends_with('\n') {
        r.out.push('\n');
    }
    r.out
}

struct Renderer {
    out: String,
    col: usize,
    budget: usize,
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
    let mut prev_was_struct = false;
    // Bumped to 2 after a top-level definition (`defun` / `defmacro`
    // / `defvar` / `defconst` / `defspecial`) so a blank line always
    // sits between a definition and the next top-level form.
    let mut min_breaks: u32 = 0;

    for (i, node) in nodes.iter().enumerate() {
        match node {
            CstNode::LineBreak { count } => {
                // The min_breaks bump only applies when more
                // non-trivia content follows; otherwise this is a
                // trailing newline and we don't want to inflate it.
                let has_more = nodes[i + 1..]
                    .iter()
                    .any(|n| !matches!(n, CstNode::LineBreak { .. }));
                let target = if has_more {
                    (*count).max(min_breaks)
                } else {
                    *count
                };
                r.newline_then_indent(target.saturating_sub(1), 0);
                if has_more {
                    min_breaks = 0;
                }
                at_line_start = true;
                prev_was_struct = false;
            }
            CstNode::Comment { text, .. } => {
                if !at_line_start {
                    r.write(" ");
                }
                r.write(text);
                at_line_start = false;
                prev_was_struct = false;
            }
            structural => {
                if prev_was_struct && !at_line_start {
                    // Two top-level forms in a row with no user line
                    // break between them. Force one — top-level forms
                    // each get their own line per Lisp convention.
                    let target = 1u32.max(min_breaks);
                    r.newline_then_indent(target.saturating_sub(1), 0);
                    min_breaks = 0;
                    at_line_start = true;
                }
                if !at_line_start {
                    r.write(" ");
                }
                render_node(structural, r);
                at_line_start = false;
                prev_was_struct = true;
                if is_top_level_definition(structural) {
                    min_breaks = 2;
                }
            }
        }
    }
}

/// True if `node` is a list whose head is one of the top-level
/// definition forms after which a blank line is conventional.
fn is_top_level_definition(node: &CstNode) -> bool {
    let CstNode::List { children, .. } = node else {
        return false;
    };
    let head = children.iter().find_map(|c| match c {
        CstNode::Atom { text, .. } => Some(text.as_str()),
        _ => None,
    });
    matches!(
        head,
        Some(
            "defun" | "defmacro" | "defvar" | "defconst" | "defspecial"
        )
    )
}

fn render_node(node: &CstNode, r: &mut Renderer) {
    match node {
        CstNode::Atom { text, .. } => r.write(text),
        CstNode::Comment { text, .. } => r.write(text),
        CstNode::List { children, .. } => render_list(children, r),
        CstNode::ReaderMacro { prefix, inner, .. } => {
            r.write(prefix.as_str());
            render_node(inner, r);
        }
        CstNode::LineBreak { .. } => {
            debug_assert!(false, "LineBreak passed to render_node");
        }
    }
}

fn render_list(children: &[CstNode], r: &mut Renderer) {
    render_list_with_override(children, None, r);
}

/// Like [`render_list`] but lets the caller override the head's
/// "header arity" for break-decision purposes. Used by
/// [`render_let_bindings`] to force one-binding-per-line layout
/// inside a `let` / `let*` bindings list.
fn render_list_with_override(
    children: &[CstNode],
    header_override: Option<usize>,
    r: &mut Renderer,
) {
    let info = analyze_list(children);
    let fits = !info.requires_multi && r.col + info.one_line_width <= r.budget;
    let open_col = r.col;
    r.write("(");
    if fits {
        render_list_one_line(children, r);
    } else {
        // Force inserted line breaks only if the list has no user
        // breaks / comments of its own. If the user already laid the
        // list out multi-line, preserve their structure exactly —
        // descendants may still wrap independently to fit the
        // budget.
        let user_laid_out = children.iter().any(|c| {
            matches!(
                c,
                CstNode::LineBreak { .. } | CstNode::Comment { .. }
            )
        });
        render_list_multi(children, open_col, !user_laid_out, header_override, r);
    }
    r.write(")");
}

/// Render the bindings list of a `let` / `let*` form. Same shape as
/// [`render_list`] but with `header_override = Some(0)`, which makes
/// the multi-line path break before every binding so they each get
/// their own line. When the bindings list fits on one line, the
/// override has no effect — short bindings stay packed.
fn render_let_bindings(children: &[CstNode], r: &mut Renderer) {
    render_list_with_override(children, Some(0), r);
}

#[derive(Default, Clone, Copy)]
struct NodeInfo {
    one_line_width: usize,
    requires_multi: bool,
}

/// Recursively compute the one-line render width and detect any
/// user-imposed multi-line requirement (line break or comment) in
/// the subtree.
fn analyze(node: &CstNode) -> NodeInfo {
    match node {
        CstNode::Atom { text, .. } => NodeInfo {
            one_line_width: text.chars().count(),
            requires_multi: false,
        },
        CstNode::Comment { .. } | CstNode::LineBreak { .. } => NodeInfo {
            one_line_width: 0,
            requires_multi: true,
        },
        CstNode::ReaderMacro { prefix, inner, .. } => {
            let inner = analyze(inner);
            NodeInfo {
                one_line_width: prefix.as_str().chars().count() + inner.one_line_width,
                requires_multi: inner.requires_multi,
            }
        }
        CstNode::List { children, .. } => analyze_list(children),
    }
}

fn analyze_list(children: &[CstNode]) -> NodeInfo {
    let mut total_width = 2; // `(` + `)`
    let mut requires_multi = false;
    let mut first = true;
    for child in children {
        match child {
            CstNode::LineBreak { .. } | CstNode::Comment { .. } => {
                requires_multi = true;
            }
            _ => {
                let info = analyze(child);
                if info.requires_multi {
                    requires_multi = true;
                }
                if !first {
                    total_width += 1;
                }
                total_width += info.one_line_width;
                first = false;
            }
        }
    }
    NodeInfo {
        one_line_width: total_width,
        requires_multi,
    }
}

fn render_list_one_line(children: &[CstNode], r: &mut Renderer) {
    let mut first = true;
    for child in children {
        // analyze_list verified there are no LineBreak/Comment
        // nodes in this list before we entered one-line mode.
        debug_assert!(!matches!(
            child,
            CstNode::LineBreak { .. } | CstNode::Comment { .. }
        ));
        if !first {
            r.write(" ");
        }
        render_node(child, r);
        first = false;
    }
}

fn render_list_multi(
    nodes: &[CstNode],
    open_col: usize,
    force_breaks: bool,
    header_override: Option<usize>,
    r: &mut Renderer,
) {
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
                // After the form's "header" args have rendered,
                // every subsequent struct child gets a line break
                // before it — but only if we're forcing breaks
                // (i.e., the user didn't already lay this list out).
                let header_args =
                    header_override.unwrap_or_else(|| header_size(head_text.as_deref()));
                let needs_forced_break = force_breaks
                    && !at_line_start
                    && struct_count > header_args;
                if needs_forced_break {
                    let indent = compute_indent(
                        head_text.as_deref(),
                        struct_count,
                        second_col,
                        open_col,
                    );
                    r.newline_then_indent(0, indent);
                    at_line_start = true;
                }
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
                // The bindings list of a `let` / `let*` form gets a
                // header_override = 0 pass, which makes each binding
                // land on its own line when the list goes multi.
                let is_let_bindings = struct_count == 1
                    && matches!(head_text.as_deref(), Some("let" | "let*"))
                    && matches!(structural, CstNode::List { .. });
                if is_let_bindings
                    && let CstNode::List { children, .. } = structural
                {
                    render_let_bindings(children, r);
                } else {
                    render_node(structural, r);
                }
                struct_count += 1;
                at_line_start = false;
            }
        }
    }
}

/// Indent column for the next line inside a list body.
///
/// Three regimes:
///
/// 1. Before any structural child has rendered → `open_col + 1`.
/// 2. Special form with at least one header arg (let, defun, when, …)
///    → `open_col + 2`. The body always indents at +2, even after the
///    second struct child has rendered.
/// 3. Otherwise (function-call form, or `progn`-shaped `Special(0)`
///    forms once their first arg has rendered) → align under the
///    second struct child if recorded; else fall back to either
///    `open_col + 2` (Special(0) broken-before-first-arg) or
///    `open_col + 1` (Default broken-before-first-arg).
fn compute_indent(
    head: Option<&str>,
    struct_count: usize,
    second_col: Option<usize>,
    open_col: usize,
) -> usize {
    if struct_count == 0 {
        return open_col + 1;
    }
    let kind = head.map(special_kind).unwrap_or(SpecialKind::None);
    match kind {
        SpecialKind::HasHeader => open_col + 2,
        SpecialKind::ZeroHeader => second_col.unwrap_or(open_col + 2),
        SpecialKind::None => second_col.unwrap_or(open_col + 1),
    }
}

#[derive(Clone, Copy)]
enum SpecialKind {
    /// Plain function call: align continuation under the second
    /// struct child; broken-before-first-arg falls back to
    /// `open_col + 1` (Emacs default).
    None,
    /// `progn` / `cond` family — no "header" args, all args are
    /// body. Broken-before-first-arg → `open_col + 2`; once an arg
    /// has rendered, subsequent breaks align under it (same as
    /// `None`).
    ZeroHeader,
    /// `let` / `defun` family — body always indents at
    /// `open_col + 2`, even after header args have rendered.
    HasHeader,
}

fn special_kind(head: &str) -> SpecialKind {
    match head {
        "progn" | "prog1" | "prog2" | "cond" => SpecialKind::ZeroHeader,
        h if is_special_form(h) => SpecialKind::HasHeader,
        _ => SpecialKind::None,
    }
}

/// Number of structural children — counted *after* the head — that
/// stay on the head's line in multi-line layout. Args past this
/// count get a forced line break before them.
///
/// - Default function call: 1 (head + first arg fit on the opening
///   line; subsequent args break).
/// - `progn`-shaped forms: 0 (head alone on the opening line; every
///   body arg breaks).
/// - `defun` / `defmacro` / `defspecial` / `condition-case`: 2
///   (head + name + arglist / var + form fit on the opening line;
///   body breaks).
/// - All other special forms: 1.
fn header_size(head: Option<&str>) -> usize {
    let Some(head) = head else { return 1 };
    match special_kind(head) {
        SpecialKind::None => 1,
        SpecialKind::ZeroHeader => 0,
        SpecialKind::HasHeader => match head {
            "defun" | "defmacro" | "defspecial" | "condition-case" => 2,
            _ => 1,
        },
    }
}

/// Special forms whose body indents at `open_col + 2` (the
/// `HasHeader` regime in [`special_kind`]). Members have at least
/// one "header" argument (the bindings list, the function name, the
/// condition, …); the body — args after the header — indent at +2.
///
/// `progn`/`cond`/`prog1`/`prog2` aren't here: they're `ZeroHeader`
/// in `special_kind`, where broken-before-first-arg goes to +2 but
/// continuation aligns under the first arg once one has rendered.
///
/// `if` / `if-let` / `if-let*` and the threading macros (`->`, `->>`,
/// `thread-first`, `thread-last`) intentionally aren't here either:
/// lisp-data-mode treats them as plain function calls (continuation
/// lines align under the second element). User-defined
/// `(declare (indent N))` overrides aren't read yet.
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
            | "when-let"
            | "while-let"
            | "condition-case"
            | "save-excursion"
            | "save-restriction"
            | "save-window-excursion"
            | "with-current-buffer"
            | "with-temp-buffer"
            | "with-temp-file"
            | "with-output-to-string"
            | "catch"
            | "unwind-protect"
    )
}

#[cfg(test)]
mod tests {
    use crate::cst::CstNode;

    fn fmt(src: &str) -> String {
        crate::format(src).expect("parse")
    }

    fn fmt_with(src: &str, width: usize) -> String {
        crate::format_with_width(src, width).expect("parse")
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

    // -------- line wrapping --------

    #[test]
    fn one_line_when_fits() {
        // Default budget is 80 cols; this is well under.
        assert_eq!(fmt("(foo a b c)"), "(foo a b c)\n");
    }

    #[test]
    fn function_call_breaks_when_too_wide() {
        // 20 chars wide; budget 15 forces a break.
        assert_eq!(
            fmt_with("(foo arg1 arg2 arg3)", 15),
            "(foo arg1\n     arg2\n     arg3)\n"
        );
    }

    #[test]
    fn defun_breaks_with_header_on_first_line() {
        // `defun` header arity is 2 (name + arglist); body breaks.
        assert_eq!(
            fmt_with("(defun greet (name) (princ name) (newline))", 30),
            "(defun greet (name)\n  (princ name)\n  (newline))\n"
        );
    }

    #[test]
    fn progn_breaks_with_head_alone() {
        // `progn` is ZeroHeader: head on its own line, every body
        // arg breaks.
        assert_eq!(
            fmt_with("(progn step-one step-two step-three)", 18),
            "(progn\n  step-one\n  step-two\n  step-three)\n"
        );
    }

    #[test]
    fn nested_lists_decide_independently() {
        // Outer list won't fit; inner list does.
        let src = "(foo (small a) (small b) (small c))";
        assert_eq!(
            fmt_with(src, 24),
            "(foo (small a)\n     (small b)\n     (small c))\n"
        );
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
            "(if cond\n    then\n    else)",
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
}
