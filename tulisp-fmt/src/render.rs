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

use std::collections::HashMap;

use crate::cst::{Cst, CstNode};

/// Default width budget (columns).
pub const DEFAULT_WIDTH: usize = 80;

/// Default body-indent step for special forms (columns).
pub const DEFAULT_INDENT_WIDTH: usize = 2;

/// Default tab display width when `use_tabs` is on (columns).
pub const DEFAULT_TAB_WIDTH: usize = 8;

/// Knobs that control formatted output. `width` is the column budget
/// for the fit-or-break decision. `indent_width` is the body-indent
/// step for `let` / `defun` / etc. — the structural "+1 space before
/// first arg" used by plain function calls always stays at 1
/// regardless. `use_tabs` switches indent emission from spaces to
/// `\t` runs of `tab_width` columns each, with any sub-tab remainder
/// padded with spaces — matching Emacs's `indent-tabs-mode`.
///
/// `alist_one_per_line` renders any list whose every structural
/// child is a dotted pair (`(k . v)`) with one pair per line, even
/// when the whole list would fit on one line. Set to false to keep
/// the default fits-or-breaks behavior.
///
/// `final_newline` (default true) ensures the output ends with `\n`,
/// which is what files want. Set to false for snippet rendering
/// (embedding a value into a `<pre>`, a textarea, a stack-trace
/// line) so the consumer doesn't have to `trim_end` themselves.
#[derive(Clone, Copy, Debug)]
pub struct Style {
    pub width: usize,
    pub indent_width: usize,
    pub use_tabs: bool,
    pub tab_width: usize,
    pub alist_one_per_line: bool,
    pub final_newline: bool,
}

impl Default for Style {
    fn default() -> Self {
        Self {
            width: DEFAULT_WIDTH,
            indent_width: DEFAULT_INDENT_WIDTH,
            use_tabs: false,
            tab_width: DEFAULT_TAB_WIDTH,
            alist_one_per_line: true,
            final_newline: true,
        }
    }
}

pub fn render(cst: &Cst) -> String {
    render_with_style(cst, &Style::default())
}

pub fn render_with_width(cst: &Cst, width: usize) -> String {
    render_with_style(
        cst,
        &Style {
            width,
            ..Style::default()
        },
    )
}

pub fn render_with_style(cst: &Cst, style: &Style) -> String {
    let mut r = Renderer {
        out: String::new(),
        col: 0,
        style: *style,
        user_indent: collect_indent_declarations(cst),
    };
    render_top_level(&cst.nodes, &mut r);
    if style.final_newline && !r.out.is_empty() && !r.out.ends_with('\n') {
        r.out.push('\n');
    }
    let mut aligned = align_trailing_comments(&r.out);
    if !style.final_newline {
        // Strip every trailing `\n` so snippet consumers don't have
        // to `trim_end` themselves. Last-line user line-breaks would
        // otherwise leak through as a trailing newline.
        let trimmed_len = aligned.trim_end_matches('\n').len();
        aligned.truncate(trimmed_len);
    }
    aligned
}

/// Walk top-level forms for `(defmacro NAME ... (declare (indent N))
/// ...)` and return a map from macro name to N. Used by the renderer
/// to indent calls to user-defined macros the same way the macro
/// author asked, matching Emacs's behavior. The body of `defmacro`
/// is searched left-to-right for the first `declare` form so the
/// canonical placement (immediately after the arglist) is honored
/// without forcing it.
fn collect_indent_declarations(cst: &Cst) -> HashMap<String, usize> {
    let mut out = HashMap::new();
    for node in &cst.nodes {
        if let Some((name, n)) = parse_defmacro_indent(node) {
            out.insert(name, n);
        }
    }
    out
}

fn parse_defmacro_indent(node: &CstNode) -> Option<(String, usize)> {
    let CstNode::List { children, .. } = node else {
        return None;
    };
    let structural: Vec<&CstNode> = children
        .iter()
        .filter(|c| !matches!(c, CstNode::LineBreak { .. } | CstNode::Comment { .. }))
        .collect();
    let head = match structural.first()? {
        CstNode::Atom { text, .. } => text.as_str(),
        _ => return None,
    };
    if head != "defmacro" {
        return None;
    }
    let name = match structural.get(1)? {
        CstNode::Atom { text, .. } => text.clone(),
        _ => return None,
    };
    // Body forms start at index 3 (after head, name, arglist).
    for body in structural.iter().skip(3) {
        if let Some(n) = read_indent_declaration(body) {
            return Some((name, n));
        }
    }
    None
}

/// Recognise a `(declare ... (indent N) ...)` form and return N, or
/// `None` for anything else. Other declarations (`debug`, `doc-string`,
/// arbitrary user spec items) are ignored.
fn read_indent_declaration(node: &CstNode) -> Option<usize> {
    let CstNode::List { children, .. } = node else {
        return None;
    };
    let structural: Vec<&CstNode> = children
        .iter()
        .filter(|c| !matches!(c, CstNode::LineBreak { .. } | CstNode::Comment { .. }))
        .collect();
    let head = match structural.first()? {
        CstNode::Atom { text, .. } => text.as_str(),
        _ => return None,
    };
    if head != "declare" {
        return None;
    }
    for spec in structural.iter().skip(1) {
        let CstNode::List { children: sc, .. } = spec else {
            continue;
        };
        let ss: Vec<&CstNode> = sc
            .iter()
            .filter(|c| !matches!(c, CstNode::LineBreak { .. } | CstNode::Comment { .. }))
            .collect();
        if let Some(CstNode::Atom { text, .. }) = ss.first()
            && text == "indent"
            && let Some(CstNode::Atom { text: n_text, .. }) = ss.get(1)
            && let Ok(n) = n_text.parse::<usize>()
        {
            return Some(n);
        }
    }
    None
}

/// Walk the rendered output and pad consecutive lines that end with
/// a `;`-comment so that all `;`s line up on the same column. A
/// "run" is a contiguous group of such lines; runs end at any line
/// without a trailing comment (blank lines, code-only lines,
/// block-comment-only lines all break a run). Single-line runs are
/// left alone — alignment is meaningful only for ≥ 2 lines.
fn align_trailing_comments(out: &str) -> String {
    let lines: Vec<&str> = out.split_inclusive('\n').collect();
    let cols: Vec<Option<usize>> = lines
        .iter()
        .map(|l| {
            let trimmed = l.strip_suffix('\n').unwrap_or(l);
            find_trailing_comment_col(trimmed)
        })
        .collect();

    let mut result = String::with_capacity(out.len());
    let mut i = 0;
    while i < lines.len() {
        if cols[i].is_none() {
            result.push_str(lines[i]);
            i += 1;
            continue;
        }
        let mut j = i + 1;
        while j < lines.len() && cols[j].is_some() {
            j += 1;
        }
        if j - i < 2 {
            result.push_str(lines[i]);
            i = j;
            continue;
        }
        let target = cols[i..j].iter().filter_map(|c| *c).max().unwrap();
        for k in i..j {
            let line = lines[k];
            let line_body = line.strip_suffix('\n').unwrap_or(line);
            let col = cols[k].unwrap();
            result.push_str(&line_body[..col]);
            for _ in col..target {
                result.push(' ');
            }
            result.push_str(&line_body[col..]);
            if line.ends_with('\n') {
                result.push('\n');
            }
        }
        i = j;
    }
    result
}

/// Return the byte column at which a trailing `;`-comment begins on
/// `line`, or `None` if the line has no trailing comment. Lines whose
/// first non-whitespace char is `;` are *block* comments — they don't
/// participate in trailing-comment alignment.
fn find_trailing_comment_col(line: &str) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut in_string = false;
    let mut prev_backslash = false;
    let mut saw_code = false;
    for (i, &b) in bytes.iter().enumerate() {
        if !in_string {
            if !saw_code {
                if b == b' ' || b == b'\t' {
                    continue;
                }
                if b == b';' {
                    return None;
                }
                saw_code = true;
            }
            if b == b'"' {
                in_string = true;
            } else if b == b';' {
                return Some(i);
            }
        } else if !prev_backslash && b == b'"' {
            in_string = false;
        }
        prev_backslash = b == b'\\' && !prev_backslash;
    }
    None
}

struct Renderer {
    out: String,
    col: usize,
    style: Style,
    /// `(declare (indent N))` declarations harvested from top-level
    /// `defmacro` forms in the same input. Indexed by macro name.
    user_indent: HashMap<String, usize>,
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
        let (tabs, spaces) = if self.style.use_tabs && self.style.tab_width > 0 {
            (indent / self.style.tab_width, indent % self.style.tab_width)
        } else {
            (0, indent)
        };
        for _ in 0..tabs {
            self.out.push('\t');
        }
        for _ in 0..spaces {
            self.out.push(' ');
        }
        self.col = indent;
    }

    fn budget(&self) -> usize {
        self.style.width
    }

    fn indent_step(&self) -> usize {
        self.style.indent_width
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
        Some("defun" | "defmacro" | "defvar" | "defconst" | "defspecial")
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
    // Dotted pair: keep on one line whenever it has no user breaks /
    // comments forcing it apart. Breaking inside `(k . v)` produces
    // ugly output (the formatter would align under the `.`) and
    // serves no purpose — the pair is a structural unit.
    let is_pair = is_dotted_pair(children) && !info.requires_multi;
    // Alist: every structural child is a dotted pair. With the
    // style flag set (default), force one pair per line even when
    // the whole list would fit on a single line.
    let force_alist_multi = r.style.alist_one_per_line && !is_pair && is_alist(children);
    let fits = is_pair
        || (!force_alist_multi
            && !info.requires_multi
            && r.col + info.one_line_width <= r.budget());
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
        let user_laid_out = children
            .iter()
            .any(|c| matches!(c, CstNode::LineBreak { .. } | CstNode::Comment { .. }));
        // Alists override the header arity to 0 so every cons cell
        // gets its own line; built-in special forms keep theirs.
        let effective_override = if force_alist_multi {
            Some(0)
        } else {
            header_override
        };

        // For default function calls (`SpecialKind::None`), the
        // aligned-under-first-arg multi-line layout sometimes
        // marches the indent off the right edge — the cascade
        // happens for deeply nested forms or long heads. Try
        // aligned first, measure the max column we wrote to, and
        // if it overran the budget, roll back and re-render
        // hanging (every arg breaks onto its own line at
        // `open_col + indent_step`). Special forms (`let` /
        // `defun` / `progn` / …) already hang via SpecialKind, so
        // they don't need the trial.
        let head_text = first_atom_text(children);
        let kind = head_text
            .map(|h| special_kind(h, &r.user_indent))
            .unwrap_or(SpecialKind::None);
        let allow_hanging_fallback =
            matches!(kind, SpecialKind::None) && !user_laid_out && !force_alist_multi;

        if allow_hanging_fallback {
            let snap_len = r.out.len();
            let snap_col = r.col;
            render_list_multi(
                children,
                open_col,
                !user_laid_out,
                effective_override,
                false,
                r,
            );
            let max_col = max_appended_col(&r.out, snap_len, snap_col);
            // +1 accounts for the closing `)` we'll write after
            // the trial finishes — it lands on the last content
            // line and adds one column to its width.
            if max_col + 1 > r.budget() {
                r.out.truncate(snap_len);
                r.col = snap_col;
                render_list_multi(children, open_col, !user_laid_out, Some(0), true, r);
            }
        } else {
            render_list_multi(
                children,
                open_col,
                !user_laid_out,
                effective_override,
                false,
                r,
            );
        }
    }
    r.write(")");
}

/// Text of the first atom child (the form's head), or None for an
/// empty list / a list whose head isn't an atom.
fn first_atom_text(children: &[CstNode]) -> Option<&str> {
    for c in children {
        match c {
            CstNode::LineBreak { .. } | CstNode::Comment { .. } => continue,
            CstNode::Atom { text, .. } => return Some(text.as_str()),
            _ => return None,
        }
    }
    None
}

/// Maximum column reached while writing the bytes appended to
/// `out` since `snap_len`, given the renderer's column was
/// `start_col` at the snapshot. Matches `Renderer::write`'s
/// per-char column accounting (every non-`\n` char advances col
/// by 1; `\n` resets to 0).
fn max_appended_col(out: &str, snap_len: usize, start_col: usize) -> usize {
    let mut col = start_col;
    let mut max = start_col;
    for c in out[snap_len..].chars() {
        if c == '\n' {
            col = 0;
        } else {
            col += 1;
            if col > max {
                max = col;
            }
        }
    }
    max
}

/// True if `children` represents a dotted pair — i.e. contains a `.`
/// atom as a structural element separating the head from the tail.
fn is_dotted_pair(children: &[CstNode]) -> bool {
    children
        .iter()
        .any(|c| matches!(c, CstNode::Atom { text, .. } if text == "."))
}

/// True if `children` represents an alist: at least two structural
/// children, every one of them a list that is itself a dotted pair.
fn is_alist(children: &[CstNode]) -> bool {
    let mut count = 0usize;
    for child in children {
        match child {
            CstNode::LineBreak { .. } | CstNode::Comment { .. } => {}
            CstNode::List { children, .. } if is_dotted_pair(children) => {
                count += 1;
            }
            _ => return false,
        }
    }
    count >= 2
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
    force_hanging: bool,
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
                    r.indent_step(),
                    &r.user_indent,
                    force_hanging,
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
                let header_args = header_override
                    .unwrap_or_else(|| header_size(head_text.as_deref(), &r.user_indent));
                let needs_forced_break =
                    force_breaks && !at_line_start && struct_count > header_args;
                if needs_forced_break {
                    let indent = compute_indent(
                        head_text.as_deref(),
                        struct_count,
                        second_col,
                        open_col,
                        r.indent_step(),
                        &r.user_indent,
                        force_hanging,
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
                if is_let_bindings && let CstNode::List { children, .. } = structural {
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
///    → `open_col + indent_step`. The body always indents one step,
///    even after the second struct child has rendered.
/// 3. Otherwise (function-call form, or `progn`-shaped `Special(0)`
///    forms once their first arg has rendered) → align under the
///    second struct child if recorded; else fall back to either
///    `open_col + indent_step` (Special(0) broken-before-first-arg)
///    or `open_col + 1` (Default broken-before-first-arg — the "+1"
///    is structural and stays at one space regardless of the
///    body-indent step).
fn compute_indent(
    head: Option<&str>,
    struct_count: usize,
    second_col: Option<usize>,
    open_col: usize,
    indent_step: usize,
    user_indent: &HashMap<String, usize>,
    force_hanging: bool,
) -> usize {
    if struct_count == 0 {
        return open_col + 1;
    }
    // `force_hanging` is set when `render_list_with_override`
    // determined an aligned-under-first-arg layout would overflow
    // the budget. Treat the form as `ZeroHeader` (every arg
    // breaks onto its own line at `open_col + indent_step`)
    // regardless of its real `SpecialKind`.
    let kind = if force_hanging {
        SpecialKind::ZeroHeader
    } else {
        head.map(|h| special_kind(h, user_indent))
            .unwrap_or(SpecialKind::None)
    };
    match kind {
        SpecialKind::HasHeader => open_col + indent_step,
        SpecialKind::ZeroHeader => second_col.unwrap_or(open_col + indent_step),
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

fn special_kind(head: &str, user_indent: &HashMap<String, usize>) -> SpecialKind {
    if let Some(&n) = user_indent.get(head) {
        return if n == 0 {
            SpecialKind::ZeroHeader
        } else {
            SpecialKind::HasHeader
        };
    }
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
fn header_size(head: Option<&str>, user_indent: &HashMap<String, usize>) -> usize {
    let Some(head) = head else { return 1 };
    if let Some(&n) = user_indent.get(head) {
        return n;
    }
    match special_kind(head, user_indent) {
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

    fn fmt_with_style(src: &str, style: &super::Style) -> String {
        crate::format_with_style(src, style).expect("parse")
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
        assert_eq!(fmt(src), "(let ((x 1))\n  (foo x\n       y))\n");
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
        assert_eq!(fmt("(foo) ; trailing\n(bar)"), "(foo) ; trailing\n(bar)\n");
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
        assert_eq!(
            crate::format("(#x1A ?\\n \"hi\")").unwrap(),
            "(#x1A ?\\n \"hi\")\n"
        );
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

    #[test]
    fn indent_width_four_doubles_body_indent() {
        let style = super::Style {
            width: 80,
            indent_width: 4,
            ..super::Style::default()
        };
        // `let` is HasHeader, so the body indents at open_col + 4
        // instead of the default + 2. The "+1" before the bindings
        // list stays at one space — that's structural, not an indent
        // step.
        let out = fmt_with_style("(let ((x 1))\n  body)", &style);
        assert_eq!(out, "(let ((x 1))\n    body)\n");
    }

    #[test]
    fn use_tabs_emits_tabs_for_indent() {
        let style = super::Style {
            indent_width: 8,
            use_tabs: true,
            tab_width: 8,
            ..super::Style::default()
        };
        // Body indent of 8 with tab-width 8 should be exactly one tab.
        let out = fmt_with_style("(let ((x 1))\n        body)", &style);
        assert_eq!(out, "(let ((x 1))\n\tbody)\n");
    }

    #[test]
    fn declare_indent_one_makes_macro_a_when_alike() {
        // Define a macro with `(declare (indent 1))` and then call
        // it. The body should indent at +2 of the open-paren even
        // when broken before it — same shape as `when`.
        let src = "\
(defmacro my-when (cond &rest body) (declare (indent 1)) `(if ,cond (progn ,@body)))

(my-when (foo)
  (do-this)
  (do-that))
";
        let out = fmt(src);
        assert!(
            out.contains("(my-when (foo)\n  (do-this)\n  (do-that))"),
            "got:\n{out}"
        );
    }

    #[test]
    fn declare_indent_zero_makes_macro_a_progn_alike() {
        // (declare (indent 0)) → ZeroHeader; head sits alone, every
        // body arg breaks.
        let src = "\
(defmacro my-progn (&rest body) (declare (indent 0)) `(progn ,@body))

(my-progn (a) (b) (c))
";
        let out = fmt_with(src, 18);
        // Force narrow width so the call wraps. `my-progn` head sits
        // alone, every arg breaks under it.
        assert!(out.contains("(my-progn\n"), "got:\n{out}");
    }

    #[test]
    fn final_newline_off_strips_trailing_newlines_for_snippets() {
        let style = super::Style {
            final_newline: false,
            ..super::Style::default()
        };
        // Multi-line output: the inserted line breaks survive, only
        // the trailing one is stripped.
        let out = fmt_with_style("'((a . 1) (b . 2))", &style);
        assert_eq!(out, "'((a . 1)\n  (b . 2))");

        // Single-line output: no trailing newline.
        let out = fmt_with_style("(foo bar)", &style);
        assert_eq!(out, "(foo bar)");

        // Empty input stays empty.
        let out = fmt_with_style("", &style);
        assert_eq!(out, "");

        // User-supplied trailing blank lines are also stripped — the
        // snippet caller wants a clean tail.
        let out = fmt_with_style("(foo)\n\n", &style);
        assert_eq!(out, "(foo)");
    }

    #[test]
    fn final_newline_on_is_default_behavior() {
        // Sanity: default still produces a trailing newline.
        assert!(fmt("(foo)").ends_with('\n'));
    }

    #[test]
    fn dotted_pair_stays_on_one_line_under_narrow_width() {
        // (banana . yellow) is 17 chars; at width 12 it would
        // normally break, but dotted pairs are atomic.
        let out = fmt_with("(banana . yellow)", 12);
        assert_eq!(out, "(banana . yellow)\n");
    }

    #[test]
    fn alist_breaks_one_pair_per_line_by_default() {
        let out = fmt("'((a . 1) (b . 2))");
        assert_eq!(out, "'((a . 1)\n  (b . 2))\n");
    }

    #[test]
    fn alist_one_per_line_can_be_disabled() {
        let style = super::Style {
            alist_one_per_line: false,
            ..super::Style::default()
        };
        let out = fmt_with_style("'((a . 1) (b . 2))", &style);
        assert_eq!(out, "'((a . 1) (b . 2))\n");
    }

    #[test]
    fn three_pair_alist_aligns_under_first() {
        let out = fmt("'((a . 1) (b . 2) (c . 3))");
        assert_eq!(out, "'((a . 1)\n  (b . 2)\n  (c . 3))\n");
    }

    #[test]
    fn aligns_consecutive_trailing_comments() {
        let src = "(setq x 1) ; first\n(setq long 22) ; second\n(setq y 333) ; third\n";
        let out = fmt(src);
        let expected = "\
(setq x 1)     ; first
(setq long 22) ; second
(setq y 333)   ; third
";
        assert_eq!(out, expected);
    }

    #[test]
    fn block_comments_dont_align_with_trailing() {
        // The `;; line on its own` is a block comment; the next two
        // lines share a trailing-comment alignment that's
        // independent of it.
        let src = "(foo) ; a\n;; standalone\n(bar) ; b\n(baz) ; cc\n";
        let out = fmt(src);
        let expected = "\
(foo) ; a
;; standalone
(bar) ; b
(baz) ; cc
";
        assert_eq!(out, expected);
    }

    #[test]
    fn semicolons_inside_strings_dont_count() {
        let src = "(princ \"hi ; not\") ; real\n(princ \"x\") ; yep\n";
        let out = fmt(src);
        let expected = "\
(princ \"hi ; not\") ; real
(princ \"x\")        ; yep
";
        assert_eq!(out, expected);
    }

    #[test]
    fn use_tabs_pads_remainder_with_spaces() {
        let style = super::Style {
            use_tabs: true,
            tab_width: 4,
            ..super::Style::default()
        };
        // Outer (when …) opens at col 0, body at col 2 → 0 tabs + 2
        // spaces. Inner (when …) opens at col 2 (one space after the
        // outer head + first arg), body at col 4 → 1 tab + 0 spaces.
        let out = fmt_with_style("(when a\n  (when b\n    body))", &style);
        assert_eq!(out, "(when a\n  (when b\n\tbody))\n");
    }

    /// When laying out aligned-under-first-arg would push a child
    /// past the budget, every arg breaks to its own line at
    /// `open_col + indent_step` instead of cascading right.
    #[test]
    fn falls_back_to_hanging_when_aligned_overflows() {
        // Aligned would put each arg at col 5 ending col 13 — 1
        // past the budget once the closing `)` is added. Hanging
        // indents at col 2, ends col 10, and fits cleanly.
        let out = fmt_with("(foo arg-aaaa arg-bbbb arg-cccc)", 12);
        assert_eq!(out, "(foo\n  arg-aaaa\n  arg-bbbb\n  arg-cccc)\n");
    }

    /// A form whose continuation args fit at aligned_col stays
    /// aligned — the fallback only fires when aligned would
    /// overflow.
    #[test]
    fn aligned_layout_kept_when_continuation_args_fit() {
        // (foo a b c) doesn't fit width 8 one-line, but laid out
        // aligned the widest line (`     c)`) is 7 chars, leaving
        // budget room for the closing paren.
        let out = fmt_with("(foo a b c)", 8);
        assert_eq!(out, "(foo a\n     b\n     c)\n");
    }

    /// Special forms (`let`, `when`, etc.) already hang at
    /// `open_col + indent_step`; the fallback shouldn't perturb
    /// their layout.
    #[test]
    fn special_forms_unchanged_by_aligned_fallback() {
        // Body args of (when …) indent at +2, not under the
        // condition.
        let out = fmt_with("(when condition body-form-1 body-form-2)", 30);
        assert_eq!(out, "(when condition\n  body-form-1\n  body-form-2)\n");
    }
}
