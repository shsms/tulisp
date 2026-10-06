//! A syntax tree of Lisp source, for editor tools.
//!
//! [`read`] reads any text, finished or not, into a [`SyntaxTree`] that keeps
//! every character: comments, the exact text of literals, and where each form
//! starts and ends, in bytes. Text that cannot be read becomes an error node,
//! and reading goes on after it, so code being typed still has a tree. The tree
//! holds the forms the evaluator's parser reads; it adds only what that parser
//! drops.

use std::ops::Range;

use crate::parse::{Token, Tokenizer};

/// A node of a [`SyntaxTree`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

/// The character or characters that make a prefixed form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Prefix {
    /// `'`
    Quote,
    /// `` ` ``
    Backquote,
    /// `,`
    Comma,
    /// `,@`
    Splice,
    /// `#'`
    Function,
}

impl Prefix {
    /// The prefix as written.
    pub fn text(self) -> &'static str {
        match self {
            Prefix::Quote => "'",
            Prefix::Backquote => "`",
            Prefix::Comma => ",",
            Prefix::Splice => ",@",
            Prefix::Function => "#'",
        }
    }
}

/// What kind of name or literal an atom is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum AtomKind {
    Symbol,
    Integer,
    Float,
    String,
    /// A character literal such as `?a`, which reads as an integer.
    Character,
}

/// What a node is.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum NodeKind {
    /// `(...)`, holding its forms, dots and comments in order. `closed` is
    /// false when the input ended before its `)`.
    List { children: Vec<NodeId>, closed: bool },
    /// A prefixed form, such as `'x`. `children` is everything in the prefixed
    /// form's place, in order: comments and the form. `child` is the form
    /// itself, `None` when no form followed the prefix.
    Prefix {
        prefix: Prefix,
        children: Vec<NodeId>,
        child: Option<NodeId>,
    },
    /// A name or a literal.
    Atom(AtomKind),
    /// The `.` of a dotted list.
    Dot,
    /// A `;` comment, up to its newline.
    Comment,
    /// Text that could not be read; [`SyntaxTree::errors`] says why.
    Error,
}

/// A node: what it is, where its text is, and the list or prefix it is in.
#[derive(Clone, Debug)]
pub struct Node {
    kind: NodeKind,
    range: Range<usize>,
    parent: Option<NodeId>,
}

impl Node {
    /// What the node is.
    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }

    /// Where the node's text is in the source, in bytes. An unclosed list runs
    /// to the end of the input.
    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    /// The list or prefix the node is in; `None` at the top level.
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }
}

/// A place where the source cannot be read, and why.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct SyntaxError {
    pub range: Range<usize>,
    pub message: String,
}

/// A call around an offset: the list, and which of its forms the offset is in
/// or before, counting from 0 for the function's name. Comments are not forms.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct CallSite {
    pub list: NodeId,
    pub position: usize,
}

/// Lisp source, read by [`read`].
#[derive(Clone, Debug)]
pub struct SyntaxTree<'a> {
    source: &'a str,
    nodes: Vec<Node>,
    roots: Vec<NodeId>,
    errors: Vec<SyntaxError>,
}

impl<'a> SyntaxTree<'a> {
    /// The text the tree was read from.
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// OFFSET moved into the source and back to the start of the character it
    /// is in, so it can slice the source.
    pub fn clamp(&self, offset: usize) -> usize {
        let mut offset = offset.min(self.source.len());
        while !self.source.is_char_boundary(offset) {
            offset -= 1;
        }
        offset
    }

    /// The top-level nodes, in order.
    pub fn roots(&self) -> &[NodeId] {
        &self.roots
    }

    /// The node ID names.
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0]
    }

    /// The text of the node ID names.
    pub fn text(&self, id: NodeId) -> &'a str {
        &self.source[self.nodes[id.0].range.clone()]
    }

    /// The nodes in ID: a list's children, or a prefix's form.
    pub fn children(&self, id: NodeId) -> &[NodeId] {
        match &self.nodes[id.0].kind {
            NodeKind::List { children, .. } => children,
            NodeKind::Prefix { children, .. } => children,
            NodeKind::Atom(_) | NodeKind::Dot | NodeKind::Comment | NodeKind::Error => &[],
        }
    }

    /// A list's forms: its children without the comments.
    pub fn forms(&self, id: NodeId) -> Vec<NodeId> {
        self.children(id)
            .iter()
            .copied()
            .filter(|child| !matches!(self.nodes[child.0].kind, NodeKind::Comment))
            .collect()
    }

    /// The nodes that hold OFFSET, from a top-level node down to the innermost.
    /// A list holds the offsets between its parentheses, and an unclosed list
    /// those up to the end of the input. A string holds the offsets between its
    /// quotes, and a comment those after its `;`. Any other node holds the
    /// offsets from its start to its end, both included, so the end of a symbol
    /// is in the symbol.
    pub fn path_at(&self, offset: usize) -> Vec<NodeId> {
        let mut path = Vec::new();
        let mut candidates: &[NodeId] = &self.roots;
        while let Some(&id) = candidates.iter().find(|&&id| self.holds(id, offset)) {
            path.push(id);
            candidates = self.children(id);
        }
        path
    }

    fn holds(&self, id: NodeId, offset: usize) -> bool {
        let node = &self.nodes[id.0];
        let range = &node.range;
        match &node.kind {
            NodeKind::List { closed: true, .. } | NodeKind::Atom(AtomKind::String) => {
                range.start < offset && offset < range.end
            }
            NodeKind::List { closed: false, .. } | NodeKind::Prefix { .. } | NodeKind::Comment => {
                range.start < offset && offset <= range.end
            }
            NodeKind::Atom(_) | NodeKind::Dot | NodeKind::Error => {
                range.start <= offset && offset <= range.end
            }
        }
    }

    /// The innermost list around OFFSET, as a call, and which of its forms the
    /// offset is at. `None` at the top level, or in a string or a comment.
    pub fn call_at(&self, offset: usize) -> Option<CallSite> {
        let path = self.path_at(offset);
        if let Some(&last) = path.last() {
            let in_text = match self.nodes[last.0].kind {
                NodeKind::Atom(AtomKind::String) | NodeKind::Comment => true,
                // A string with no closing quote is an error node; the offset
                // must be after its opening quote.
                NodeKind::Error => {
                    self.text(last).starts_with('"') && self.nodes[last.0].range.start < offset
                }
                NodeKind::List { .. }
                | NodeKind::Prefix { .. }
                | NodeKind::Atom(_)
                | NodeKind::Dot => false,
            };
            if in_text {
                return None;
            }
        }
        let list = path
            .iter()
            .rev()
            .copied()
            .find(|id| matches!(self.nodes[id.0].kind, NodeKind::List { .. }))?;
        let position = self
            .forms(list)
            .iter()
            .take_while(|form| self.nodes[form.0].range.end < offset)
            .count();
        Some(CallSite { list, position })
    }

    /// Where the source cannot be read, in order.
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    /// Every node in the tree.
    pub fn ids(&self) -> impl Iterator<Item = NodeId> + '_ {
        (0..self.nodes.len()).map(NodeId)
    }
}

/// Reads SOURCE into a syntax tree. It never fails: text it cannot read becomes
/// error nodes, listed in [`SyntaxTree::errors`]. Nesting deeper than a new
/// context's parser accepts is one of those errors.
pub fn read(source: &str) -> SyntaxTree<'_> {
    read_with_limit(source, crate::context::default_max_nesting_depth() as usize)
}

/// [`read`], with LIMIT as the deepest nesting it accepts.
pub(crate) fn read_with_limit(source: &str, limit: usize) -> SyntaxTree<'_> {
    Builder {
        source,
        tokens: Tokenizer::new(0, source).with_comments(),
        nodes: Vec::new(),
        roots: Vec::new(),
        errors: Vec::new(),
        open: Vec::new(),
        limit,
    }
    .run()
}

/// The value of TEXT, a string literal with its quotes, escapes read.
pub(crate) fn string_value(text: &str) -> Option<String> {
    match Tokenizer::new(0, text).next() {
        Some(Token::String { value, .. }) => Some(value),
        _ => None,
    }
}

/// The byte offset of a parser position: a 1-based line, and a 1-based column
/// counted in characters.
fn offset_of(source: &str, (line, column): (usize, usize)) -> usize {
    let line_start = if line <= 1 {
        0
    } else {
        source
            .match_indices('\n')
            .nth(line - 2)
            .map_or(source.len(), |(at, _)| at + 1)
    };
    source[line_start..]
        .char_indices()
        .nth(column.saturating_sub(1))
        .map_or(source.len(), |(at, _)| line_start + at)
}

/// Builds a tree from the tokens, with the lists and prefixes still open on a
/// stack instead of in recursion, so no input can overflow the native stack.
struct Builder<'a> {
    source: &'a str,
    tokens: Tokenizer<'a>,
    nodes: Vec<Node>,
    roots: Vec<NodeId>,
    errors: Vec<SyntaxError>,
    /// The lists and prefixes still open, innermost last.
    open: Vec<NodeId>,
    limit: usize,
}

impl<'a> Builder<'a> {
    fn run(mut self) -> SyntaxTree<'a> {
        while let Some(token) = self.tokens.next() {
            let range = self.tokens.token_range();
            self.token(token, range);
        }
        let end = self.source.len();
        while let Some(top) = self.open.pop() {
            let range = self.nodes[top.0].range.clone();
            if matches!(self.nodes[top.0].kind, NodeKind::Prefix { .. }) {
                self.error(range, "Unexpected EOF");
            } else {
                self.error(range.start..range.start + 1, "Unclosed list");
                self.nodes[top.0].range.end = end;
            }
            self.attach_value(top);
        }
        self.errors.sort_by_key(|error| error.range.start);
        SyntaxTree {
            source: self.source,
            nodes: self.nodes,
            roots: self.roots,
            errors: self.errors,
        }
    }

    fn token(&mut self, token: Token, range: Range<usize>) {
        match token {
            Token::Comment => {
                let id = self.add(NodeKind::Comment, range);
                self.attach_comment(id);
            }
            Token::CloseParen { .. } => self.close(range),
            Token::Dot { .. } => self.dot(range),
            // The parser counts every form it enters, lists and prefixes and
            // atoms, against its limit.
            token if self.open.len() >= self.limit => self.too_deep(&token, range),
            Token::OpenParen { .. } => {
                let list = NodeKind::List {
                    children: Vec::new(),
                    closed: false,
                };
                let id = self.add(list, range);
                self.open.push(id);
            }
            Token::Quote { .. } => self.open_prefix(Prefix::Quote, range),
            Token::Backtick { .. } => self.open_prefix(Prefix::Backquote, range),
            Token::Comma { .. } => self.open_prefix(Prefix::Comma, range),
            Token::Splice { .. } => self.open_prefix(Prefix::Splice, range),
            Token::SharpQuote { .. } => self.open_prefix(Prefix::Function, range),
            Token::String { .. } => self.atom(AtomKind::String, range),
            Token::Integer { .. } if self.source[range.clone()].starts_with('?') => {
                self.atom(AtomKind::Character, range)
            }
            Token::Integer { .. } => self.atom(AtomKind::Integer, range),
            Token::Float { .. } => self.atom(AtomKind::Float, range),
            Token::Ident { .. } => self.atom(AtomKind::Symbol, range),
            Token::ParserError(err) => {
                // The error is where the tokenizer found it, such as a bad
                // escape inside a string; the node covers the whole token.
                let at = offset_of(self.source, err.span.start).clamp(range.start, range.end);
                self.error(at..range.end, err.desc);
                let id = self.add(NodeKind::Error, range);
                self.attach_value(id);
            }
        }
    }

    fn add(&mut self, kind: NodeKind, range: Range<usize>) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(Node {
            kind,
            range,
            parent: None,
        });
        id
    }

    fn error(&mut self, range: Range<usize>, message: impl Into<String>) {
        self.errors.push(SyntaxError {
            range,
            message: message.into(),
        });
    }

    fn atom(&mut self, kind: AtomKind, range: Range<usize>) {
        let id = self.add(NodeKind::Atom(kind), range);
        self.attach_value(id);
    }

    fn open_prefix(&mut self, prefix: Prefix, range: Range<usize>) {
        let id = self.add(
            NodeKind::Prefix {
                prefix,
                children: Vec::new(),
                child: None,
            },
            range,
        );
        self.open.push(id);
    }

    /// Makes ID a child of PARENT, a list, or a top-level node.
    fn push_child(&mut self, parent: Option<NodeId>, id: NodeId) {
        self.nodes[id.0].parent = parent;
        match parent {
            Some(parent) => {
                if let NodeKind::List { children, .. } = &mut self.nodes[parent.0].kind {
                    children.push(id);
                }
            }
            None => self.roots.push(id),
        }
    }

    /// Adds a finished form: it completes the prefixes waiting for one,
    /// innermost first, and the list under them, if any, takes the result.
    fn attach_value(&mut self, mut id: NodeId) {
        while let Some(&top) = self.open.last() {
            if !matches!(self.nodes[top.0].kind, NodeKind::Prefix { .. }) {
                self.push_child(Some(top), id);
                return;
            }
            if let NodeKind::Prefix {
                children, child, ..
            } = &mut self.nodes[top.0].kind
            {
                *child = Some(id);
                children.push(id);
            }
            let end = self.nodes[id.0].range.end;
            self.nodes[id.0].parent = Some(top);
            self.nodes[top.0].range.end = end;
            self.open.pop();
            id = top;
        }
        self.push_child(None, id);
    }

    /// Adds a comment to the innermost open list or prefix, or the top level: a
    /// comment is not the form a prefix waits for.
    fn attach_comment(&mut self, id: NodeId) {
        let Some(&top) = self.open.last() else {
            self.push_child(None, id);
            return;
        };
        self.nodes[id.0].parent = Some(top);
        match &mut self.nodes[top.0].kind {
            NodeKind::List { children, .. } | NodeKind::Prefix { children, .. } => {
                children.push(id)
            }
            _ => {}
        }
    }

    fn dot(&mut self, range: Range<usize>) {
        let after_dot = |tree: &Self, top: NodeId| match &tree.nodes[top.0].kind {
            NodeKind::List { children, .. } => children
                .iter()
                .any(|c| matches!(tree.nodes[c.0].kind, NodeKind::Dot)),
            _ => false,
        };
        match self.open.last() {
            Some(&top)
                if matches!(self.nodes[top.0].kind, NodeKind::List { .. })
                    && !after_dot(self, top) =>
            {
                let id = self.add(NodeKind::Dot, range);
                self.push_child(Some(top), id);
            }
            _ => {
                self.error(range.clone(), "Unexpected dot");
                let id = self.add(NodeKind::Error, range);
                self.attach_value(id);
            }
        }
    }

    fn close(&mut self, range: Range<usize>) {
        // A prefix that nothing followed before the `)`.
        let mut reported = false;
        while let Some(&top) = self.open.last()
            && matches!(self.nodes[top.0].kind, NodeKind::Prefix { .. })
        {
            self.error(range.clone(), "Unexpected closing parenthesis");
            reported = true;
            self.open.pop();
            self.attach_value(top);
        }
        let Some(list) = self.open.pop() else {
            if !reported {
                self.error(range.clone(), "Unexpected closing parenthesis");
            }
            let id = self.add(NodeKind::Error, range);
            self.push_child(None, id);
            return;
        };
        if let NodeKind::List { closed, .. } = &mut self.nodes[list.0].kind {
            *closed = true;
        }
        self.nodes[list.0].range.end = range.end;
        self.check_dot(list, range);
        self.attach_value(list);
    }

    /// The parser's rule for a dotted list: one form after the dot, then the
    /// `)`.
    fn check_dot(&mut self, list: NodeId, close: Range<usize>) {
        let forms: Vec<NodeId> = match &self.nodes[list.0].kind {
            NodeKind::List { children, .. } => children
                .iter()
                .copied()
                .filter(|child| !matches!(self.nodes[child.0].kind, NodeKind::Comment))
                .collect(),
            _ => return,
        };
        let Some(dot) = forms
            .iter()
            .position(|form| matches!(self.nodes[form.0].kind, NodeKind::Dot))
        else {
            return;
        };
        match &forms[dot + 1..] {
            [] => self.error(close, "Unexpected closing parenthesis"),
            [_] => {}
            [first, ..] => {
                let range = self.nodes[first.0].range.clone();
                self.error(range, "Expected only one item in list after dot.");
            }
        }
    }

    /// Reads a form nested deeper than the limit, and the rest of the list it
    /// is in, as one error, as the parser refuses it. Reading goes on at the
    /// `)` that closes that list.
    fn too_deep(&mut self, first: &Token, range: Range<usize>) {
        let start = range.start;
        let mut end = range.end;
        let mut depth = usize::from(matches!(first, Token::OpenParen { .. }));
        let mut closing = None;
        while let Some(token) = self.tokens.next() {
            let next = self.tokens.token_range();
            match token {
                Token::OpenParen { .. } => depth += 1,
                Token::CloseParen { .. } if depth == 0 => {
                    closing = Some(next);
                    break;
                }
                Token::CloseParen { .. } => depth -= 1,
                _ => {}
            }
            end = next.end;
        }
        let message = format!("Lisp nesting exceeds max-nesting-depth ({})", self.limit);
        self.error(start..end, message);
        let id = self.add(NodeKind::Error, start..end);
        self.attach_value(id);
        if let Some(closing) = closing {
            self.close(closing);
        }
    }
}

#[cfg(test)]
impl SyntaxTree<'_> {
    /// The tree written out for tests: lists rebuilt with single spaces, an
    /// unclosed list without its `)`, `∅` for a missing prefixed form, `#c` for
    /// a comment and `#err` for an error node.
    fn sexp(&self) -> String {
        let roots: Vec<String> = self.roots.iter().map(|&id| self.sexp_of(id)).collect();
        roots.join(" ")
    }

    fn sexp_of(&self, id: NodeId) -> String {
        match self.node(id).kind() {
            NodeKind::List { children, closed } => {
                let inner: Vec<String> = children.iter().map(|&c| self.sexp_of(c)).collect();
                let inner = inner.join(" ");
                if *closed {
                    format!("({inner})")
                } else {
                    format!("({inner}")
                }
            }
            NodeKind::Prefix {
                prefix,
                children,
                child,
            } => {
                let inner = if child.is_none() {
                    "∅".to_string()
                } else {
                    let parts: Vec<String> = children.iter().map(|&c| self.sexp_of(c)).collect();
                    parts.join(" ")
                };
                format!("{}{inner}", prefix.text())
            }
            NodeKind::Comment => "#c".to_string(),
            NodeKind::Error => "#err".to_string(),
            _ => self.text(id).to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{TulispContext, TulispObject, TulispValue};

    /// The position a parser span gives byte OFFSET: a 1-based line, and a
    /// 1-based column counted in characters.
    fn line_col(source: &str, offset: usize) -> (usize, usize) {
        let before = &source[..offset];
        let line = before.matches('\n').count() + 1;
        let line_start = before.rfind('\n').map_or(0, |at| at + 1);
        (line, before[line_start..].chars().count() + 1)
    }

    /// Asserts that tree node ID and the parser's OBJ are the same form.
    /// Positions are compared for the objects the parser makes fresh: lists,
    /// prefixed forms, strings and floats. A symbol or an integer is shared
    /// between its uses, so its span is the last use's.
    fn assert_same_shape(tree: &SyntaxTree, id: NodeId, obj: &TulispObject) {
        let node = tree.node(id);
        let fresh = matches!(
            node.kind(),
            NodeKind::Prefix { .. } | NodeKind::Atom(AtomKind::String | AtomKind::Float)
        ) || (matches!(node.kind(), NodeKind::List { .. }) && !obj.null());
        if fresh && let Some(span) = obj.span() {
            let at = line_col(tree.source(), node.range().start);
            assert_eq!(at, span.start, "{} starts elsewhere", tree.text(id));
        }
        match node.kind() {
            NodeKind::List { children, .. } => {
                let mut forms = children
                    .iter()
                    .copied()
                    .filter(|c| !matches!(tree.node(*c).kind(), NodeKind::Comment));
                let mut rest = obj.clone();
                while let Some(form) = forms.next() {
                    if matches!(tree.node(form).kind(), NodeKind::Dot) {
                        let tail = forms.next().expect("a form after the dot");
                        assert_same_shape(tree, tail, &rest);
                        return;
                    }
                    assert!(rest.consp(), "{obj} is shorter than {}", tree.text(id));
                    assert_same_shape(tree, form, &rest.car().unwrap());
                    rest = rest.cdr().unwrap();
                }
                assert!(rest.null(), "{obj} is longer than {}", tree.text(id));
            }
            NodeKind::Prefix { prefix, child, .. } => {
                let child = child.expect("a prefixed form");
                let inner = match (prefix, &obj.inner_ref().0) {
                    (Prefix::Quote, TulispValue::Quote { value })
                    | (Prefix::Backquote, TulispValue::Backquote { value })
                    | (Prefix::Comma, TulispValue::Unquote { value })
                    | (Prefix::Splice, TulispValue::Splice { value }) => value.clone(),
                    (Prefix::Function, _) => obj.cadr().unwrap(),
                    _ => panic!("{obj} is not {}", tree.text(id)),
                };
                assert_same_shape(tree, child, &inner);
            }
            NodeKind::Atom(_) => assert!(!obj.consp(), "{obj} is not {}", tree.text(id)),
            other => panic!("{other:?} in valid input"),
        }
    }

    #[test]
    fn the_tree_has_the_shape_the_parser_reads() {
        let cases = [
            "(defun f (a &optional b) \"doc\" (+ a b))",
            "'(a . b) `(x ,y ,@z) #'car",
            "(a ; comment\n \"é\" 1.5 ?\\n #x1F)",
            "((()))",
            "",
        ];
        for source in cases {
            let mut ctx = TulispContext::new();
            let forms = ctx.parse_file_text("<parity>", source).expect(source);
            let tree = read(source);
            assert!(tree.errors().is_empty(), "{source}: {:?}", tree.errors());
            let roots: Vec<NodeId> = tree
                .roots()
                .iter()
                .copied()
                .filter(|id| !matches!(tree.node(*id).kind(), NodeKind::Comment))
                .collect();
            let forms: Vec<TulispObject> = forms.base_iter().collect();
            assert_eq!(roots.len(), forms.len(), "{source}");
            for (&id, form) in roots.iter().zip(&forms) {
                assert_same_shape(&tree, id, form);
            }
        }
    }

    // Where the parser stops with an error, the tree has an error at the same
    // place. The parser stops at its first error; the tree may find more.
    #[test]
    fn the_tree_has_an_error_where_the_parser_fails() {
        let cases = [
            "(a",
            "a)",
            "(a . )",
            "(a . b c)",
            ". a",
            "(a ')",
            "'",
            "(a [b])",
            r#"("a\M-b" c)"#,
            "(#z)",
            "\"abc",
        ];
        for source in cases {
            let mut ctx = TulispContext::new();
            let err = ctx
                .parse_file_text("<parity>", source)
                .expect_err(source)
                .to_string();
            let tree = read(source);
            let Some(at) = err
                .split("<parity>:")
                .nth(1)
                .and_then(|rest| rest.split('-').next())
            else {
                // The parser blames a shared symbol, which has no span of its
                // own, so it reports no position: compare the message.
                let message = err.rsplit("ParsingError: ").next().unwrap_or(&err);
                assert!(
                    tree.errors().iter().any(|e| e.message == message),
                    "{source}: the parser says {err}, the tree {:?}",
                    tree.errors()
                );
                continue;
            };
            let found: Vec<String> = tree
                .errors()
                .iter()
                .map(|e| {
                    let (line, column) = line_col(source, e.range.start);
                    format!("{line}.{column}")
                })
                .collect();
            assert!(
                found.iter().any(|position| position == at),
                "{source}: the parser fails at {at}, the tree at {found:?}"
            );
        }
    }

    // The parser's nesting error has no position; the tree has the same error.
    #[test]
    fn the_tree_refuses_the_nesting_the_parser_refuses() {
        let source = format!("{}a{}", "(".repeat(100), ")".repeat(100));
        let mut ctx = TulispContext::new();
        let err = ctx
            .parse_file_text("<parity>", &source)
            .expect_err("too deep")
            .to_string();
        assert!(err.contains("Lisp nesting exceeds"), "{err}");
        assert!(
            read(&source)
                .errors()
                .iter()
                .any(|e| e.message.starts_with("Lisp nesting exceeds"))
        );
    }

    fn sexp(source: &str) -> String {
        read(source).sexp()
    }

    /// The text each error covers, and its message.
    fn errors(source: &str) -> Vec<(String, String)> {
        read(source)
            .errors()
            .iter()
            .map(|e| (source[e.range.clone()].to_string(), e.message.clone()))
            .collect()
    }

    fn pairs(list: &[(&str, &str)]) -> Vec<(String, String)> {
        list.iter()
            .map(|(text, message)| (text.to_string(), message.to_string()))
            .collect()
    }

    #[test]
    fn reads_lists_atoms_and_prefixes() {
        let source = "(a 1 2.5 \"s\" ?c) 'x `(,y ,@z) #'f (a . b)";
        assert_eq!(sexp(source), source);
        assert!(read(source).errors().is_empty());
    }

    #[test]
    fn atoms_have_their_kinds() {
        let tree = read("a 1 2.5 \"s\" ?c #x1F");
        let kinds: Vec<NodeKind> = tree
            .roots()
            .iter()
            .map(|&id| tree.node(id).kind().clone())
            .collect();
        assert_eq!(
            kinds,
            [
                NodeKind::Atom(AtomKind::Symbol),
                NodeKind::Atom(AtomKind::Integer),
                NodeKind::Atom(AtomKind::Float),
                NodeKind::Atom(AtomKind::String),
                NodeKind::Atom(AtomKind::Character),
                NodeKind::Atom(AtomKind::Integer),
            ]
        );
    }

    #[test]
    fn comments_are_nodes() {
        assert_eq!(sexp("(a ; one\n b) ; two"), "(a #c b) #c");
    }

    #[test]
    fn empty_input_reads_as_an_empty_tree() {
        for source in ["", " \r\n\t"] {
            let tree = read(source);
            assert!(tree.roots().is_empty(), "{source:?}");
            assert!(tree.errors().is_empty(), "{source:?}");
        }
    }

    #[test]
    fn ranges_cover_whole_characters() {
        let source = "(é \"ü\")";
        let tree = read(source);
        let list = tree.roots()[0];
        let texts: Vec<&str> = tree
            .children(list)
            .iter()
            .map(|&id| tree.text(id))
            .collect();
        assert_eq!(texts, ["é", "\"ü\""]);
        assert_eq!(tree.node(list).range(), 0..source.len());
    }

    // Every character that is not white space belongs to a node: a token of its
    // own, or the parenthesis or prefix of a list or prefixed form.
    #[test]
    fn every_character_belongs_to_a_node() {
        let source = "(é \"ü\" ; ñ\r\n 'x `(,y ,@z) #'f (a . b) [ ?\\n) ) #x1F";
        let tree = read(source);
        let mut covered = vec![false; source.len()];
        let mut cover = |range: Range<usize>| {
            for at in range {
                covered[at] = true;
            }
        };
        for id in tree.ids() {
            let node = tree.node(id);
            let range = node.range();
            match node.kind() {
                NodeKind::List { closed, .. } => {
                    cover(range.start..range.start + 1);
                    if *closed {
                        cover(range.end - 1..range.end);
                    }
                }
                NodeKind::Prefix { prefix, .. } => {
                    cover(range.start..range.start + prefix.text().len())
                }
                _ => cover(range),
            }
        }
        for (at, ch) in source.char_indices() {
            assert!(
                covered[at] || ch.is_whitespace(),
                "{ch:?} at {at} is in no node"
            );
        }
    }

    #[test]
    fn an_unclosed_list_ends_at_the_end_of_the_input() {
        let source = "(a (b c";
        assert_eq!(sexp(source), "(a (b c");
        assert_eq!(
            errors(source),
            pairs(&[("(", "Unclosed list"), ("(", "Unclosed list")])
        );
        let tree = read(source);
        let outer = tree.roots()[0];
        let inner = tree.children(outer)[1];
        assert_eq!(tree.node(outer).range().end, source.len());
        assert_eq!(tree.node(inner).range().end, source.len());
    }

    #[test]
    fn a_stray_close_paren_is_an_error_and_reading_goes_on() {
        assert_eq!(sexp("a) b"), "a #err b");
        assert_eq!(
            errors("a) b"),
            pairs(&[(")", "Unexpected closing parenthesis")])
        );
    }

    #[test]
    fn misplaced_dots_are_errors() {
        assert_eq!(sexp(". a"), "#err a");
        assert_eq!(errors(". a"), pairs(&[(".", "Unexpected dot")]));
        assert_eq!(sexp("(a . )"), "(a .)");
        assert_eq!(
            errors("(a . )"),
            pairs(&[(")", "Unexpected closing parenthesis")])
        );
        assert_eq!(
            errors("(a . b c)"),
            pairs(&[("b", "Expected only one item in list after dot.")])
        );
    }

    #[test]
    fn a_prefix_with_nothing_after_it() {
        assert_eq!(sexp("(a ')"), "(a '∅)");
        assert_eq!(
            errors("(a ')"),
            pairs(&[(")", "Unexpected closing parenthesis")])
        );
        assert_eq!(sexp("'"), "'∅");
        assert_eq!(errors("'"), pairs(&[("'", "Unexpected EOF")]));
    }

    #[test]
    fn tokenizer_errors_become_error_nodes() {
        assert_eq!(sexp("(a [b] #z c)"), "(a #err b #err #err z c)");
        assert_eq!(
            errors("(a [b] #z c)"),
            pairs(&[
                ("[", "Vector syntax is not supported"),
                ("]", "Vector syntax is not supported"),
                ("#", "Unknown token #.  Did you mean #' ?"),
            ])
        );
    }

    // The error starts at the bad escape; the node is the whole string.
    #[test]
    fn a_string_with_a_bad_escape_is_one_error() {
        let source = r#"("a\M-b" c)"#;
        assert_eq!(sexp(source), "(#err c)");
        assert_eq!(
            errors(source),
            pairs(&[("M-b\"", r"Modifier keys are not supported: \M-")])
        );
    }

    #[test]
    fn an_incomplete_string_runs_to_the_end() {
        let source = "(a \"bc";
        assert_eq!(sexp(source), "(a #err");
        assert_eq!(
            errors(source),
            pairs(&[
                ("(", "Unclosed list"),
                ("\"bc", "Incomplete string literal")
            ])
        );
    }

    #[test]
    fn nesting_past_the_limit_is_one_error() {
        let source = "(((a b) c) d)";
        let tree = read_with_limit(source, 2);
        assert_eq!(tree.sexp(), "((#err) d)");
        let found: Vec<(&str, &str)> = tree
            .errors()
            .iter()
            .map(|e| (&source[e.range.clone()], e.message.as_str()))
            .collect();
        assert_eq!(
            found,
            [("(a b) c", "Lisp nesting exceeds max-nesting-depth (2)")]
        );
    }

    #[test]
    fn a_second_dot_is_an_error() {
        let found = read("(a . .)").errors().to_vec();
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].message, "Unexpected dot");
        assert_eq!(found[0].range, 5..6);
        let found = read("(. .)").errors().to_vec();
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].message, "Unexpected dot");
        assert_eq!(found[0].range.start, 3);
        assert_eq!(errors("(a . .)"), pairs(&[(".", "Unexpected dot")]));
    }

    #[test]
    fn a_comment_after_a_prefix_stays_inside_it() {
        assert_eq!(sexp("(' ;c\n x)"), "('#c x)");
        let tree = read("' ;c\n x");
        assert_eq!(tree.roots().len(), 1);
        let prefix = tree.roots()[0];
        let children = tree.children(prefix).to_vec();
        assert_eq!(children.len(), 2);
        assert_eq!(*tree.node(children[0]).kind(), NodeKind::Comment);
        assert_eq!(tree.text(children[1]), "x");
        match tree.node(prefix).kind() {
            NodeKind::Prefix { child, .. } => assert_eq!(*child, Some(children[1])),
            other => panic!("{other:?}"),
        }
        let tree = read("(' ;c\n x)");
        assert_eq!(tree.children(tree.roots()[0]).len(), 1);
    }

    #[test]
    fn a_stray_close_after_a_top_level_prefix_is_one_error() {
        assert_eq!(
            errors("')"),
            pairs(&[(")", "Unexpected closing parenthesis")])
        );
    }

    #[test]
    fn deep_balanced_input_builds_and_drops_without_a_limit() {
        let source = "(".repeat(200_000) + &")".repeat(200_000);
        let tree = read_with_limit(&source, usize::MAX);
        assert!(tree.errors().is_empty());
        drop(tree);
    }

    #[test]
    fn deep_input_builds_and_drops() {
        let source = "(".repeat(200_000);
        let tree = read(&source);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.starts_with("Lisp nesting exceeds"))
        );
        drop(tree);
    }

    fn texts<'a>(tree: &SyntaxTree<'a>, ids: &[NodeId]) -> Vec<&'a str> {
        ids.iter().map(|&id| tree.text(id)).collect()
    }

    #[test]
    fn the_path_goes_down_to_the_innermost_node() {
        // Offsets: ( 0, a 1, ( 3, b 4, c 5, d 7, ) 8, ' 10, e 11, ) 12.
        let tree = read("(a (bc d) 'e)");
        assert_eq!(
            texts(&tree, &tree.path_at(5)),
            ["(a (bc d) 'e)", "(bc d)", "bc"]
        );
        // The end of a symbol is in it.
        assert_eq!(
            texts(&tree, &tree.path_at(6)),
            ["(a (bc d) 'e)", "(bc d)", "bc"]
        );
        // Before a `(` is outside the list.
        assert_eq!(texts(&tree, &tree.path_at(3)), ["(a (bc d) 'e)"]);
        assert_eq!(
            texts(&tree, &tree.path_at(12)),
            ["(a (bc d) 'e)", "'e", "e"]
        );
        // After the last `)` is outside every list.
        assert!(tree.path_at(13).is_empty());
    }

    #[test]
    fn a_string_holds_only_the_offsets_between_its_quotes() {
        // Offsets: ( 0, f 1, " 3, a 4, b 5, " 6, ) 7.
        let tree = read("(f \"ab\")");
        assert_eq!(texts(&tree, &tree.path_at(3)), ["(f \"ab\")"]);
        assert_eq!(texts(&tree, &tree.path_at(5)), ["(f \"ab\")", "\"ab\""]);
        assert_eq!(texts(&tree, &tree.path_at(7)), ["(f \"ab\")"]);
    }

    #[test]
    fn an_unclosed_list_holds_the_end_of_the_input() {
        let tree = read("(f (g ");
        assert_eq!(texts(&tree, &tree.path_at(6)), ["(f (g ", "(g "]);
    }

    #[test]
    fn call_at_counts_the_forms_before_the_offset() {
        // Offsets: ( 0, f 1, a 3, ; 5, c 7, \n 8, ( 10, g 11, ) 12, ) 14.
        let tree = read("(f a ; c\n (g) )");
        let root = tree.roots()[0];
        let at = |offset| tree.call_at(offset);
        assert_eq!(
            at(2),
            Some(CallSite {
                list: root,
                position: 0
            })
        );
        assert_eq!(
            at(3),
            Some(CallSite {
                list: root,
                position: 1
            })
        );
        assert_eq!(
            at(13),
            Some(CallSite {
                list: root,
                position: 2
            })
        );
        assert_eq!(
            at(14),
            Some(CallSite {
                list: root,
                position: 3
            })
        );
        let inner = tree.forms(root)[2];
        assert_eq!(
            at(11),
            Some(CallSite {
                list: inner,
                position: 0
            })
        );
        // In a comment.
        assert_eq!(at(6), None);
    }

    #[test]
    fn call_at_is_none_in_a_string_and_at_the_top_level() {
        assert_eq!(read("(f \"ab\")").call_at(5), None);
        assert_eq!(read("(f \"ab").call_at(5), None);
        assert_eq!(read("a b").call_at(1), None);
    }

    #[test]
    fn neighbouring_nodes_share_no_offset_inside_a_list() {
        // Offsets: ( 0, a 1, ) 2, ( 3, b 4, ) 5.
        let tree = read("(a)(b)");
        assert!(tree.path_at(3).is_empty());
        assert_eq!(tree.call_at(3), None);
    }

    #[test]
    fn the_end_of_a_symbol_is_in_it_before_a_list_or_a_string() {
        // Offsets: a 0, ( 1, b 2, ) 3.
        let tree = read("a(b)");
        assert_eq!(texts(&tree, &tree.path_at(1)), ["a"]);
        // Offsets: x 0, " 1, s 2, " 3.
        let tree = read("x\"s\"");
        assert_eq!(texts(&tree, &tree.path_at(1)), ["x"]);
    }

    #[test]
    fn a_prefix_starts_after_its_first_character() {
        // Offsets: ( 0, f 1, ' 3, a 4, ) 5.
        let tree = read("(f 'a)");
        assert_eq!(texts(&tree, &tree.path_at(3)), ["(f 'a)"]);
        assert_eq!(texts(&tree, &tree.path_at(4)), ["(f 'a)", "'a", "a"]);
    }

    #[test]
    fn a_comment_holds_its_end_but_not_the_next_line() {
        // Offsets: ( 0, f 1, ; 3, c 5, \n 6, a 8, ) 9.
        let tree = read("(f ; c\n a)");
        let root = tree.roots()[0];
        assert_eq!(tree.call_at(6), None);
        assert_eq!(
            tree.call_at(8),
            Some(CallSite {
                list: root,
                position: 1
            })
        );
    }

    #[test]
    fn a_comment_inside_a_prefix_can_be_entered() {
        // Offsets: ( 0, f 1, ' 3, ; 4, c 6, \n 7, a 9, ) 10.
        let tree = read("(f '; c\n a)");
        let path = tree.path_at(5);
        assert_eq!(texts(&tree, &path), ["(f '; c\n a)", "'; c\n a", "; c"]);
        let comment = tree.node(path[2]);
        assert_eq!(comment.kind(), &NodeKind::Comment);
        assert_eq!(comment.parent(), Some(path[1]));
        assert_eq!(tree.call_at(5), None);
        let path = tree.path_at(9);
        assert_eq!(texts(&tree, &path), ["(f '; c\n a)", "'; c\n a", "a"]);
    }

    #[test]
    fn call_at_before_an_unclosed_string_is_at_its_form() {
        // Offsets: ( 0, f 1, " 3, a 4, b 5.
        let tree = read("(f \"ab");
        let root = tree.roots()[0];
        assert_eq!(
            tree.call_at(3),
            Some(CallSite {
                list: root,
                position: 1
            })
        );
        assert_eq!(tree.call_at(4), None);
    }
}
