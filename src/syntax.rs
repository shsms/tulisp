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
            _ => &[],
        }
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

/// The byte offset of a parser position: a 1-based line, and a 1-based
/// column counted in characters.
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
}
