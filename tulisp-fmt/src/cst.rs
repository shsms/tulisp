//! Concrete syntax tree.
//!
//! Built from the lexer's token stream by [`crate::parse::parse`].
//! Trivia tokens (comments, line-break runs) live as siblings inside
//! whatever sequence they appeared in — top-level for source-file
//! comments, inside a `List` for comments between elements of a form.
//! That keeps the renderer's job simple: walk children left to right
//! and emit each one according to its kind.

use std::ops::Range;

/// Top-level CST. Exists as a wrapper around `Vec<CstNode>` so the
/// public surface has a single concrete type to talk about.
#[derive(Clone, Debug)]
pub struct Cst {
    pub nodes: Vec<CstNode>,
}

#[derive(Clone, Debug)]
pub enum CstNode {
    /// Atom: symbol, number, string, character literal, or
    /// `#x`/`#o`/`#b`-prefixed integer. The original source spelling is
    /// preserved verbatim — `#x1A` round-trips as `#x1A`, not `26`.
    Atom { text: String, span: Range<usize> },

    /// `( … )`. `children` retains structural elements *and* trivia
    /// (comments, line breaks) in source order.
    List {
        children: Vec<CstNode>,
        span: Range<usize>,
    },

    /// A reader-macro prefix attached to one inner form: `'x`, `` `x ``,
    /// `,x`, `,@x`, `#'x`. Trivia between the prefix and the inner
    /// form is discarded — `'\n  x` re-renders as `'x`.
    ReaderMacro {
        prefix: ReaderPrefix,
        inner: Box<CstNode>,
        span: Range<usize>,
    },

    /// `;`-prefixed line comment. `text` is the comment's source
    /// spelling including the leading semicolons. The renderer counts
    /// the semicolons to pick column-vs-aligned-vs-flush placement.
    Comment { text: String, span: Range<usize> },

    /// One or more `\n`s between sibling nodes. `count == 1` is a
    /// plain line break; `count >= 2` is a blank line. The renderer
    /// preserves blank-line presence (collapsed to a single blank).
    LineBreak { count: u32 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReaderPrefix {
    Quote,
    Backquote,
    Unquote,
    Splice,
    Sharpquote,
}

impl ReaderPrefix {
    pub fn as_str(self) -> &'static str {
        match self {
            ReaderPrefix::Quote => "'",
            ReaderPrefix::Backquote => "`",
            ReaderPrefix::Unquote => ",",
            ReaderPrefix::Splice => ",@",
            ReaderPrefix::Sharpquote => "#'",
        }
    }
}

impl CstNode {
    /// Source byte range covered by this node. For trivia (line
    /// breaks), an empty range at an arbitrary position is acceptable
    /// since the renderer doesn't use the span for re-emission.
    pub fn span(&self) -> Range<usize> {
        match self {
            CstNode::Atom { span, .. }
            | CstNode::List { span, .. }
            | CstNode::ReaderMacro { span, .. }
            | CstNode::Comment { span, .. } => span.clone(),
            CstNode::LineBreak { .. } => 0..0,
        }
    }
}
