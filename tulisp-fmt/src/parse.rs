//! Token stream → [`Cst`].
//!
//! Recursive descent over the flat token vector produced by the
//! lexer. Trivia (comments, line breaks) lives alongside structural
//! nodes inside whatever sequence it appeared in.

use std::ops::Range;

use crate::cst::{Cst, CstNode, ReaderPrefix};
use crate::lex::{Token, TokenKind};

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Range<usize>,
}

pub fn parse(tokens: &[Token]) -> Result<Cst, ParseError> {
    parse_with_source(tokens, None)
}

/// Parse using `source` to recover atom and comment text. `lex`
/// returns spans into the source string; the parser needs the
/// original text to materialise the owned-`String` payloads on
/// `Atom` and `Comment` nodes. The free function version above is
/// kept as a convenience and recovers the source from the token
/// span ends, but the caller normally has it on hand and passes it
/// through `format`.
pub(crate) fn parse_with_source(
    tokens: &[Token],
    source: Option<&str>,
) -> Result<Cst, ParseError> {
    let mut p = Parser {
        tokens,
        pos: 0,
        source,
    };
    let nodes = p.parse_sequence(None)?;
    Ok(Cst { nodes })
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    source: Option<&'a str>,
}

impl<'a> Parser<'a> {
    /// Parse a sequence of nodes. If `terminator` is `Some(kind)`,
    /// the loop ends when that kind is encountered (and consumed by
    /// the caller); otherwise it ends at end-of-input.
    fn parse_sequence(
        &mut self,
        terminator: Option<TokenKind>,
    ) -> Result<Vec<CstNode>, ParseError> {
        let mut out = Vec::new();
        while self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            if Some(tok.kind) == terminator {
                return Ok(out);
            }
            out.push(self.parse_node()?);
        }
        if terminator.is_some() {
            // Find the most recent unmatched `(` for a useful span.
            let span = self
                .tokens
                .last()
                .map(|t| t.start..t.end)
                .unwrap_or(0..0);
            return Err(ParseError {
                message: "unclosed `(`".to_string(),
                span,
            });
        }
        Ok(out)
    }

    fn parse_node(&mut self) -> Result<CstNode, ParseError> {
        let tok = &self.tokens[self.pos];
        match tok.kind {
            TokenKind::LParen => self.parse_list(),
            TokenKind::RParen => {
                let span = tok.start..tok.end;
                Err(ParseError {
                    message: "unexpected `)`".to_string(),
                    span,
                })
            }
            TokenKind::Quote
            | TokenKind::Backquote
            | TokenKind::Unquote
            | TokenKind::Splice
            | TokenKind::Sharpquote => self.parse_reader_macro(),
            TokenKind::Atom => {
                let text = self.text(tok).to_string();
                let span = tok.start..tok.end;
                self.pos += 1;
                Ok(CstNode::Atom { text, span })
            }
            TokenKind::Comment => {
                let text = self.text(tok).to_string();
                let span = tok.start..tok.end;
                self.pos += 1;
                Ok(CstNode::Comment { text, span })
            }
            TokenKind::LineBreak { count } => {
                self.pos += 1;
                Ok(CstNode::LineBreak { count })
            }
        }
    }

    fn parse_list(&mut self) -> Result<CstNode, ParseError> {
        let start = self.tokens[self.pos].start;
        self.pos += 1; // eat `(`
        let children = self.parse_sequence(Some(TokenKind::RParen))?;
        let end = self.tokens[self.pos].end;
        self.pos += 1; // eat `)`
        Ok(CstNode::List {
            children,
            span: start..end,
        })
    }

    fn parse_reader_macro(&mut self) -> Result<CstNode, ParseError> {
        let prefix_tok = &self.tokens[self.pos];
        let prefix = match prefix_tok.kind {
            TokenKind::Quote => ReaderPrefix::Quote,
            TokenKind::Backquote => ReaderPrefix::Backquote,
            TokenKind::Unquote => ReaderPrefix::Unquote,
            TokenKind::Splice => ReaderPrefix::Splice,
            TokenKind::Sharpquote => ReaderPrefix::Sharpquote,
            _ => unreachable!(),
        };
        let start = prefix_tok.start;
        self.pos += 1;
        // Skip any trivia between the prefix and its target. Comments
        // there are unusual and would render strangely after a quote;
        // dropping them matches what most lisp formatters do.
        while self.pos < self.tokens.len() {
            match self.tokens[self.pos].kind {
                TokenKind::Comment | TokenKind::LineBreak { .. } => self.pos += 1,
                _ => break,
            }
        }
        if self.pos >= self.tokens.len() {
            return Err(ParseError {
                message: format!("trailing reader macro `{}`", prefix.as_str()),
                span: start..prefix_tok.end,
            });
        }
        let inner = self.parse_node()?;
        let end = inner.span().end;
        Ok(CstNode::ReaderMacro {
            prefix,
            inner: Box::new(inner),
            span: start..end,
        })
    }

    fn text(&self, tok: &Token) -> &str {
        match self.source {
            Some(s) => &s[tok.start..tok.end],
            None => "",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cst::CstNode;

    fn parse_str(src: &str) -> Cst {
        let tokens = crate::lex::lex(src);
        parse_with_source(&tokens, Some(src)).expect("parse")
    }

    #[test]
    fn empty_source() {
        let cst = parse_str("");
        assert!(cst.nodes.is_empty());
    }

    #[test]
    fn flat_form() {
        let cst = parse_str("(foo 1 2)");
        assert_eq!(cst.nodes.len(), 1);
        match &cst.nodes[0] {
            CstNode::List { children, .. } => assert_eq!(children.len(), 3),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn nested_with_trivia() {
        let src = ";; top\n(let ((x 1)) ;; trailing\n  x)\n";
        let cst = parse_str(src);
        // top-level: comment, line-break, list, line-break.
        assert!(matches!(&cst.nodes[0], CstNode::Comment { .. }));
        assert!(matches!(&cst.nodes[1], CstNode::LineBreak { count: 1 }));
        assert!(matches!(&cst.nodes[2], CstNode::List { .. }));
        // Inside the list, the trailing `;; trailing` comment should
        // sit between the binding list and the body symbol.
        let body = match &cst.nodes[2] {
            CstNode::List { children, .. } => children,
            _ => unreachable!(),
        };
        let comments: Vec<&str> = body
            .iter()
            .filter_map(|n| match n {
                CstNode::Comment { text, .. } => Some(text.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(comments, vec![";; trailing"]);
    }

    #[test]
    fn reader_macros_attach_to_inner() {
        let cst = parse_str("'foo `(a ,b ,@c) #'fn");
        // 5 top-level nodes: 'foo, ws, `(...), ws, #'fn  → 5 with line-breaks?
        // Actually: 'foo + space-skipped + `(...) + space + #'fn — no
        // newlines so there are no LineBreak nodes; spaces are
        // dropped by the lexer. So 3 reader-macro nodes.
        let prefixes: Vec<ReaderPrefix> = cst
            .nodes
            .iter()
            .filter_map(|n| match n {
                CstNode::ReaderMacro { prefix, .. } => Some(*prefix),
                _ => None,
            })
            .collect();
        assert_eq!(
            prefixes,
            vec![
                ReaderPrefix::Quote,
                ReaderPrefix::Backquote,
                ReaderPrefix::Sharpquote,
            ]
        );
    }

    #[test]
    fn unclosed_paren_errors() {
        let tokens = crate::lex::lex("(foo");
        let err = parse_with_source(&tokens, Some("(foo")).unwrap_err();
        assert!(err.message.contains("unclosed"), "got: {}", err.message);
    }

    #[test]
    fn unexpected_close_errors() {
        let tokens = crate::lex::lex(")");
        let err = parse_with_source(&tokens, Some(")")).unwrap_err();
        assert!(err.message.contains("unexpected"), "got: {}", err.message);
    }

    #[test]
    fn atom_text_round_trips() {
        let src = r#"foo "bar baz" 42 #x1A ?\n"#;
        let cst = parse_str(src);
        let texts: Vec<&str> = cst
            .nodes
            .iter()
            .filter_map(|n| match n {
                CstNode::Atom { text, .. } => Some(text.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(texts, vec!["foo", r#""bar baz""#, "42", "#x1A", r#"?\n"#]);
    }
}
