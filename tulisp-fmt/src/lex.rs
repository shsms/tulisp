//! Token-stream lexer with trivia capture.
//!
//! Produces a flat `Vec<Token>` from source text. Comments and
//! line-break runs are emitted as their own tokens so the formatter
//! can place them back; structural whitespace (spaces / tabs) is
//! discarded since the renderer re-emits its own indentation.

/// A single token. `start..end` is the byte range in the source.
#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

impl Token {
    /// The token's exact source text.
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    LParen,    // (
    RParen,    // )
    Quote,     // '
    Backquote, // `
    Unquote,   // ,    — not followed by `@`
    Splice,    // ,@
    Sharpquote, // #'
    /// Symbol, number, string, character literal, etc. The kind is
    /// recovered by the parser (and ultimately the renderer) from the
    /// token text.
    Atom,
    /// Line comment, including the leading `;`s but not the trailing
    /// newline. The renderer counts the leading semicolons to apply
    /// indentation conventions (`;` / `;;` / `;;;`).
    Comment,
    /// Run of newlines. `count` is the number of `\n` characters; `1`
    /// means a normal line break, `>= 2` means at least one blank
    /// line between the surrounding tokens.
    LineBreak { count: u32 },
}

/// Tokenize `source`. Spaces and tabs are skipped silently;
/// everything else lands in the returned vector.
pub fn lex(source: &str) -> Vec<Token> {
    let bytes = source.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        match c {
            b' ' | b'\t' | b'\r' => {
                i += 1;
            }
            b'\n' => {
                let start = i;
                let mut count: u32 = 0;
                while i < bytes.len() {
                    match bytes[i] {
                        b'\n' => {
                            count += 1;
                            i += 1;
                        }
                        b' ' | b'\t' | b'\r' => {
                            i += 1;
                        }
                        _ => break,
                    }
                }
                out.push(Token {
                    kind: TokenKind::LineBreak { count },
                    start,
                    end: i,
                });
            }
            b'(' => {
                out.push(Token { kind: TokenKind::LParen, start: i, end: i + 1 });
                i += 1;
            }
            b')' => {
                out.push(Token { kind: TokenKind::RParen, start: i, end: i + 1 });
                i += 1;
            }
            b'\'' => {
                out.push(Token { kind: TokenKind::Quote, start: i, end: i + 1 });
                i += 1;
            }
            b'`' => {
                out.push(Token { kind: TokenKind::Backquote, start: i, end: i + 1 });
                i += 1;
            }
            b',' => {
                let (kind, end) = if bytes.get(i + 1) == Some(&b'@') {
                    (TokenKind::Splice, i + 2)
                } else {
                    (TokenKind::Unquote, i + 1)
                };
                out.push(Token { kind, start: i, end });
                i = end;
            }
            b'#' if bytes.get(i + 1) == Some(&b'\'') => {
                out.push(Token { kind: TokenKind::Sharpquote, start: i, end: i + 2 });
                i += 2;
            }
            b';' => {
                let start = i;
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
                out.push(Token { kind: TokenKind::Comment, start, end: i });
            }
            b'"' => {
                let start = i;
                i += 1;
                while i < bytes.len() {
                    match bytes[i] {
                        b'\\' if i + 1 < bytes.len() => {
                            // Skip the backslash and its escapee, including
                            // a backslash-newline that some sources use.
                            i += 2;
                        }
                        b'"' => {
                            i += 1;
                            break;
                        }
                        _ => i += 1,
                    }
                }
                out.push(Token { kind: TokenKind::Atom, start, end: i });
            }
            _ => {
                // Bare atom: read until we hit a structural delimiter.
                // `?x` character literals starting here pass through as
                // an atom whose text begins with `?`.
                let start = i;
                while i < bytes.len() && !is_atom_terminator(bytes[i]) {
                    i += 1;
                }
                if i == start {
                    // Defensive: an unrecognised single byte. Emit it
                    // as a one-char atom rather than infinite-looping.
                    i += 1;
                }
                out.push(Token { kind: TokenKind::Atom, start, end: i });
            }
        }
    }
    out
}

fn is_atom_terminator(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b'\'' | b'`' | b',' | b'"' | b';'
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(source: &str) -> Vec<TokenKind> {
        lex(source).into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn empty() {
        assert!(lex("").is_empty());
    }

    #[test]
    fn simple_call() {
        assert_eq!(
            kinds("(foo 1 2)"),
            vec![
                TokenKind::LParen,
                TokenKind::Atom,
                TokenKind::Atom,
                TokenKind::Atom,
                TokenKind::RParen,
            ]
        );
    }

    #[test]
    fn line_breaks_and_blank_lines() {
        let toks = lex("(a)\n(b)\n\n(c)");
        let breaks: Vec<_> = toks
            .iter()
            .filter_map(|t| match t.kind {
                TokenKind::LineBreak { count } => Some(count),
                _ => None,
            })
            .collect();
        assert_eq!(breaks, vec![1, 2]);
    }

    #[test]
    fn comments_and_reader_macros() {
        let src = ";; top\n'(a ,b ,@c #'d)";
        let kinds = kinds(src);
        assert_eq!(
            kinds,
            vec![
                TokenKind::Comment,
                TokenKind::LineBreak { count: 1 },
                TokenKind::Quote,
                TokenKind::LParen,
                TokenKind::Atom,
                TokenKind::Unquote,
                TokenKind::Atom,
                TokenKind::Splice,
                TokenKind::Atom,
                TokenKind::Sharpquote,
                TokenKind::Atom,
                TokenKind::RParen,
            ]
        );
    }

    #[test]
    fn string_with_escapes() {
        let src = r#"(message "hi\nthere")"#;
        let toks = lex(src);
        // Three structural tokens + the string atom.
        let atoms: Vec<_> = toks
            .iter()
            .filter(|t| t.kind == TokenKind::Atom)
            .map(|t| t.text(src))
            .collect();
        assert_eq!(atoms, vec!["message", r#""hi\nthere""#]);
    }

    #[test]
    fn span_round_trip() {
        // Every token's span maps back to the source verbatim.
        let src = "(let ((x 1)) ;; bind\n  (+ x 2))\n";
        for t in lex(src) {
            assert!(t.end <= src.len());
            assert_eq!(&src[t.start..t.end], t.text(src));
        }
    }
}
