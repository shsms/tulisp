//! Lexer stub. Implementation lands in the next commit.

/// Source token; a richer struct will replace this once the lexer is
/// wired up.
pub struct Token;

/// Tokenize `source` into a flat token stream. Comments, whitespace,
/// and blank lines are preserved as trivia tokens so the formatter can
/// place them back in the rendered output.
pub fn lex(_source: &str) -> Vec<Token> {
    Vec::new()
}
