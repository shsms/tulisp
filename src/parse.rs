use std::{collections::HashMap, iter::Peekable, ops::Range, str::Chars};

use crate::{Error, Number, Rest, TulispContext, TulispObject, TulispValue, object::Span};

/// The characters that end a number or symbol. Emacs also ends one at any other
/// character below a space and at a no-break space.
const TOKEN_ENDS: &str = "()[]'\";`,# \t\n\r";

pub(crate) struct Tokenizer<'a> {
    file_id: usize,
    chars: Chars<'a>,
    /// A character read ahead by `peek_char` and not yet taken.
    peeked: Option<Option<char>>,
    /// The text being read.
    source: &'a str,
    line: usize,
    pos: usize,
    /// Where the token `next` last returned starts, in bytes.
    token_start: usize,
    /// Whether comments come back as `Token::Comment` instead of being skipped.
    keep_comments: bool,
}

#[derive(PartialEq, Debug)]
enum ParserErrorKind {
    SyntaxError,
}

#[allow(unused)]
#[derive(Debug)]
pub(crate) struct ParserError {
    kind: ParserErrorKind,
    pub(crate) desc: String,
    pub(crate) span: Span,
    /// Where the error is in the source, in bytes: the character the start of
    /// `span` names.
    pub(crate) offset: usize,
}

impl ParserError {
    fn syntax_error(desc: String, span: Span, offset: usize) -> Self {
        ParserError {
            kind: ParserErrorKind::SyntaxError,
            desc,
            span,
            offset,
        }
    }
}

#[derive(Debug)]
pub(crate) enum Token {
    OpenParen {
        span: Span,
    },
    CloseParen {
        span: Span,
    },
    Quote {
        span: Span,
    },
    Backtick {
        span: Span,
    },
    Dot {
        span: Span,
    },
    Comma {
        span: Span,
    },
    Splice {
        span: Span,
    }, // ,@
    SharpQuote {
        span: Span,
    }, // #'
    String {
        span: Span,
        value: String,
    },
    Integer {
        span: Span,
        value: i64,
    },
    Float {
        span: Span,
        value: f64,
    },
    Ident {
        span: Span,
        value: String,
    },
    /// A `;` comment, up to its newline. Only a tokenizer made `with_comments`
    /// returns one.
    Comment,

    ParserError(ParserError),
}

impl Tokenizer<'_> {
    pub(crate) fn new(file_id: usize, program: &str) -> Tokenizer<'_> {
        Tokenizer {
            file_id,
            chars: program.chars(),
            peeked: None,
            source: program,
            line: 1,
            pos: 0,
            token_start: 0,
            keep_comments: false,
        }
    }

    /// The tokenizer, returning comments as `Token::Comment` instead of
    /// skipping them.
    pub(crate) fn with_comments(mut self) -> Self {
        self.keep_comments = true;
        self
    }

    /// The bytes of the source that the token `next` last returned was read
    /// from.
    pub(crate) fn token_range(&self) -> Range<usize> {
        self.token_start..self.offset()
    }

    /// How many bytes have been read.
    fn offset(&self) -> usize {
        let peeked = match self.peeked {
            Some(Some(ch)) => ch.len_utf8(),
            _ => 0,
        };
        self.source.len() - self.chars.as_str().len() - peeked
    }

    /// Where the character last read starts, in bytes; after a newline, where
    /// the new line starts. This is the place `(self.line, self.pos)` names.
    fn last_char_offset(&self) -> usize {
        let end = self.offset();
        match self.source[..end].chars().next_back() {
            Some(ch) if self.pos > 0 => end - ch.len_utf8(),
            _ => end,
        }
    }

    /// Reads past the rest of a string literal, to its closing quote, so that
    /// the text after a bad escape is not read as code.
    fn skip_rest_of_string(&mut self) {
        while let Some(ch) = self.next_char() {
            match ch {
                '\\' => {
                    self.next_char();
                }
                '"' => break,
                _ => {}
            }
        }
    }

    /// Whether the character last read is a `"` with no backslash before it.
    /// An escape reads a bare `"` only as the base of `\C-` or `\^`, and fails
    /// on it; that `"` closed the string, so nothing of the string is left.
    fn ended_on_bare_quote(&self) -> bool {
        let read = &self.source.as_bytes()[..self.offset()];
        read.ends_with(b"\"") && !read.ends_with(b"\\\"")
    }

    fn peek_char(&mut self) -> Option<char> {
        *self.peeked.get_or_insert_with(|| self.chars.next())
    }

    fn next_char(&mut self) -> Option<char> {
        let next = match self.peeked.take() {
            Some(peeked) => peeked,
            None => self.chars.next(),
        };
        next.inspect(|ch| {
            if *ch == '\n' {
                self.line += 1;
                self.pos = 0;
            } else {
                self.pos += 1;
            }
        })
    }

    fn read_string(&mut self) -> Option<Token> {
        self.next_char()?; // consume the opening '"'
        let start_pos = (self.line, self.pos);
        let mut output = String::new();
        while let Some(ch) = self.next_char() {
            match ch {
                // A backslash before a newline or a space reads as nothing.
                '\\' if matches!(self.peek_char(), Some('\n' | ' ')) => {
                    self.next_char();
                }
                '\\' => {
                    let escape = self.read_escape(true).and_then(|code| {
                        char::from_u32(code).ok_or_else(|| format!("Not a character: {code:#x}"))
                    });
                    match escape {
                        Ok(ch) => output.push(ch),
                        Err(desc) => {
                            let pos = (self.line, self.pos);
                            let span = Span::new(self.file_id, pos, pos);
                            let offset = self.last_char_offset();
                            if !self.ended_on_bare_quote() {
                                self.skip_rest_of_string();
                            }
                            return Some(Token::ParserError(ParserError::syntax_error(
                                desc, span, offset,
                            )));
                        }
                    }
                }
                '"' => {
                    return Some(Token::String {
                        span: Span {
                            file_id: self.file_id,
                            start: start_pos,
                            end: (self.line, self.pos),
                        },
                        value: output,
                    });
                }
                ch => output.push(ch),
            }
        }

        Some(Token::ParserError(ParserError::syntax_error(
            "Incomplete string literal".to_owned(),
            Span {
                file_id: self.file_id,
                start: start_pos,
                end: (self.line, self.pos),
            },
            self.token_start,
        )))
    }

    fn read_num_ident(&mut self) -> Option<Token> {
        let start_pos = (self.line, self.pos + 1);
        self.read_num_ident_impl(start_pos, String::new())
    }

    /// Read a `#x` / `#o` / `#b` integer literal. Caller has just
    /// peeked the radix-prefix letter; this consumes that letter and
    /// the digits that follow, with an optional `+`/`-` sign between
    /// the prefix and the first digit (Emacs allows `#x-10` for -16).
    fn read_radix_int(
        &mut self,
        start_pos: (usize, usize),
        radix: u32,
        prefix: &str,
    ) -> Option<Token> {
        self.next_char()?; // consume the prefix letter
        let mut digits = String::new();
        if matches!(self.peek_char(), Some('-' | '+')) {
            digits.push(self.next_char()?);
        }
        while let Some(c) = self.peek_char() {
            if c.is_digit(radix) {
                digits.push(c);
                self.next_char()?;
            } else {
                break;
            }
        }
        let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
        if digits.is_empty() || digits == "-" || digits == "+" {
            return Some(Token::ParserError(ParserError::syntax_error(
                format!("{prefix}: expected digits after radix prefix"),
                span,
                self.token_start,
            )));
        }
        match i64::from_str_radix(&digits, radix) {
            Ok(value) => Some(Token::Integer { span, value }),
            Err(e) => Some(Token::ParserError(ParserError::syntax_error(
                format!("{prefix}{digits}: {e}"),
                span,
                self.token_start,
            ))),
        }
    }

    /// Read a `?X` character literal. Returns the character's code point as an
    /// `Integer` token. `?\X` takes the escapes of `read_escape`.
    fn read_char_literal(&mut self) -> Option<Token> {
        let start_pos = (self.line, self.pos + 1);
        self.next_char()?; // consume '?'
        let value = match self.next_char() {
            Some('\\') => self.read_escape(false),
            Some(c) => Ok(c as u32),
            None => Err("Unexpected EOF after ?".to_string()),
        };
        let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
        Some(match value {
            Ok(value) => Token::Integer {
                span,
                value: value.into(),
            },
            Err(desc) => {
                Token::ParserError(ParserError::syntax_error(desc, span, self.token_start))
            }
        })
    }

    /// Read the escape after a backslash, as Emacs does: `\n`, `\s`, `\d` and
    /// the other letters, octal `\101`, hex `\x41`, `\u00e9`, `\U0001F600`, and
    /// control characters `\C-a` and `\^a`. Modifier keys such as `\M-a`,
    /// `\N{NAME}` and a malformed escape are errors. Any other character stands
    /// for itself.
    fn read_escape(&mut self, in_string: bool) -> Result<u32, String> {
        let ch = self
            .next_char()
            .ok_or_else(|| "Unexpected EOF after \\".to_string())?;
        let dash = self.peek_char() == Some('-');
        let code = match ch {
            'a' => 0x07,
            'b' => 0x08,
            'd' => 0x7f,
            'e' => 0x1b,
            'f' => 0x0c,
            'n' => '\n' as u32,
            'r' => '\r' as u32,
            't' => '\t' as u32,
            'v' => 0x0b,
            's' if !in_string && dash => {
                return Err("Modifier keys are not supported: \\s-".to_string());
            }
            's' => ' ' as u32,
            'M' | 'S' | 'H' | 'A' if dash => {
                return Err(format!("Modifier keys are not supported: \\{ch}-"));
            }
            'N' if self.peek_char() == Some('{') => {
                return Err("\\N{NAME} is not supported".to_string());
            }
            'N' => return Err("Expected opening brace after \\N".to_string()),
            '0'..='7' => self.read_digits(ch as u32 - '0' as u32, 8, 2).0,
            'x' => self.read_hex_escape()?,
            'u' | 'U' => {
                let len = if ch == 'u' { 4 } else { 8 };
                let (code, count) = self.read_digits(0, 16, len);
                if count != len {
                    return Err(format!("\\{ch} needs {len} hex digits"));
                }
                if code > 0x10ffff {
                    return Err(format!("Not a Unicode character: \\{ch}{code:x}"));
                }
                code
            }
            'C' if dash => {
                self.next_char();
                self.read_control_char(in_string)?
            }
            '^' => self.read_control_char(in_string)?,
            'C' | 'M' | 'S' | 'H' | 'A' => {
                return Err(format!(
                    "Invalid escape char syntax: \\{ch} not followed by -"
                ));
            }
            '\n' => return Err("Invalid escape char syntax: \\<newline>".to_string()),
            c => c as u32,
        };
        Ok(code)
    }

    /// Read up to `max` digits in `radix`, adding them to `value`. Returns the
    /// result and how many digits were read. Eight hex digits at most fit.
    fn read_digits(&mut self, mut value: u32, radix: u32, max: usize) -> (u32, usize) {
        let mut count = 0;
        while count < max
            && let Some(digit) = self.peek_char().and_then(|c| c.to_digit(radix))
        {
            self.next_char();
            value = value * radix + digit;
            count += 1;
        }
        (value, count)
    }

    /// Read the hex digits of a `\x` escape, up to `\xFFFFFFF`, the largest
    /// value Emacs takes.
    fn read_hex_escape(&mut self) -> Result<u32, String> {
        let (mut code, mut count) = (0, 0);
        while let Some(digit) = self.peek_char().and_then(|c| c.to_digit(16)) {
            self.next_char();
            code = code * 16 + digit;
            count += 1;
            if code > 0xfff_ffff {
                return Err(format!("Hex character out of range: \\x{code:x}..."));
            }
        }
        if count == 0 {
            return Err("\\x not followed by a hex digit".to_string());
        }
        Ok(code)
    }

    /// Read the character after `\C-` or `\^`, which may be an escape itself,
    /// and return its control character: `\C-a` is 1 and `\C-?` is 127. In a
    /// string, a control space is 0.
    fn read_control_char(&mut self, in_string: bool) -> Result<u32, String> {
        let base = match self.next_char() {
            // A `\s-` here is a modifier key, even in a string.
            Some('\\') => self.read_escape(false)?,
            Some(c) => c as u32,
            None => return Err("Unexpected EOF after \\C-".to_string()),
        };
        match base {
            0x20 if in_string => Ok(0),
            0x3f => Ok(0x7f),
            0x40..=0x5f | 0x61..=0x7a => Ok(base & 0x1f),
            _ => Err(match char::from_u32(base) {
                Some(c) => format!("No control character for {c:?}"),
                None => format!("No control character for {base}"),
            }),
        }
    }

    /// Read the rest of a number or symbol token after `output`.
    fn read_num_ident_impl(
        &mut self,
        start_pos: (usize, usize),
        mut output: String,
    ) -> Option<Token> {
        while let Some(ch) = self.peek_char() {
            if TOKEN_ENDS.contains(ch) {
                break;
            }
            output.push(ch);
            self.next_char()?;
        }
        let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
        Some(number_or_symbol(output, span, self.token_start))
    }
}

/// Read `text` as Emacs does: an integer like `+1` or `10.`, a float like `.5`,
/// `1e3` or `-1.0e+INF`, or else a symbol. START is where `text` starts in the
/// source, in bytes.
fn number_or_symbol(text: String, span: Span, start: usize) -> Token {
    let unsigned = text.strip_prefix(['+', '-']).unwrap_or(&text);
    let lead = unsigned.bytes().take_while(u8::is_ascii_digit).count();
    let rest = &unsigned[lead..];
    let rest = rest.strip_prefix('.').unwrap_or(rest);
    let trail = rest.bytes().take_while(u8::is_ascii_digit).count();
    let tail = &rest[trail..];

    if lead + trail == 0 {
        return Token::Ident { span, value: text };
    }
    if trail == 0 && tail.is_empty() {
        let digits = text.strip_suffix('.').unwrap_or(&text);
        return match digits.parse::<i64>() {
            Ok(value) => Token::Integer { span, value },
            Err(e) => Token::ParserError(ParserError::syntax_error(
                format!("{e}: {text}"),
                span,
                start,
            )),
        };
    }
    // `-f64::NAN` flips the sign bit.
    let signed = |value: f64| if text.starts_with('-') { -value } else { value };
    let float = match tail.strip_prefix(['e', 'E']) {
        Some("+INF") => Some(signed(f64::INFINITY)),
        Some("+NaN") => Some(signed(f64::NAN)),
        // Rust reads the same shape: digits, a dot, digits and an exponent.
        _ => text.parse::<f64>().ok(),
    };
    match float {
        Some(value) => Token::Float { span, value },
        None => Token::Ident { span, value: text },
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while matches!(self.peek_char(), Some(' ' | '\t' | '\r' | '\n')) {
                self.next_char();
            }
            self.token_start = self.offset();
            let ch = self.peek_char()?;

            match ch {
                '(' => {
                    self.next_char()?;
                    return Some(Token::OpenParen {
                        span: Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                    });
                }
                ')' => {
                    self.next_char()?;
                    return Some(Token::CloseParen {
                        span: Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                    });
                }
                '[' | ']' => {
                    // Tulisp has no vector type; reject the syntax
                    // outright so pasted Elisp fails with a clear
                    // message instead of brackets being swallowed
                    // into symbol tokens.
                    self.next_char()?;
                    return Some(Token::ParserError(ParserError::syntax_error(
                        "Vector syntax is not supported".to_string(),
                        Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                        self.token_start,
                    )));
                }
                '\'' => {
                    self.next_char()?;
                    return Some(Token::Quote {
                        span: Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                    });
                }
                '`' => {
                    self.next_char()?;
                    return Some(Token::Backtick {
                        span: Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                    });
                }
                '.' => {
                    let start_pos = (self.line, self.pos + 1);
                    self.next_char()?;
                    if matches!(self.peek_char(), Some('0'..='9')) {
                        return self.read_num_ident_impl(start_pos, String::from("."));
                    }
                    return Some(Token::Dot {
                        span: Span::new(self.file_id, (self.line, self.pos), (self.line, self.pos)),
                    });
                }
                '#' => {
                    // Capture the sigil's position before consuming
                    // it. Computing the start column from the
                    // post-consume `pos` would underflow if the
                    // tokenizer ever reset `pos` (e.g. on a newline)
                    // between the sigil and its companion char —
                    // can't happen today because `#'` / `#x` etc.
                    // must be adjacent, but the explicit start_pos
                    // keeps the span correct under any future
                    // tokenizer change.
                    let start_pos = (self.line, self.pos + 1);
                    self.next_char()?;
                    // `peek_char()?` would silently terminate the
                    // tokenizer at EOF, hiding the bad input. Match
                    // explicitly so we can surface a `ParserError`.
                    match self.peek_char() {
                        Some('\'') => {
                            self.next_char()?;
                            return Some(Token::SharpQuote {
                                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                            });
                        }
                        Some('x' | 'X') => return self.read_radix_int(start_pos, 16, "#x"),
                        Some('o' | 'O') => return self.read_radix_int(start_pos, 8, "#o"),
                        Some('b' | 'B') => return self.read_radix_int(start_pos, 2, "#b"),
                        Some(_) => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unknown token #.  Did you mean #' ?".to_string(),
                                Span::new(self.file_id, start_pos, (self.line, self.pos)),
                                self.token_start,
                            )));
                        }
                        None => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unexpected EOF after #".to_string(),
                                Span::new(self.file_id, start_pos, (self.line, self.pos)),
                                self.token_start,
                            )));
                        }
                    }
                }
                '?' => return self.read_char_literal(),
                ',' => {
                    let start_pos = (self.line, self.pos + 1);
                    self.next_char()?;
                    match self.peek_char() {
                        Some('@') => {
                            self.next_char()?;
                            return Some(Token::Splice {
                                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                            });
                        }
                        Some(_) => {
                            return Some(Token::Comma {
                                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                            });
                        }
                        None => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unexpected EOF after ,".to_string(),
                                Span::new(self.file_id, start_pos, (self.line, self.pos)),
                                self.token_start,
                            )));
                        }
                    }
                }
                '"' => {
                    return self.read_string();
                }
                ';' if self.keep_comments => {
                    while self.peek_char().is_some_and(|ch| ch != '\n') {
                        self.next_char();
                    }
                    return Some(Token::Comment);
                }
                ';' => while self.next_char()? != '\n' {},
                _ => return self.read_num_ident(),
            }
        }
    }
}

struct Parser<'a, 'b> {
    file_id: usize,
    tokenizer: Peekable<Tokenizer<'a>>,
    ctx: &'b mut TulispContext,
    ints: HashMap<i64, TulispObject>,
    /// Current parse nesting depth, bounded by `ctx.max_nesting_depth()`.
    depth: u32,
    #[cfg(feature = "etags")]
    follow_load_files: bool,
}

impl Parser<'_, '_> {
    fn new<'b, 'a>(
        ctx: &'b mut TulispContext,
        file_id: usize,
        program: &'a str,
        #[cfg(feature = "etags")] follow_load_files: bool,
    ) -> Parser<'a, 'b> {
        Parser {
            file_id,
            tokenizer: Tokenizer::new(file_id, program).peekable(),
            ctx,
            ints: Default::default(),
            depth: 0,
            #[cfg(feature = "etags")]
            follow_load_files,
        }
    }

    fn parse_list(&mut self, start_span: Span) -> Result<TulispObject, Error> {
        let mut builder = crate::cons::ListBuilder::new();
        let mut got_dot = false;
        let mut full_span: Option<Span> = None;
        loop {
            let Some(token) = self.tokenizer.peek() else {
                return Err(Error::parsing_error("Unclosed list".to_string())
                    .with_trace(TulispObject::nil().with_span(Some(start_span))));
            };
            match token {
                Token::CloseParen { span: end_span } => {
                    full_span = Some(Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: end_span.end,
                    });
                    break;
                }
                Token::Dot { .. } => {
                    got_dot = true;
                    break;
                }
                // The parser's tokenizer skips comments; one that reaches here
                // anyway is skipped too.
                Token::Comment => {
                    let _ = self.tokenizer.next();
                }
                _ => {
                    let next = self.parse_value()?.unwrap();
                    builder.push(next);
                }
            }
        }

        // consume a close paren or a dot.
        let _ = self.tokenizer.next();

        if got_dot {
            let Some(next) = self.parse_value()? else {
                return Err(Error::parsing_error("Unexpected EOF after dot".to_string())
                    .with_trace(TulispObject::nil().with_span(Some(start_span))));
            };
            if let Some(Token::CloseParen { span: end_span }) = self.tokenizer.next() {
                full_span = Some(Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end: end_span.end,
                });
            } else {
                return Err(Error::parsing_error(
                    "Expected only one item in list after dot.".to_string(),
                )
                .with_trace(next));
            }
            builder.append(next)?;
        }

        let inner = builder.build().with_span(full_span);

        #[cfg(feature = "etags")]
        if self.follow_load_files
            && let Ok("load") = inner.car()?.as_symbol().as_ref().map(|x| x.as_str())
            && let Ok(filename) = inner.cadr().and_then(|c| c.as_string())
        {
            // Only (load "literal-path") gets followed for tag
            // discovery. (load some-var) / (load (compute-path))
            // get silently skipped — without a static path we
            // can't know which file to descend into, and aborting
            // the parse here would drop every defun that comes
            // after the dynamic-load form.
            if let Ok(contents) = std::fs::read_to_string(&filename) {
                self.ctx.filenames.push(filename.to_string());
                let _ = parse(
                    self.ctx,
                    self.ctx.filenames.len() - 1,
                    contents.as_str(),
                    self.follow_load_files,
                );
            }
        }

        // A name that is no plain symbol, such as `t`, gets no tag.
        #[cfg(feature = "etags")]
        if let Ok("defun" | "defmacro" | "defvar") =
            inner.car()?.as_symbol().as_ref().map(|x| x.as_str())
            && let Ok(name) = inner.cadr().and_then(|name| name.as_symbol())
            && let Some(span) = inner.span()
        {
            self.ctx
                .tags_table
                .entry(self.ctx.filenames[self.file_id].clone())
                .or_default()
                .insert(name, span.start.0);
        }
        Ok(inner)
    }

    fn parse_value(&mut self) -> Result<Option<TulispObject>, Error> {
        // Every nested list / quote re-enters here, so bounding this
        // depth bounds the parser's native recursion: deeply nested
        // input raises a catchable error instead of overflowing the
        // stack. Downstream walks (compile, `macroexpand`,
        // …) see only structure this deep, so they're bounded too.
        let limit = self.ctx.max_nesting_depth();
        if self.depth >= limit {
            return Err(Error::parsing_error(format!(
                "Lisp nesting exceeds max-nesting-depth ({})",
                limit
            )));
        }
        self.depth += 1;
        let r = self.parse_value_inner();
        self.depth -= 1;
        r
    }

    fn parse_value_inner(&mut self) -> Result<Option<TulispObject>, Error> {
        let Some(token) = self.tokenizer.next() else {
            return Ok(None);
        };
        match token {
            Token::OpenParen { span } => self.parse_list(span).map(Some),
            Token::CloseParen { span } => Err(Error::parsing_error(
                "Unexpected closing parenthesis".to_string(),
            )
            .with_trace(TulispValue::Nil.into_ref(Some(span)))),
            Token::SharpQuote { span } => {
                let Some(next) = self.parse_value()? else {
                    return Err(Error::parsing_error("Unexpected EOF".to_string())
                        .with_trace(TulispValue::Nil.into_ref(Some(span))));
                };
                // `#'X` reads as `(function X)`, as in Emacs.
                let span = match next.span() {
                    Some(next_span) => Span::new(span.file_id, span.start, next_span.end),
                    None => span,
                };
                let function = self.ctx.intern("function");
                Ok(Some(
                    TulispObject::cons(function, TulispObject::cons(next, TulispObject::nil()))
                        .with_span(Some(span)),
                ))
            }
            Token::Quote { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::parsing_error("Unexpected EOF".to_string())
                            .with_trace(TulispValue::Nil.into_ref(Some(span))));
                    }
                };
                Ok(Some(
                    TulispValue::Quote { value: next }.into_ref(Some(span)),
                ))
            }
            Token::Backtick { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::parsing_error("Unexpected EOF".to_string())
                            .with_trace(TulispValue::Nil.into_ref(Some(span))));
                    }
                };
                Ok(Some(
                    TulispValue::Backquote { value: next }.into_ref(Some(span)),
                ))
            }
            Token::Dot { span } => Err(Error::parsing_error("Unexpected dot".to_string())
                .with_trace(TulispValue::Nil.into_ref(Some(span)))),
            Token::Comma { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::parsing_error("Unexpected EOF".to_string())
                            .with_trace(TulispValue::Nil.into_ref(Some(span))));
                    }
                };
                Ok(Some(
                    TulispValue::Unquote { value: next }.into_ref(Some(span)),
                ))
            }
            Token::Splice { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::parsing_error("Unexpected EOF".to_string())
                            .with_trace(TulispValue::Nil.into_ref(Some(span))));
                    }
                };
                Ok(Some(
                    TulispValue::Splice { value: next }.into_ref(Some(span)),
                ))
            }
            // Each string literal gets a fresh `TulispObject` —
            // matching Emacs' `(eq "hello" "hello") => nil`. Interning
            // would make `eq` collide and, more importantly, alias any
            // future `aset`-style mutation across unrelated literals.
            Token::String { span, value } => {
                Ok(Some(TulispValue::String { value }.into_ref(Some(span))))
            }

            Token::Integer { span, value } => Ok(Some(match self.ints.get(&value) {
                Some(vv) => vv.with_span(Some(span)),
                None => {
                    let vv = TulispValue::Number {
                        value: Number::Int(value),
                    }
                    .into_ref(Some(span));
                    self.ints.insert(value, vv.clone());
                    vv
                }
            })),
            Token::Float { span, value } => Ok(Some(
                TulispValue::Number {
                    value: Number::Float(value),
                }
                .into_ref(Some(span)),
            )),
            Token::Ident { span, value } => Ok(Some(self.ctx.intern(&value).with_span(Some(span)))),
            // The parser's tokenizer skips comments; one that reaches here
            // anyway is skipped too.
            Token::Comment => self.parse_value_inner(),
            Token::ParserError(err) => {
                Err(Error::parsing_error(format!("{:?} {}", err.kind, err.desc))
                    .with_trace(TulispValue::Nil.into_ref(Some(err.span))))
            }
        }
    }

    fn parse(&mut self) -> Result<TulispObject, Error> {
        let mut builder = crate::cons::ListBuilder::new();
        while let Some(next) = self.parse_value()? {
            builder.push(next);
        }
        Ok(builder.build())
    }
}

/// Marks the tail calls in `body`, the body of the function `name`, as
/// `(Bounce FN ARGS...)`, looking through `progn`, `let`, `let*`, `if`
/// and `cond`. It marks a call to `name` itself and a call to a function
/// already in the compiler's `defun_args`. A call to anything else,
/// such as a function held in a variable or one not registered yet,
/// stays an ordinary call. `compile_fn_defun_bounce_call` compiles a
/// marked call.
pub(crate) fn mark_tail_calls(
    ctx: &mut TulispContext,
    name: &TulispObject,
    body: TulispObject,
) -> TulispObject {
    let mut items = body.base_iter();
    let forms = items.by_ref().collect::<Vec<_>>();
    // A dotted body is left as it is, for its own form to report.
    if items.take_error().is_err() {
        return body;
    }
    mark_body(ctx, name, forms)
}

/// FORMS, a body, as a list with the tail calls in its last form
/// marked.
fn mark_body(
    ctx: &mut TulispContext,
    name: &TulispObject,
    forms: impl IntoIterator<Item = TulispObject>,
) -> TulispObject {
    let mut forms = forms.into_iter().collect::<Vec<_>>();
    if let Some(last) = forms.pop() {
        forms.push(mark_tail_form(ctx, name, last));
    }
    forms.into()
}

/// FORM, a form in tail position, with its tail calls marked; FORM
/// itself when this pass cannot read it.
fn mark_tail_form(
    ctx: &mut TulispContext,
    name: &TulispObject,
    form: TulispObject,
) -> TulispObject {
    match try_mark_tail_form(ctx, name, &form) {
        Ok(marked) => marked.with_span(form.span()),
        Err(_) => form,
    }
}

fn try_mark_tail_form(
    ctx: &mut TulispContext,
    name: &TulispObject,
    form: &TulispObject,
) -> Result<TulispObject, Error> {
    if !form.consp() {
        return Ok(form.clone());
    }
    let head = form.car()?;
    // A head that is not a symbol, such as a `(lambda ...)` list, is
    // no function the compiler knows: the call stays as it is.
    let Ok(head_name) = head.as_symbol() else {
        return Ok(form.clone());
    };
    let is_self_call = head.eq(name);
    let is_known_vm_defun = ctx
        .compiler
        .as_ref()
        .is_some_and(|c| c.defun_args.contains_key(&head.addr_as_usize()));
    if is_self_call || is_known_vm_defun {
        // The marker is its own head, so no function or variable of
        // the program can shadow it.
        return Ok(TulispObject::cons(
            TulispValue::Bounce.into_ref(None),
            form.clone(),
        ));
    }
    match head_name.as_str() {
        "progn" => Ok(TulispObject::cons(
            head,
            mark_tail_calls(ctx, name, form.cdr()?),
        )),
        "let" | "let*" => {
            let rest = form.cdr()?;
            if !rest.consp() {
                return Ok(form.clone());
            }
            // The bindings are never marked; only the body is.
            Ok(TulispObject::cons(
                head,
                TulispObject::cons(rest.car()?, mark_tail_calls(ctx, name, rest.cdr()?)),
            ))
        }
        "if" => {
            let (_, condition, then_form, else_body): (
                TulispObject,
                TulispObject,
                TulispObject,
                Rest<TulispObject>,
            ) = form.destructure(ctx)?;
            let then_form = mark_tail_form(ctx, name, then_form);
            let else_body = mark_body(ctx, name, else_body);
            Ok(TulispObject::cons(
                head,
                TulispObject::cons(condition, TulispObject::cons(then_form, else_body)),
            ))
        }
        "cond" => {
            let (_, clauses): (TulispObject, Rest<TulispObject>) = form.destructure(ctx)?;
            let mut marked = vec![head];
            for clause in clauses {
                // A clause this pass cannot read stays as it is.
                marked.push(
                    match clause.destructure::<(TulispObject, Rest<TulispObject>)>(ctx) {
                        Ok((condition, body)) => {
                            TulispObject::cons(condition, mark_body(ctx, name, body))
                        }
                        Err(_) => clause,
                    },
                );
            }
            Ok(marked.into())
        }
        _ => Ok(form.clone()),
    }
}

pub fn parse(
    ctx: &mut TulispContext,
    file_id: usize,
    program: &str,
    #[cfg(feature = "etags")] follow_load_files: bool,
) -> Result<TulispObject, Error> {
    let parsed = Parser::new(
        ctx,
        file_id,
        program,
        #[cfg(feature = "etags")]
        follow_load_files,
    )
    .parse();
    parsed.map_err(|err| err.with_file_names(ctx))
}

#[cfg(test)]
mod tests {
    use super::{Token, Tokenizer};
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_equal_fresh, eval_assert_error,
        eval_assert_error_line,
    };
    use crate::{Error, TulispContext};

    // Each token's range covers its own text, in bytes, multi-byte characters
    // included.
    #[test]
    fn a_token_range_covers_its_text() {
        let source = "(é \"ü\" ?ä #'f 1.5) ; ñ\n,@x";
        let mut tokenizer = Tokenizer::new(0, source).with_comments();
        let mut texts = Vec::new();
        while tokenizer.next().is_some() {
            texts.push(&source[tokenizer.token_range()]);
        }
        assert_eq!(
            texts,
            [
                "(", "é", "\"ü\"", "?ä", "#'", "f", "1.5", ")", "; ñ", ",@", "x"
            ]
        );
    }

    // A tokenizer not made `with_comments` skips comments, as before.
    #[test]
    fn comments_are_skipped_by_default() {
        let mut tokenizer = Tokenizer::new(0, "; a\nx ; b");
        assert!(matches!(tokenizer.next(), Some(Token::Ident { .. })));
        assert!(tokenizer.next().is_none());
    }

    // A comment token ends before its newline, or at the end of the input.
    #[test]
    fn a_comment_ends_at_its_newline_or_the_input() {
        let source = "a ;one\n;two";
        let mut tokenizer = Tokenizer::new(0, source).with_comments();
        let mut texts = Vec::new();
        while tokenizer.next().is_some() {
            texts.push(&source[tokenizer.token_range()]);
        }
        assert_eq!(texts, ["a", ";one", ";two"]);
    }

    // After a bad escape the tokenizer reads on to the string's closing quote,
    // so the text after the string is read as code.
    #[test]
    fn a_bad_escape_ends_at_the_closing_quote() {
        let source = r#""a\M-b c" d"#;
        let mut tokenizer = Tokenizer::new(0, source);
        assert!(matches!(tokenizer.next(), Some(Token::ParserError(_))));
        assert_eq!(&source[tokenizer.token_range()], r#""a\M-b c""#);
        assert!(matches!(tokenizer.next(), Some(Token::Ident { value, .. }) if value == "d"));
        assert!(tokenizer.next().is_none());
    }

    // A string's span starts at its opening quote and ends at its closing one,
    // as a list's covers its parentheses.
    #[test]
    fn a_string_span_covers_its_quotes() {
        let ctx = &mut TulispContext::new();
        let forms = ctx.eval_string(r#"'(x "ab" (y))"#).unwrap();
        let string = forms.cdr().unwrap().car().unwrap();
        let list = forms.cddr().unwrap().car().unwrap();
        let (string, list) = (string.span().unwrap(), list.span().unwrap());
        assert_eq!((string.start, string.end), ((1, 5), (1, 8)));
        assert_eq!((list.start, list.end), ((1, 10), (1, 12)));
    }

    // Marking tail calls rejects no form: one it cannot read is left
    // unmarked, and a malformed one is reported by its own form.
    #[test]
    fn the_tail_call_pass_rejects_no_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun f (a) (cond (a . b))) (list (f nil) (f 1))",
            "'(nil nil)",
        );
        for (program, form) in [
            ("(defun g () (cond . 3))", "(cond . 3)"),
            ("(defun h (a) (cond (a) . 3))", "(cond (a) . 3)"),
            ("(defun p () (progn . 3))", "(progn . 3)"),
            ("(defun q (n) (progn 1 (q n) . 3))", "(progn 1 (q n) . 3)"),
        ] {
            let Err(err) = ctx.eval_string(program) else {
                panic!("{program} compiled");
            };
            let message = err.to_string();
            assert!(
                message.starts_with("ERR TypeMismatch: expected list, got: 3\n"),
                "{message}"
            );
            assert!(message.contains(&format!("at {form}\n")), "{message}");
        }
    }

    // A `cond` clause the pass cannot read stays as it is, and the
    // other clauses are still marked; a `let` with no body keeps its
    // bindings unmarked.
    #[test]
    fn marking_keeps_what_it_cannot_read() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun f (n) (cond ((= n 0) 0) (nil . x) (t (f (- n 1))))) (f 100)",
            "0",
        );
        eval_assert_equal(ctx, "(defun x () (let (x))) (x)", "nil");
        eval_assert_equal(ctx, "(defun g () 1) (defun h () (let* (g y))) (h)", "nil");
    }

    // A `cond` clause that is not a list is reported by `cond` itself
    // when its defun is compiled, not by tail-call marking.
    #[test]
    fn a_non_list_cond_clause_is_reported_by_cond() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(defun k () (cond x))",
            "ERR TypeMismatch: Expected list, got: x\n\
             <eval_string>:1.13-1.20:  at (cond x)\n\
             <eval_string>:1.1-1.21:  at (defun k nil (cond x))\n",
        );
    }

    // A call whose head is a `(lambda ...)` list can be in tail
    // position, alone or in a branch.
    #[test]
    fn a_lambda_call_can_be_a_tail_call() {
        eval_assert_equal_fresh("(defun f () ((lambda (x) (list 'l x)) 1)) (f)", "'(l 1)");
        eval_assert_equal_fresh(
            "(defun f (n) (if (= n 0) ((lambda () 'done)) (f (- n 1)))) (f 3)",
            "'done",
        );
    }

    // A dotted-pair tail with no value before end-of-input must
    // parse-error, not panic. Regression: `parse_list` unwrapped
    // `parse_value()`, which returns `None` at EOF.
    #[test]
    fn dotted_pair_eof_errors_cleanly() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(1 .",
            "ERR ParsingError: Unexpected EOF after dot\n<eval_string>:1.1-1.1:  at nil\n",
        );
        eval_assert_error(
            &mut ctx,
            "(.",
            "ERR ParsingError: Unexpected EOF after dot\n<eval_string>:1.1-1.1:  at nil\n",
        );
    }

    // Deeply nested input raises a catchable parse error instead of
    // overflowing the stack. The cap (256 in debug) sits above this
    // harness thread's ~2 MiB ceiling, so run on an 8 MiB thread (the
    // size the non-test default targets) — an overflow would abort the
    // whole process rather than fail the assertion.
    #[test]
    fn deeply_nested_input_errors_without_overflowing() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let mut ctx = TulispContext::new();
                let deep = format!("'{}{}", "(".repeat(100_000), ")".repeat(100_000));
                let err = ctx.eval_string(&deep).unwrap_err();
                assert!(
                    err.to_string().contains("max-nesting-depth"),
                    "expected a nesting-depth error, got: {}",
                    err
                );
                // Shallow nesting still parses and evaluates fine.
                assert!(ctx.eval_string("'((((1))))").is_ok());
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // Tulisp has no vector type. The reader must say so instead of
    // silently swallowing the brackets into symbol tokens and
    // failing later with a baffling unrelated error.
    #[test]
    fn vector_syntax_rejected() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(princ [1 2 3])",
            r#"ERR ParsingError: SyntaxError Vector syntax is not supported
<eval_string>:1.8-1.8:  at nil
"#,
        );
        eval_assert_error(
            &mut ctx,
            "(princ ])",
            r#"ERR ParsingError: SyntaxError Vector syntax is not supported
<eval_string>:1.8-1.8:  at nil
"#,
        );
        // A bracket also terminates a symbol token, as in Emacs, so
        // `foo[1]` can't sneak through as a single symbol name.
        eval_assert_error(
            &mut ctx,
            "(princ 'foo[1])",
            r#"ERR ParsingError: SyntaxError Vector syntax is not supported
<eval_string>:1.12-1.12:  at nil
"#,
        );
    }

    // Reading a program defines nothing: a quoted definition is data.
    #[test]
    fn a_quoted_definition_defines_nothing() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(progn '(defun qf () 1) (condition-case nil (qf) (error 'undefined)))",
            "'undefined",
        );
        eval_assert_equal(
            ctx,
            "(progn '(defmacro qm () 1) (condition-case nil (qm) (error 'undefined)))",
            "'undefined",
        );
        eval_assert_equal(
            ctx,
            "'(defvar qx) (defun rq () qx)
             (let ((qx 5)) (condition-case nil (rq) (error 'lexical)))",
            "'lexical",
        );
    }

    // A backquoted definition is a template: it reads as data, and a
    // macro can fill it in.
    #[test]
    fn a_backquoted_definition_is_a_template() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defmacro mkdef (name) `(defun ,name () 7)) (mkdef bq-f) (bq-f)",
            "7",
        );
    }

    // The cap admits exactly `max_nesting_depth` reader levels and
    // rejects the next one. `deeply_nested_input_errors_without_
    // overflowing` (100_000 levels against a cap in the hundreds)
    // cannot see an off-by-one.
    #[test]
    fn nesting_cap_boundary_is_exact() {
        // `max_nesting_depth` is 4x `max_eval_depth`, so 40 here;
        // the leading quote is itself one reader level.
        let parse_nested = |n: usize| {
            let mut ctx = TulispContext::new();
            ctx.set_max_eval_depth(10);
            ctx.eval_string(&format!("'{}{}", "(".repeat(n), ")".repeat(n)))
        };
        assert!(parse_nested(39).is_ok(), "40 levels must be accepted");
        assert!(parse_nested(40).is_err(), "41 levels must be rejected");
    }

    #[test]
    fn test_parse_does_not_eval_list_cars() -> Result<(), Error> {
        // A defun whose body has a cond with a side-effecting predicate
        // must not fire that predicate during defun registration.
        eval_assert_equal_fresh(
            "(progn
           (setq c 0)
           (defun bump () (setq c (1+ c)) c)
           (defun nc () (cond ((bump) 1) (t 2)))
           c)",
            "0",
        );
        // Calling the defun runs the predicate exactly once.
        eval_assert_equal_fresh(
            "(progn
           (setq c 0)
           (defun bump () (setq c (1+ c)) c)
           (defun nc () (cond ((bump) 1) (t 2)))
           (nc)
           c)",
            "1",
        );
        // A list car that's a lambda is called.
        eval_assert_equal_fresh("((lambda (x) (* x 2)) 21)", "42");
        // let binding-init expressions don't fire at parse time either.
        eval_assert_equal_fresh(
            "(progn
           (setq c 0)
           (defun bump () (setq c (1+ c)) c)
           (defun nl () (let ((a (bump))) a))
           c)",
            "0",
        );
        Ok(())
    }
    // `?X` reads as the character's code point.
    #[test]
    fn character_literals_read_as_code_points() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "?A", "65");
        eval_assert_equal(ctx, "?z", "122");
        eval_assert_equal(ctx, r"?\n", "10");
        eval_assert_equal(ctx, r"?\t", "9");
        eval_assert_equal(ctx, r"?\\", "92");
        eval_assert_equal(ctx, r"?\0", "0");
    }

    #[test]
    fn character_escapes() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r"?\s", "32");
        eval_assert_equal(ctx, r"?\ ", "32");
        eval_assert_equal(ctx, r"?\d", "127");
        eval_assert_equal(ctx, r"?\101", "65");
        eval_assert_equal(ctx, r"?\x41", "65");
        eval_assert_equal(ctx, r"?\x0041", "65");
        eval_assert_equal(ctx, r"?\xFFFFF00", "268435200");
        eval_assert_equal(ctx, r"?\u00e9", "233");
        eval_assert_equal(ctx, r"?\U0001F600", "128512");
        eval_assert_equal(ctx, r"?\C-a", "1");
        eval_assert_equal(ctx, r"?\C-A", "1");
        eval_assert_equal(ctx, r"?\^a", "1");
        eval_assert_equal(ctx, r"?\^@", "0");
        eval_assert_equal(ctx, r"?\^?", "127");
        eval_assert_equal(ctx, r"?\(", "40");
        eval_assert_equal(ctx, r"?\8", "56");
        eval_assert_equal(ctx, r"(list ?\s ?\d)", "'(32 127)");
    }

    // A string takes the escapes of a character literal. A backslash before a
    // newline or a space reads as nothing, and can end a hex escape.
    #[test]
    fn string_escapes() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let cases = [
            (r#""\(x\)""#, "(x)"),
            (r#""a\sb""#, "a b"),
            (r#""\s-a""#, " -a"),
            (r#""\d""#, "\u{7f}"),
            (r#""\101\102""#, "AB"),
            (r#""\x41\ b""#, "Ab"),
            (r#""\u00e9""#, "é"),
            ("\"a\\\nb\"", "ab"),
            (r#""\C-a\^?""#, "\u{1}\u{7f}"),
            (r#""\q""#, "q"),
            (r#""\1012""#, "A2"),
            (r#""\u00e9a\U0001F600a""#, "\u{e9}a\u{1F600}a"),
            (r#""\C-\ x\C- x\^ x""#, "\u{0}x\u{0}x\u{0}x"),
        ];
        for (program, expected) in cases {
            assert_eq!(
                ctx.eval_string(program)?.as_string()?,
                expected,
                "{program}"
            );
        }
        Ok(())
    }

    #[test]
    fn bad_string_escapes_are_errors() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (r#""\M-a""#, r"Modifier keys are not supported: \M-"),
            (r#""\C-%""#, "No control character for '%'"),
            (r#""\x110000""#, "Not a character: 0x110000"),
            (r#""\xFFFFFFF""#, "Not a character: 0xfffffff"),
            (r#""\ud800""#, "Not a character: 0xd800"),
            (
                r#""\Ca""#,
                r"Invalid escape char syntax: \C not followed by -",
            ),
            (r#""\Na""#, r"Expected opening brace after \N"),
            (r#""\C-\s-a""#, r"Modifier keys are not supported: \s-"),
        ];
        for (program, desc) in cases {
            let line = format!("ERR ParsingError: SyntaxError {desc}");
            eval_assert_error_line(ctx, program, &line);
        }
    }

    // Modifier keys, `\N{NAME}` and malformed escapes are read errors.
    #[test]
    fn bad_character_escapes_are_errors() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (r"?\M-a", r"Modifier keys are not supported: \M-"),
            (r"?\s-a", r"Modifier keys are not supported: \s-"),
            (r"?\H-a", r"Modifier keys are not supported: \H-"),
            (r"?\C-%", "No control character for '%'"),
            (r"?\N{LATIN SMALL LETTER A}", r"\N{NAME} is not supported"),
            (r"?\x", r"\x not followed by a hex digit"),
            (r"?\u00e", r"\u needs 4 hex digits"),
            (r"?\U0001F60", r"\U needs 8 hex digits"),
            (r"?\U00110000", r"Not a Unicode character: \U110000"),
            (r"?\S-a", r"Modifier keys are not supported: \S-"),
            (r"?\A-a", r"Modifier keys are not supported: \A-"),
            (r"?\C", r"Invalid escape char syntax: \C not followed by -"),
            (r"?\Ma", r"Invalid escape char syntax: \M not followed by -"),
            (r"?\S", r"Invalid escape char syntax: \S not followed by -"),
            (r"?\N", r"Expected opening brace after \N"),
            (r"?\C- ", "No control character for ' '"),
            (r"?\x10000000", r"Hex character out of range: \x10000000..."),
            (
                r"?\x123456789",
                r"Hex character out of range: \x12345678...",
            ),
            ("?\\\n", r"Invalid escape char syntax: \<newline>"),
        ];
        for (program, desc) in cases {
            let line = format!("ERR ParsingError: SyntaxError {desc}");
            eval_assert_error_line(ctx, program, &line);
        }
    }

    // `#x` / `#X` hex, `#o` octal, `#b` binary. The sign goes between the
    // prefix and the digits.
    #[test]
    fn radix_prefixed_integers() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "#x10", "16");
        eval_assert_equal(ctx, "#xff", "255");
        eval_assert_equal(ctx, "#xFF", "255");
        eval_assert_equal(ctx, "#X10", "16");
        eval_assert_equal(ctx, "#o10", "8");
        eval_assert_equal(ctx, "#b1010", "10");
        eval_assert_equal(ctx, "#x-10", "-16");
    }

    // Scientific notation reads as a float. `e5`, `1ee5` and `1e` read as
    // symbols.
    #[test]
    fn scientific_notation_reads_as_float() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(+ 1e5 1)", "100001.0");
        eval_assert_equal(ctx, "(+ 1E5 1)", "100001.0");
        eval_assert_equal(ctx, "(+ 1.5e2 0)", "150.0");
        eval_assert_equal(ctx, "(+ 1e+5 0)", "100000.0");
        eval_assert_equal(ctx, "(+ -1.5e-3 0)", "-0.0015");
        eval_assert_equal(ctx, "(integerp 1e5)", "nil");
        eval_assert_equal(ctx, "(floatp 1e5)", "t");
        eval_assert_equal(ctx, "(progn (setq e5 7) e5)", "7");
        eval_assert_equal(ctx, "(progn (setq 1ee5 9) 1ee5)", "9");
        eval_assert_equal(ctx, "(progn (setq 1e 11) 1e)", "11");
    }

    // `<mantissa>e+INF` and `<mantissa>e+NaN`, uppercase and `e+` only. Only
    // the mantissa's sign matters. Other spellings read as symbols.
    #[test]
    fn infinity_and_nan_literals() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(format "%S" 1.0e+INF)"#, r#""1.0e+INF""#);
        eval_assert_equal(ctx, r#"(format "%S" -1.0e+INF)"#, r#""-1.0e+INF""#);
        eval_assert_equal(ctx, r#"(format "%S" 0.0e+NaN)"#, r#""0.0e+NaN""#);
        eval_assert_equal(ctx, r#"(format "%S" -0.0e+NaN)"#, r#""-0.0e+NaN""#);
        eval_assert_equal(ctx, r#"(format "%S" 5.5e+INF)"#, r#""1.0e+INF""#);
        eval_assert_equal(ctx, r#"(format "%S" 1e+INF)"#, r#""1.0e+INF""#);
        eval_assert_equal(ctx, r#"(format "%S" -1e999)"#, r#""-1.0e+INF""#);
        eval_assert_equal(ctx, "(progn (setq 1.0e+inf 5) 1.0e+inf)", "5");
        eval_assert_equal(ctx, "(progn (setq 1.0e-INF 5) 1.0e-INF)", "5");
        eval_assert_equal(ctx, "(progn (setq inf 5) inf)", "5");
    }

    // Numbers read as Emacs reads them: a leading `+` and a trailing `.` still
    // give an integer, and a token that is no number is a symbol.
    #[test]
    fn numbers_read_as_in_emacs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list +1 10. -10. +1.5 +.5 -.5 1.e3)",
            "'(1 10 -10 1.5 0.5 -0.5 1000.0)",
        );
        eval_assert(ctx, "(integerp +1)");
        eval_assert(ctx, "(integerp 10.)");
        eval_assert(ctx, "(floatp 1.e3)");
        for symbol in [
            "1_000", "1_000.5", ".1_5", "+.", "-.", "1.5.3", "1e5.0", "1e+INFx", "e+INF", "+e+INF",
        ] {
            eval_assert(ctx, &format!("(symbolp '{symbol})"));
        }
    }

    // A lone or leading underscore makes a symbol.
    #[test]
    fn underscore_symbols() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((_ 42)) _)", "42");
        eval_assert_equal(ctx, "(let ((_x 7)) _x)", "7");
    }

    // A number or symbol ends at a parenthesis, a quote, a comma, a string, a
    // `#` or a comment, as in Emacs.
    #[test]
    fn a_token_ends_at_a_delimiter() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "'(1;c\n)", "'(1)");
        eval_assert_equal(ctx, "'(1'a)", "'(1 'a)");
        eval_assert_equal(ctx, "'(a(b))", "'(a (b))");
        eval_assert_equal(ctx, r#"'(a"x")"#, r#"'(a "x")"#);
        eval_assert_equal(ctx, "'(a#'b)", "'(a #'b)");
        eval_assert_equal(ctx, "'(a#x10)", "'(a 16)");
        eval_assert_equal(ctx, "(length '(1`a))", "2");
        eval_assert_equal(ctx, "`(1,(+ 1 1))", "'(1 2)");
    }

    #[test]
    fn a_float_can_start_with_a_dot() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, ".5", "0.5");
        eval_assert_equal(ctx, ".25", "0.25");
        eval_assert_equal(ctx, "(+ .5 .25)", "0.75");
    }

    #[test]
    fn a_too_large_integer_is_an_error() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "99999999999999999999",
            r#"ERR ParsingError: SyntaxError number too large to fit in target type: 99999999999999999999
<eval_string>:1.1-1.20:  at nil
"#,
        );
    }
}

#[cfg(all(test, feature = "etags"))]
mod etags_tests {
    use crate::TulispContext;
    use std::io::Write;

    fn write_temp_file(name: &str, content: &str) -> (std::path::PathBuf, impl Drop + use<>) {
        let dir = std::env::temp_dir().join("tulisp_etags_test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join(name);
        let mut f = std::fs::File::create(&path).unwrap();
        write!(f, "{}", content).unwrap();
        struct Cleanup(std::path::PathBuf);
        impl Drop for Cleanup {
            fn drop(&mut self) {
                std::fs::remove_file(&self.0).ok();
            }
        }
        let cleanup = Cleanup(path.clone());
        (path, cleanup)
    }

    /// Assert that the tags output contains a tag entry line with the given
    /// function/macro name between the \x7f and \x01 delimiters.
    #[track_caller]
    fn assert_tag_entry(tags: &str, name: &str) {
        let pattern = format!("\x7f{}\x01", name);
        assert!(
            tags.contains(&pattern),
            "tags output should contain an entry for `{name}`, got: {tags}"
        );
    }

    #[test]
    fn test_etags_defun_tracking() -> Result<(), crate::Error> {
        let (path, _cleanup) =
            write_temp_file("defun_test.el", "(defun my-test-func (x) (+ x 1))\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-func");
        Ok(())
    }

    #[test]
    fn test_etags_defmacro_tracking() -> Result<(), crate::Error> {
        let (path, _cleanup) =
            write_temp_file("defmacro_test.el", "(defmacro my-test-macro (x) x)\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-macro");
        Ok(())
    }

    #[test]
    fn test_etags_multiple_definitions() -> Result<(), crate::Error> {
        let (path, _cleanup) = write_temp_file(
            "multi_test.el",
            "(defun func-a () 1)\n(defun func-b () 2)\n(defmacro macro-c (x) x)\n",
        );
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "func-a");
        assert_tag_entry(&tags, "func-b");
        assert_tag_entry(&tags, "macro-c");
        Ok(())
    }

    #[test]
    fn test_etags_defvar_tracking() -> Result<(), crate::Error> {
        let (path, _cleanup) = write_temp_file(
            "defvar_test.el",
            r#"(defvar my-test-var 42)
(defvar my-test-var-with-doc 7 "docs")
"#,
        );
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-var");
        assert_tag_entry(&tags, "my-test-var-with-doc");
        Ok(())
    }

    // A definition read from a string has no file to point at, so the table
    // leaves it out, and still has the rest.
    #[test]
    fn test_etags_skips_a_definition_from_a_string() -> Result<(), crate::Error> {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun from-a-string () 1)")?;
        let tags = ctx.tags_table(None)?;
        assert!(!tags.contains("<eval_string>"), "{tags}");
        let fresh = TulispContext::new().tags_table(None)?;
        let sorted = |text: &str| {
            let mut lines: Vec<String> = text.lines().map(str::to_string).collect();
            lines.sort();
            lines
        };
        assert_eq!(sorted(&tags), sorted(&fresh));
        Ok(())
    }

    // A tag whose line is past the end of its file, as for a Rust registration
    // whose source path names a shorter file from the working directory, is
    // left out.
    #[test]
    fn test_etags_skips_a_line_past_the_end_of_the_file() -> Result<(), crate::Error> {
        let (path, _cleanup) = write_temp_file("short_tags.rs", "fn main() {}\n");
        let path_str = path.to_str().unwrap().to_string();
        let mut ctx = TulispContext::new();
        ctx.tags_table
            .entry(path_str.clone())
            .or_default()
            .insert("far-away".to_string(), 50);
        assert!(!ctx.tags_table(None)?.contains("far-away"));
        Ok(())
    }

    #[test]
    fn test_etags_builtin_functions_tracked() -> Result<(), crate::Error> {
        let mut ctx = TulispContext::new();
        let tags = ctx.tags_table(None)?;
        assert!(
            !tags.is_empty(),
            "tags table should contain builtin entries"
        );
        // Verify at least some well-known builtins have proper tag entries.
        assert_tag_entry(&tags, "if");
        assert_tag_entry(&tags, "let");
        Ok(())
    }

    #[test]
    fn test_etags_output_contains_filename() -> Result<(), crate::Error> {
        let (path, _cleanup) =
            write_temp_file("filename_test.el", "(defun filename-test-fn () 42)\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert!(
            tags.contains(path_str),
            "tags output should reference the filename, got: {tags}"
        );
        assert_tag_entry(&tags, "filename-test-fn");
        Ok(())
    }

    #[test]
    fn test_etags_multiple_files() -> Result<(), crate::Error> {
        let (path1, _c1) = write_temp_file("file1.el", "(defun fn-from-file1 () 1)\n");
        let (path2, _c2) = write_temp_file("file2.el", "(defun fn-from-file2 () 2)\n");
        let mut ctx = TulispContext::new();
        let p1 = path1.to_str().unwrap();
        let p2 = path2.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[p1, p2]))?;
        assert_tag_entry(&tags, "fn-from-file1");
        assert_tag_entry(&tags, "fn-from-file2");
        assert!(tags.contains(p1), "should contain first file path");
        assert!(tags.contains(p2), "should contain second file path");
        Ok(())
    }

    #[test]
    fn test_etags_follow_load() -> Result<(), crate::Error> {
        let (path2, _c2) = write_temp_file("loaded.el", "(defun loaded-fn () 42)\n");
        let p2_str = path2.to_str().unwrap();
        let (path1, _c1) = write_temp_file(
            "loader.el",
            &format!("(defun loader-fn () 1)\n(load \"{}\")\n", p2_str),
        );
        let mut ctx = TulispContext::new();
        let p1_str = path1.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[p1_str]))?;
        assert_tag_entry(&tags, "loader-fn");
        assert_tag_entry(&tags, "loaded-fn");
        assert!(
            tags.contains(p2_str),
            "should contain the loaded file's path, got: {tags}"
        );
        Ok(())
    }

    #[test]
    fn test_etags_continue_past_a_failing_defvar() -> Result<(), crate::Error> {
        let (path, _cleanup) = write_temp_file(
            "failing_defvar.el",
            "(defvar fails-at-load (no-such-fn))\n(defun after-failing-defvar () 1)\n",
        );
        let mut ctx = TulispContext::new();
        let tags = ctx.tags_table(Some(&[path.to_str().unwrap()]))?;
        assert_tag_entry(&tags, "fails-at-load");
        assert_tag_entry(&tags, "after-failing-defvar");
        Ok(())
    }
}
