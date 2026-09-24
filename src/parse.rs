use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{
    Error, Number, TulispContext, TulispObject, TulispValue, destruct_bind, list, object::Span,
};

struct Tokenizer<'a> {
    file_id: usize,
    chars: Peekable<Chars<'a>>,
    line: usize,
    pos: usize,
}

#[derive(PartialEq, Debug)]
enum ParserErrorKind {
    SyntaxError,
}

#[allow(unused)]
#[derive(Debug)]
struct ParserError {
    kind: ParserErrorKind,
    desc: String,
    span: Span,
}

impl ParserError {
    fn new(kind: ParserErrorKind, desc: String, span: Span) -> Self {
        ParserError { kind, desc, span }
    }

    fn syntax_error(desc: String, span: Span) -> Self {
        Self::new(ParserErrorKind::SyntaxError, desc, span)
    }
}

#[derive(Debug)]
enum Token {
    OpenParen { span: Span },
    CloseParen { span: Span },
    Quote { span: Span },
    Backtick { span: Span },
    Dot { span: Span },
    Comma { span: Span },
    Splice { span: Span },     // ,@
    SharpQuote { span: Span }, // #'
    String { span: Span, value: String },
    Integer { span: Span, value: i64 },
    Float { span: Span, value: f64 },
    Ident { span: Span, value: String },

    ParserError(ParserError),
}

impl Tokenizer<'_> {
    fn new(file_id: usize, program: &str) -> Tokenizer<'_> {
        let chars = program.chars().peekable();
        Tokenizer {
            file_id,
            chars,
            line: 1,
            pos: 0,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|x| x.to_owned())
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().inspect(|ch| {
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
        let start_pos = (self.line, self.pos + 1);
        let mut output = String::new();
        while let Some(ch) = self.next_char() {
            match ch {
                '\\' => {
                    // Common control-char escapes match Emacs / C.
                    // Hex / octal / Unicode (`\xHH`, `\NNN`,
                    // `\u{HHHH}`) aren't supported yet — the reader
                    // errors on unknown escapes rather than passing
                    // them through, partly to flag typos and partly
                    // because Display only round-trips the four it
                    // emits (`\"`, `\\`, `\n`, `\t`).
                    let out_ch = match self.next_char()? {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        'b' => '\u{08}', // backspace
                        'f' => '\u{0c}', // form feed
                        'v' => '\u{0b}', // vertical tab
                        'a' => '\u{07}', // alarm / bell
                        'e' => '\u{1b}', // escape
                        '0' => '\u{00}', // null
                        '\\' => '\\',
                        '"' => '"',
                        e => {
                            return Some(Token::ParserError(ParserError::new(
                                ParserErrorKind::SyntaxError,
                                format!("Unknown escape char {}", e),
                                Span {
                                    file_id: self.file_id,
                                    start: (self.line, self.pos),
                                    end: (self.line, self.pos),
                                },
                            )));
                        }
                    };
                    output.push(out_ch);
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
        )))
    }

    fn read_num_ident(&mut self) -> Option<Token> {
        let start_pos = (self.line, self.pos + 1);
        self.read_num_ident_impl(start_pos, String::new(), true, false)
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
            )));
        }
        match i64::from_str_radix(&digits, radix) {
            Ok(value) => Some(Token::Integer { span, value }),
            Err(e) => Some(Token::ParserError(ParserError::syntax_error(
                format!("{prefix}{digits}: {e}"),
                span,
            ))),
        }
    }

    /// Read a `?X` character literal. Returns the character's code
    /// point as an `Integer` token. Supports the same backslash
    /// escapes as string literals (`?\n`, `?\t`, `?\\`, `?\"`,
    /// `?\r`, `?\b`, `?\f`, `?\v`, `?\a`, `?\e`, `?\0`); plus
    /// `?\'` which is convenient for `?\'`-style apostrophe.
    fn read_char_literal(&mut self) -> Option<Token> {
        let start_pos = (self.line, self.pos + 1);
        self.next_char()?; // consume '?'
        let span_for = |toklen: &Tokenizer<'_>| -> Span {
            Span::new(toklen.file_id, start_pos, (toklen.line, toklen.pos))
        };
        let value: i64 = match self.next_char() {
            Some('\\') => match self.next_char() {
                Some('n') => '\n' as i64,
                Some('t') => '\t' as i64,
                Some('r') => '\r' as i64,
                Some('b') => 0x08,
                Some('f') => 0x0c,
                Some('v') => 0x0b,
                Some('a') => 0x07,
                Some('e') => 0x1b,
                Some('0') => 0x00,
                Some('\\') => '\\' as i64,
                Some('\'') => '\'' as i64,
                Some('"') => '"' as i64,
                // Emacs reads `?\j` as just `j` for unknown escapes;
                // keep that — easier to remove later than to add.
                Some(c) => c as i64,
                None => {
                    return Some(Token::ParserError(ParserError::syntax_error(
                        "Unexpected EOF after ?\\".to_string(),
                        span_for(self),
                    )));
                }
            },
            Some(c) => c as i64,
            None => {
                return Some(Token::ParserError(ParserError::syntax_error(
                    "Unexpected EOF after ?".to_string(),
                    span_for(self),
                )));
            }
        };
        Some(Token::Integer {
            span: span_for(self),
            value,
        })
    }

    fn read_num_ident_impl(
        &mut self,
        start_pos: (usize, usize),
        mut output: String,
        mut is_int: bool,
        mut is_float: bool,
    ) -> Option<Token> {
        let mut first_char = output.is_empty();
        // Scientific-notation state. `seen_e` blocks a second `e`/`E`,
        // `expect_exp_sign` lets one `+`/`-` follow `e`/`E` without
        // tipping the token into ident mode.
        let mut seen_e = false;
        let mut expect_exp_sign = false;

        while let Some(ch) = self.peek_char() {
            match ch {
                ')' | '[' | ']' | ' ' | '\t' | '\n' | '\r' => {
                    break;
                }
                'e' | 'E' if (is_int || is_float) && !first_char && !seen_e => {
                    // Enter exponent mode: any preceding digits/dot
                    // make this a float, regardless of `is_int`.
                    is_int = false;
                    is_float = true;
                    seen_e = true;
                    expect_exp_sign = true;
                    output.push(ch);
                }
                '-' | '+' if expect_exp_sign && (is_int || is_float) => {
                    // Sign of the exponent — stays in float mode.
                    expect_exp_sign = false;
                    output.push(ch);
                }
                '-' => {
                    if !first_char {
                        is_int = false;
                        is_float = false;
                    }
                    output.push(ch);
                }
                '0'..='9' => {
                    expect_exp_sign = false;
                    output.push(ch);
                }
                '_' if (is_int || is_float) && !first_char => {}
                '.' => {
                    if is_int && !is_float {
                        is_int = false;
                        is_float = true;
                    } else if is_float {
                        is_float = false;
                    }
                    output.push(ch)
                }
                ch => {
                    is_int = false;
                    is_float = false;
                    output.push(ch);
                }
            }
            self.next_char()?;
            first_char = false;
        }
        if is_int && output != "-" {
            let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
            match output.parse::<i64>() {
                Ok(value) => Some(Token::Integer { span, value }),
                Err(e) => Some(Token::ParserError(ParserError::syntax_error(
                    format!("{e}: {output}"),
                    span,
                ))),
            }
        } else if is_float {
            let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
            match output.parse::<f64>() {
                Ok(value) => Some(Token::Float { span, value }),
                // `1e` / `1e+` (and similar) eagerly entered exponent
                // mode but never produced an exponent digit. Emacs
                // reads these as identifiers — fall back rather than
                // erroring on a syntactically valid Lisp symbol.
                Err(_) if seen_e => Some(Token::Ident {
                    span,
                    value: output,
                }),
                Err(e) => Some(Token::ParserError(ParserError::syntax_error(
                    format!("{e}: {output}"),
                    span,
                ))),
            }
        } else {
            let span = Span::new(self.file_id, start_pos, (self.line, self.pos));
            // Emacs' `1.0e+INF` / `-1.0e+INF` / `0.0e+NaN` /
            // `-0.0e+NaN` shapes — uppercase suffix only, only `e+`
            // (not `e-`). Mantissa value is ignored; only its sign
            // matters. Lowercase / `e-` variants stay as identifiers,
            // matching Emacs' reader.
            if let Some(value) = parse_emacs_inf_nan(&output) {
                return Some(Token::Float { span, value });
            }
            Some(Token::Ident {
                span,
                value: output,
            })
        }
    }
}

/// Recognize the `<mantissa>e+INF` / `<mantissa>e+NaN` shapes that
/// Emacs' reader uses for the special float values. The mantissa
/// value is irrelevant (`5.5e+INF` and `1.0e+INF` both produce
/// `+INF`); only its sign carries through. Returns `None` for
/// anything else (so the caller can fall back to identifier).
fn parse_emacs_inf_nan(s: &str) -> Option<f64> {
    for (suffix, base) in [("e+INF", f64::INFINITY), ("e+NaN", f64::NAN)] {
        let Some(prefix) = s.strip_suffix(suffix) else {
            continue;
        };
        // Mantissa must itself be a finite f64 (rules out empty,
        // double-dot, leading-letter, etc.). The mantissa's value is
        // discarded — only its sign matters.
        if let Ok(mantissa) = prefix.parse::<f64>()
            && mantissa.is_finite()
        {
            return Some(if prefix.starts_with('-') {
                // `-f64::NAN` flips the sign bit; `-f64::INFINITY`
                // produces `f64::NEG_INFINITY`.
                -base
            } else {
                base
            });
        }
    }
    None
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let ch = self.peek_char()?;

            match ch {
                '\n' => {
                    self.next_char()?;
                    continue;
                }
                ' ' | '\r' | '\t' => {
                    self.next_char()?;
                    continue;
                }
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
                        return self.read_num_ident_impl(start_pos, String::from("."), false, true);
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
                            )));
                        }
                        None => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unexpected EOF after #".to_string(),
                                Span::new(self.file_id, start_pos, (self.line, self.pos)),
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
                            )));
                        }
                    }
                }
                '"' => {
                    return self.read_string();
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
            Token::SharpQuote { span } | Token::Quote { span } => {
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

pub(crate) fn mark_tail_calls(
    ctx: &mut TulispContext,
    name: TulispObject,
    body: TulispObject,
) -> Result<TulispObject, Error> {
    if !body.consp() {
        return Ok(body);
    }
    let mut builder = crate::cons::ListBuilder::new();
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap();
    for next in body_iter {
        builder.push(tail);
        tail = next;
    }
    if !tail.consp() {
        return Ok(body);
    }
    let span = tail.span();
    let tail_ident = tail.car()?;
    let tail_name_str = tail_ident.as_symbol()?;
    let is_self_call = tail_ident.eq(&name);
    // A call to another VM-compiled defun in tail position is also
    // a TCO opportunity. The call site uses the same `Bounce` shape
    // as self-recursion; `compile_fn_defun_bounce_call` emits a
    // `TailCall` for it (loop-style unwind in the caller), which
    // gives mutual recursion bounded Rust stack usage. We can only
    // mark when the target is already registered — forward
    // references (callee defined after caller) miss this and fall
    // through to a regular `Call`.
    let is_known_vm_defun = ctx
        .compiler
        .as_ref()
        .is_some_and(|c| c.defun_args.contains_key(&tail_ident.addr_as_usize()));
    let new_tail = if is_self_call || is_known_vm_defun {
        let ret_tail = TulispObject::nil().append(tail.cdr()?)?.to_owned();
        list!(,ctx.intern("list")
              ,TulispValue::Bounce.into_ref(None)
              ,tail_ident
              ,@ret_tail)?
    } else if tail_name_str == "progn" || tail_name_str == "let" || tail_name_str == "let*" {
        list!(,tail_ident ,@mark_tail_calls(ctx, name, tail.cdr()?)?)?
    } else if tail_name_str == "if" {
        destruct_bind!((_if condition then_body &rest else_body) = tail);
        list!(,tail_ident
            ,condition.clone()
            ,mark_tail_calls(
                ctx,
                name.clone(),
                list!(,then_body)?
            )?.car()?
            ,@mark_tail_calls(ctx, name, else_body)?
        )?
    } else if tail_name_str == "cond" {
        destruct_bind!((_cond &rest conds) = tail);
        let mut ret = list!(,tail_ident)?;
        for cond in conds.base_iter() {
            destruct_bind!((condition &rest body) = cond);
            ret = list!(,@ret
                ,list!(,condition.clone()
                    ,@mark_tail_calls(ctx, name.clone(), body)?)?)?;
        }
        ret
    } else {
        tail
    };
    builder.push(new_tail.with_span(span));
    Ok(builder.build())
}

pub fn parse(
    ctx: &mut TulispContext,
    file_id: usize,
    program: &str,
    #[cfg(feature = "etags")] follow_load_files: bool,
) -> Result<TulispObject, Error> {
    Parser::new(
        ctx,
        file_id,
        program,
        #[cfg(feature = "etags")]
        follow_load_files,
    )
    .parse()
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert_equal, eval_assert_equal_fresh, eval_assert_error};
    use crate::{Error, TulispContext};

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
