use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{
    Error, Number, TulispContext, TulispObject, TulispValue, destruct_bind, eval::macroexpand,
    list, object::Span,
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
                    let out_ch = match self.next_char()? {
                        'n' => '\n',
                        't' => '\t',
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
                ')' | ' ' | '\t' | '\n' | '\r' => {
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
                    self.next_char()?;
                    // `peek_char()?` would silently terminate the
                    // tokenizer at EOF, hiding the bad input. Match
                    // explicitly so we can surface a `ParserError`.
                    match self.peek_char() {
                        Some('\'') => {
                            self.next_char()?;
                            return Some(Token::SharpQuote {
                                span: Span::new(
                                    self.file_id,
                                    (self.line, self.pos - 1),
                                    (self.line, self.pos),
                                ),
                            });
                        }
                        Some(_) => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unknown token #.  Did you mean #' ?".to_string(),
                                Span::new(
                                    self.file_id,
                                    (self.line, self.pos),
                                    (self.line, self.pos),
                                ),
                            )));
                        }
                        None => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unexpected EOF after #".to_string(),
                                Span::new(
                                    self.file_id,
                                    (self.line, self.pos),
                                    (self.line, self.pos),
                                ),
                            )));
                        }
                    }
                }
                ',' => {
                    self.next_char()?;
                    match self.peek_char() {
                        Some('@') => {
                            self.next_char()?;
                            return Some(Token::Splice {
                                span: Span::new(
                                    self.file_id,
                                    (self.line, self.pos - 1),
                                    (self.line, self.pos),
                                ),
                            });
                        }
                        Some(_) => {
                            return Some(Token::Comma {
                                span: Span::new(
                                    self.file_id,
                                    (self.line, self.pos),
                                    (self.line, self.pos),
                                ),
                            });
                        }
                        None => {
                            return Some(Token::ParserError(ParserError::syntax_error(
                                "Unexpected EOF after ,".to_string(),
                                Span::new(
                                    self.file_id,
                                    (self.line, self.pos),
                                    (self.line, self.pos),
                                ),
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
    #[cfg(feature = "etags")]
    follow_load_files: bool,
}

fn recursive_update_ctxobj(ctx: &mut TulispContext, body: &TulispObject) -> Result<(), Error> {
    if !body.consp() {
        return Ok(());
    }
    let name = body.car()?;
    if name.symbolp() && body.ctxobj().is_none() {
        let ctxobj = ctx.eval(&name).ok();
        body.with_ctxobj(ctxobj);
    }
    for item in body.base_iter() {
        recursive_update_ctxobj(ctx, &item)?;
    }
    Ok(())
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
            let next = self.parse_value()?.unwrap();
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

        let mut inner = builder.build().with_span(full_span);

        #[cfg(feature = "etags")]
        if self.follow_load_files
            && let Ok("load") = inner.car()?.as_symbol().as_ref().map(|x| x.as_str())
        {
            let filename = inner.cadr()?.as_string()?;
            let contents = std::fs::read_to_string(&filename).map_err(|e| {
                Error::os_error(format!("Unable to read file: {filename}. Error: {e}"))
            })?;
            self.ctx.filenames.push(filename.to_string());
            // Parse the file to populate the tags table, but ignore the
            // result since we only care about the side effect of populating
            // the tags table.
            let _ = parse(
                self.ctx,
                self.ctx.filenames.len() - 1,
                contents.as_str(),
                self.follow_load_files,
            );
        }

        if let Ok("defun" | "defmacro" | "defvar") =
            inner.car()?.as_symbol().as_ref().map(|x| x.as_str())
        {
            #[cfg(feature = "etags")]
            {
                let name = inner.cadr()?.as_symbol()?.clone();
                if let Some(span) = inner.span() {
                    self.ctx
                        .tags_table
                        .entry(self.ctx.filenames[self.file_id].clone())
                        .or_default()
                        .insert(name, span.start.0);
                }
            }

            inner = macroexpand(self.ctx, inner)?;
            self.ctx.eval(&inner)?;
            // recursively update ctx obj in case it is a recursive function.
            recursive_update_ctxobj(self.ctx, &inner)?;
        }
        if inner.consp() {
            let name = inner.car()?;
            let ctxobj = self.ctx.eval(&name).ok();
            inner.with_ctxobj(ctxobj);
        }
        Ok(inner)
    }

    fn parse_value(&mut self) -> Result<Option<TulispObject>, Error> {
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
                    TulispValue::Unquote {
                        value: macroexpand(self.ctx, next)?,
                    }
                    .into_ref(Some(span)),
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
            Token::Ident { span, value } => Ok(Some(match self.ctx.intern_soft(&value) {
                Some(vv) => vv.with_span(Some(span)),
                None => {
                    if value == "t" {
                        TulispValue::T.into_ref(Some(span))
                    } else if value == "nil" {
                        TulispValue::Nil.into_ref(Some(span))
                    } else {
                        self.ctx.intern(&value).with_span(Some(span))
                    }
                }
            })),
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
        macroexpand(self.ctx, builder.build())
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
    let ctxobj = tail.ctxobj();
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
    let new_tail = if is_self_call
        || is_known_vm_defun
        || ctx
            .eval(&tail_ident)
            .is_ok_and(|f| matches!(&f.inner_ref().0, TulispValue::Lambda { .. }))
    {
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
    builder.push(new_tail.with_ctxobj(ctxobj).with_span(span));
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
