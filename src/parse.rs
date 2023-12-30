use std::{collections::HashMap, fmt::Write, iter::Peekable, str::Chars};

use crate::{
    destruct_bind,
    eval::{eval, macroexpand},
    list,
    object::Span,
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
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
            pos: 1,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|x| x.to_owned())
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|ch| {
            if ch == '\n' {
                self.line += 1;
                self.pos = 1;
            } else {
                self.pos += 1;
            }
            ch
        })
    }

    fn read_string(&mut self) -> Option<Token> {
        assert_eq!(self.next_char()?, '"');
        let start_pos = (self.line, self.pos);
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
                                    start: (self.line, self.pos - 1),
                                    end: (self.line, self.pos),
                                },
                            )))
                        }
                    };
                    output.write_char(out_ch).unwrap();
                }
                '"' => {
                    return Some(Token::String {
                        span: Span {
                            file_id: self.file_id,
                            start: start_pos,
                            end: (self.line, self.pos),
                        },
                        value: output,
                    })
                }
                ch => output.write_char(ch).unwrap(),
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
        let start_pos = (self.line, self.pos);
        let mut output = String::new();
        let mut first_char = true;
        let mut is_int = true;
        let mut is_float = false;

        while let Some(ch) = self.peek_char() {
            match ch {
                ')' | ' ' | '\t' | '\n' | '\r' => {
                    break;
                }
                '-' => {
                    if !first_char {
                        is_int = false;
                        is_float = false;
                    }
                    output.write_char(ch).unwrap();
                }
                '0'..='9' => output.write_char(ch).unwrap(),
                '.' => {
                    if is_int && !is_float {
                        is_int = false;
                        is_float = true;
                    } else if is_float {
                        is_float = false;
                    }
                    output.write_char(ch).unwrap()
                }
                ch => {
                    is_int = false;
                    is_float = false;
                    output.write_char(ch).unwrap();
                }
            }
            self.next_char()?;
            first_char = false;
        }
        if is_int && output != "-" {
            Some(Token::Integer {
                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                value: output.parse::<i64>().unwrap(),
            })
        } else if is_float {
            Some(Token::Float {
                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                value: output.parse::<f64>().unwrap(),
            })
        } else {
            Some(Token::Ident {
                span: Span::new(self.file_id, start_pos, (self.line, self.pos)),
                value: output,
            })
        }
    }
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
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
                }
                ')' => {
                    self.next_char()?;
                    return Some(Token::CloseParen {
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
                }
                '\'' => {
                    self.next_char()?;
                    return Some(Token::Quote {
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
                }
                '`' => {
                    self.next_char()?;
                    return Some(Token::Backtick {
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
                }
                '.' => {
                    self.next_char()?;
                    return Some(Token::Dot {
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
                }
                '#' => {
                    self.next_char()?;
                    if self.peek_char()? == '\'' {
                        self.next_char()?;
                        return Some(Token::SharpQuote {
                            span: Span::new(
                                self.file_id,
                                (self.line, self.pos - 2),
                                (self.line, self.pos),
                            ),
                        });
                    }
                    return Some(Token::ParserError(ParserError::syntax_error(
                        "Unknown token #.  Did you mean #' ?".to_string(),
                        Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    )));
                }
                ',' => {
                    self.next_char()?;
                    if self.peek_char()? == '@' {
                        self.next_char()?;
                        return Some(Token::Splice {
                            span: Span::new(
                                self.file_id,
                                (self.line, self.pos - 2),
                                (self.line, self.pos),
                            ),
                        });
                    }
                    return Some(Token::Comma {
                        span: Span::new(
                            self.file_id,
                            (self.line, self.pos - 1),
                            (self.line, self.pos),
                        ),
                    });
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
    strings: HashMap<String, TulispObject>,
}

fn recursive_update_ctxobj(ctx: &mut TulispContext, body: &TulispObject) -> Result<(), Error> {
    if !body.consp() {
        return Ok(());
    }
    let name = body.car()?;
    if name.symbolp() && body.ctxobj().is_none() {
        let ctxobj = eval(ctx, &name).ok();
        body.with_ctxobj(ctxobj);
    }
    for item in body.base_iter() {
        recursive_update_ctxobj(ctx, &item)?;
    }
    Ok(())
}

impl Parser<'_, '_> {
    fn new<'b, 'a>(ctx: &'b mut TulispContext, file_id: usize, program: &'a str) -> Parser<'a, 'b> {
        Parser {
            file_id,
            tokenizer: Tokenizer::new(file_id, program).peekable(),
            ctx,
            ints: Default::default(),
            strings: Default::default(),
        }
    }

    fn parse_list(&mut self, start_span: Span) -> Result<TulispObject, Error> {
        let mut inner = TulispObject::nil();
        let mut got_dot = false;
        loop {
            let Some(token) = self.tokenizer.peek() else {
                return Err(
                    Error::new(ErrorKind::ParsingError, "Unclosed list".to_string())
                        .with_trace(TulispObject::nil().with_span(Some(start_span))),
                );
            };
            match token {
                Token::CloseParen { span: end_span } => {
                    inner.with_span(Some(Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: end_span.end,
                    }));
                    break;
                }
                Token::Dot { .. } => {
                    got_dot = true;
                    break;
                }
                _ => {
                    let next = self.parse_value()?.unwrap();
                    inner.push(next)?;
                }
            }
        }

        // consume a close paren or a dot.
        let _ = self.tokenizer.next();

        if got_dot {
            let next = self.parse_value()?.unwrap();
            if let Some(Token::CloseParen { span: end_span }) = self.tokenizer.next() {
                inner.with_span(Some(Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end: end_span.end,
                }));
            } else {
                return Err(Error::new(
                    ErrorKind::ParsingError,
                    "Expected only one item in list after dot.".to_string(),
                )
                .with_trace(next));
            }
            inner.append(next)?;
        }

        if let Ok("defun" | "defmacro" | "lambda") =
            inner.car()?.as_symbol().as_ref().map(|x| x.as_str())
        {
            inner = macroexpand(self.ctx, inner)?;
            eval(self.ctx, &inner)?;
            // recursively update ctx obj in case it is a recursive function.
            recursive_update_ctxobj(self.ctx, &inner)?;
        }
        if inner.consp() {
            let name = inner.car()?;
            let ctxobj = eval(self.ctx, &name).ok();
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
            Token::CloseParen { span } => Err(Error::new(
                ErrorKind::ParsingError,
                "Unexpected closing parenthesis".to_string(),
            )
            .with_trace(TulispValue::Nil.into_ref(Some(span)))),
            Token::SharpQuote { span } | Token::Quote { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_trace(TulispValue::Nil.into_ref(Some(span))))
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
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_trace(TulispValue::Nil.into_ref(Some(span))))
                    }
                };
                Ok(Some(
                    TulispValue::Backquote { value: next }.into_ref(Some(span)),
                ))
            }
            Token::Dot { span } => Err(Error::new(
                ErrorKind::ParsingError,
                "Unexpected dot".to_string(),
            )
            .with_trace(TulispValue::Nil.into_ref(Some(span)))),
            Token::Comma { span } => {
                let next = match self.parse_value()? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_trace(TulispValue::Nil.into_ref(Some(span))))
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
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_trace(TulispValue::Nil.into_ref(Some(span))))
                    }
                };
                Ok(Some(
                    TulispValue::Splice { value: next }.into_ref(Some(span)),
                ))
            }
            Token::String { span, value } => Ok(Some(match self.strings.get(&value) {
                Some(vv) => vv.with_span(Some(span)),
                None => {
                    let vv = TulispValue::String {
                        value: value.clone(),
                    }
                    .into_ref(Some(span));
                    self.strings.insert(value, vv.clone());
                    vv
                }
            })),

            Token::Integer { span, value } => Ok(Some(match self.ints.get(&value) {
                Some(vv) => vv.with_span(Some(span)),
                None => {
                    let vv = TulispValue::Int { value }.into_ref(Some(span));
                    self.ints.insert(value, vv.clone());
                    vv
                }
            })),
            Token::Float { span, value } => {
                Ok(Some(TulispValue::Float { value }.into_ref(Some(span))))
            }
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
            Token::ParserError(err) => Err(Error::new(
                ErrorKind::ParsingError,
                format!("{:?} {}", err.kind, err.desc),
            )
            .with_trace(TulispValue::Nil.into_ref(Some(err.span)))),
        }
    }

    fn parse(&mut self) -> Result<TulispObject, Error> {
        let output = TulispObject::nil();
        while let Some(next) = self.parse_value()? {
            output.push(next)?;
        }
        macroexpand(self.ctx, output)
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
    let ret = TulispObject::nil();
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap();
    for next in body_iter {
        ret.push(tail)?;
        tail = next;
    }
    if !tail.consp() {
        return Ok(body);
    }
    let span = tail.span();
    let ctxobj = tail.ctxobj();
    let tail_ident = tail.car()?;
    let tail_name_str = tail_ident.as_symbol()?;
    let new_tail = if tail_ident.eq(&name) {
        let ret_tail = TulispObject::nil().append(tail.cdr()?)?.to_owned();
        list!(,ctx.intern("list")
              ,TulispValue::Bounce.into_ref(None)
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
    ret.push(new_tail.with_ctxobj(ctxobj).with_span(span))?;
    Ok(ret)
}

pub fn parse(
    ctx: &mut TulispContext,
    file_id: usize,
    program: &str,
) -> Result<TulispObject, Error> {
    Parser::new(ctx, file_id, program).parse()
}
