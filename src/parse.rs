use std::{fmt::Write, iter::Peekable, str::Chars};

use crate::{
    eval::{eval, macroexpand},
    value::{Span, TulispValue},
    ContextObject, Error, ErrorKind, TulispContext, TulispValueRef,
};

struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    char_pos: usize,
    line_pos: usize,
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
    fn new(program: &str) -> Tokenizer<'_> {
        let chars = program.chars().peekable();
        Tokenizer {
            chars,
            char_pos: 0,
            line_pos: 1,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|x| x.to_owned())
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|ch| {
            self.char_pos += 1;
            ch
        })
    }

    fn read_string(&mut self) -> Option<Token> {
        assert_eq!(self.next_char()?, '"');
        let start_pos = self.char_pos;
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
                                    start: self.char_pos - 1,
                                    end: self.char_pos,
                                },
                            )))
                        }
                    };
                    output.write_char(out_ch).unwrap();
                }
                '"' => {
                    return Some(Token::String {
                        span: Span {
                            start: start_pos,
                            end: self.char_pos,
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
                start: start_pos,
                end: self.char_pos,
            },
        )))
    }

    fn read_num_ident(&mut self) -> Option<Token> {
        let start_pos = self.char_pos;
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
                span: Span::new(start_pos, self.char_pos),
                value: output.parse::<i64>().unwrap(),
            })
        } else if is_float {
            Some(Token::Float {
                span: Span::new(start_pos, self.char_pos),
                value: output.parse::<f64>().unwrap(),
            })
        } else {
            Some(Token::Ident {
                span: Span::new(start_pos, self.char_pos),
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
                    self.line_pos += 1;
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
                        span: Span::new(self.char_pos - 1, self.char_pos),
                    });
                }
                ')' => {
                    self.next_char()?;
                    return Some(Token::CloseParen {
                        span: Span::new(self.char_pos - 1, self.char_pos),
                    });
                }
                '\'' => {
                    self.next_char()?;
                    return Some(Token::Quote {
                        span: Span::new(self.char_pos - 1, self.char_pos),
                    });
                }
                '`' => {
                    self.next_char()?;
                    return Some(Token::Backtick {
                        span: Span::new(self.char_pos - 1, self.char_pos),
                    });
                }
                '.' => {
                    self.next_char()?;
                    return Some(Token::Dot {
                        span: Span::new(self.char_pos - 1, self.char_pos),
                    });
                }
                '#' => {
                    self.next_char()?;
                    if self.peek_char()? == '\'' {
                        self.next_char()?;
                        return Some(Token::SharpQuote {
                            span: Span::new(self.char_pos - 2, self.char_pos),
                        });
                    }
                    return Some(Token::ParserError(ParserError::syntax_error(
                        "Unknown token #.  Did you mean #' ?".to_string(),
                        Span::new(self.char_pos - 1, self.char_pos),
                    )));
                }
                ',' => {
                    self.next_char()?;
                    if self.peek_char()? == '@' {
                        self.next_char()?;
                        return Some(Token::Splice {
                            span: Span::new(self.char_pos - 2, self.char_pos),
                        });
                    }
                    return Some(Token::Comma {
                        span: Span::new(self.char_pos - 1, self.char_pos),
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

#[derive(Debug, PartialEq, PartialOrd)]
enum MacroExpand {
    Yes,
    No,
    Unquote,
}

struct Parser<'a, 'b> {
    tokenizer: Peekable<Tokenizer<'a>>,
    ctx: &'b mut TulispContext,
}

impl Parser<'_, '_> {
    fn new<'b, 'a>(ctx: &'b mut TulispContext, program: &'a str) -> Parser<'a, 'b> {
        Parser {
            tokenizer: Tokenizer::new(program).peekable(),
            ctx,
        }
    }

    fn parse_list(
        &mut self,
        start_span: Span,
        expand_macros: &MacroExpand,
    ) -> Result<TulispValueRef, Error> {
        let inner = TulispValue::Nil.into_ref();
        let mut got_dot = false;
        loop {
            let token = if let Some(token) = self.tokenizer.peek() {
                token
            } else {
                return Err(
                    Error::new(ErrorKind::ParsingError, "Unclosed list".to_string())
                        .with_span(Some(start_span)),
                );
            };
            match token {
                Token::CloseParen { span: end_span } => {
                    inner.with_span(Some(Span {
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
                    let next = self.parse_value(expand_macros)?.unwrap();
                    inner.push(next)?;
                }
            }
        }

        // consume a close paren or a dot.
        let _ = self.tokenizer.next();

        if got_dot {
            let next = self.parse_value(expand_macros)?.unwrap();
            if let Some(Token::CloseParen { span: end_span }) = self.tokenizer.next() {
                inner.with_span(Some(Span {
                    start: start_span.start,
                    end: end_span.end,
                }));
            } else {
                return Err(Error::new(
                    ErrorKind::ParsingError,
                    "Expected only one item in list after dot.".to_string(),
                )
                .with_span(next.span()));
            }
            inner.append(next)?;
        }

        if let Ok("defun" | "defmacro") = inner.car()?.as_symbol().as_ref().map(|x| x.as_str()) {
            eval(self.ctx, &inner)?;
        }
        if *expand_macros == MacroExpand::Yes {
            macroexpand(self.ctx, inner)
        } else {
            Ok(inner)
        }
    }

    fn parse_value(
        &mut self,
        expand_macros: &MacroExpand,
    ) -> Result<Option<TulispValueRef>, Error> {
        let token = if let Some(token) = self.tokenizer.next() {
            token
        } else {
            return Ok(None);
        };
        match token {
            Token::OpenParen { span } => self.parse_list(span, expand_macros).map(Some),
            Token::CloseParen { span } => Err(Error::new(
                ErrorKind::ParsingError,
                "Unexpected closing parenthesis".to_string(),
            )
            .with_span(Some(span))),
            Token::SharpQuote { span } | Token::Quote { span } => {
                let next = match self.parse_value(&MacroExpand::No)? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_span(Some(span)))
                    }
                };
                Ok(Some(
                    TulispValue::Quote {
                        value: next,
                        span: Some(span),
                    }
                    .into_ref(),
                ))
            }
            Token::Backtick { span } => {
                let next = match self.parse_value(if *expand_macros != MacroExpand::No {
                    &MacroExpand::Unquote
                } else {
                    expand_macros
                })? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_span(Some(span)))
                    }
                };
                Ok(Some(
                    TulispValue::Backquote {
                        value: next,
                        span: Some(span),
                    }
                    .into_ref(),
                ))
            }
            Token::Dot { span } => Err(Error::new(
                ErrorKind::ParsingError,
                "Unexpected dot".to_string(),
            )
            .with_span(Some(span))),
            Token::Comma { span } => {
                let next = match self.parse_value(if *expand_macros != MacroExpand::No {
                    &MacroExpand::Yes
                } else {
                    expand_macros
                })? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_span(Some(span)))
                    }
                };
                Ok(Some(
                    TulispValue::Unquote {
                        value: next,
                        span: Some(span),
                    }
                    .into_ref(),
                ))
            }
            Token::Splice { span } => {
                let next = match self.parse_value(if *expand_macros != MacroExpand::No {
                    &MacroExpand::Yes
                } else {
                    expand_macros
                })? {
                    Some(next) => next,
                    None => {
                        return Err(Error::new(
                            ErrorKind::ParsingError,
                            "Unexpected EOF".to_string(),
                        )
                        .with_span(Some(span)))
                    }
                };
                Ok(Some(
                    TulispValue::Splice {
                        value: next,
                        span: Some(span),
                    }
                    .into_ref(),
                ))
            }
            Token::String { span, value } => Ok(Some(
                TulispValue::String {
                    value,
                    span: Some(span),
                }
                .into_ref(),
            )),

            Token::Integer { span, value } => Ok(Some(
                TulispValue::Int {
                    value,
                    span: Some(span),
                }
                .into_ref(),
            )),
            Token::Float { span, value } => Ok(Some(
                TulispValue::Float {
                    value,
                    span: Some(span),
                }
                .into_ref(),
            )),
            Token::Ident { span, value } => Ok(Some(
                if value == "nil" {
                    TulispValue::Nil
                } else {
                    TulispValue::Symbol {
                        value,
                        span: Some(span),
                    }
                }
                .into_ref(),
            )),
            Token::ParserError(err) => Err(Error::new(
                ErrorKind::ParsingError,
                format!("{:?} {}", err.kind, err.desc),
            )
            .with_span(Some(err.span))),
        }
    }

    fn parse(&mut self) -> Result<TulispValueRef, Error> {
        let output = TulispValue::Nil.into_ref();
        while let Some(next) = self.parse_value(&MacroExpand::Yes)? {
            let next = if next.is_cons() {
                locate_all_func(self.ctx, next)?
            } else {
                next
            };
            output.push(next)?;
        }
        Ok(output)
    }
}

fn locate_all_func(ctx: &mut TulispContext, expr: TulispValueRef) -> Result<TulispValueRef, Error> {
    let ret = TulispValue::Nil.into_ref();
    for ele in expr.base_iter() {
        let next = if ele.is_cons() && ele.ctxobj().is_none() {
            let next = locate_all_func(ctx, ele)?;
            locate_func(ctx, next)?
        } else {
            ele
        };
        ret.push(next)?;
    }
    ret.with_span(expr.span());
    Ok(ret)
}

fn locate_func(ctx: &mut TulispContext, expr: TulispValueRef) -> Result<TulispValueRef, Error> {
    let name = match expr.clone().car()?.clone_inner() {
        TulispValue::Symbol { value, .. } => value,
        _ => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(_) | ContextObject::Defun { .. } => {
                if expr.is_cons() && expr.ctxobj().is_none() {
                    Ok(expr.with_ctxobj(Some(item.clone())))
                } else {
                    Ok(expr)
                }
            }
            _ => Ok(expr),
        },
        None => Ok(expr),
    }
}

pub fn parse(ctx: &mut TulispContext, program: &str) -> Result<TulispValueRef, Error> {
    Parser::new(ctx, program).parse()
}