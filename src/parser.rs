use pest;

use pest::{iterators::Pair, Parser};

use crate::cons::{car, cdr};
use crate::context::{ContextObject, TulispContext};
use crate::eval::{eval, eval_defmacro};
use crate::value::Span;
use crate::{cons::Cons, value::TulispValue, Error, ErrorKind};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

pub fn parse_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);

    let mut list = Cons::new();
    for ele in p.unwrap() {
        let p = match parse(ctx, ele, &MacroExpand::Yes)? {
            p @ TulispValue::SExp{..} => locate_all_func(ctx, p)?,
            p => p,
        };
        list.push(p)?;
    }
    Ok(TulispValue::SExp {
        cons: Box::new(list),
        ctxobj: None,
        span: None,
    })
}

pub fn macroexpand(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    match &expr {
        TulispValue::SExp { .. } => {}
        _ => return Ok(expr),
    };
    let name = match car(&expr)? {
        TulispValue::Ident(ident) => ident.to_string(),
        _ => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Macro(func) => {
                let expansion = func(ctx, &expr)?;
                macroexpand(ctx, expansion)
            }
            ContextObject::Defmacro { args, body } => {
                let expansion = eval_defmacro(ctx, &args, &body, cdr(&expr)?)?;
                macroexpand(ctx, expansion)
            }
            _ => Ok(expr),
        },
        None => Ok(expr),
    }
}

fn locate_all_func(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    let mut ret = Cons::new();
    for ele in expr.iter() {
        let next = match ele.clone() {
            e @ TulispValue::SExp { ctxobj: None, .. } => {
                let next = locate_all_func(ctx, e)?;
                locate_func(ctx, next)?
            }
            e => e,
        };
        ret.push(next)?;
    }
    Ok(TulispValue::SExp {
        cons: Box::new(ret),
        ctxobj: None,
        span: Span::from(&expr),
    })
}

fn locate_func(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    let name = match car(&expr)? {
        TulispValue::Ident(ident) => ident.to_string(),
        _ => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(_) | ContextObject::Defun { .. } => match expr {
                TulispValue::SExp {
                    cons: body,
                    ctxobj: None,
                    span,
                } => Ok(TulispValue::SExp {
                    cons: body,
                    ctxobj: Some(item.clone()),
                    span,
                }),
                _ => Ok(expr),
            },
            _ => Ok(expr),
        },
        None => Ok(expr),
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum MacroExpand {
    Yes,
    No,
    Unquote,
}

fn parse(
    ctx: &mut TulispContext,
    value: Pair<'_, Rule>,
    expand_macros: &MacroExpand,
) -> Result<TulispValue, Error> {
    match value.as_rule() {
        Rule::form => {
            let mut list = Cons::new();
            let span: Span = value.as_span().into();
            value
                .into_inner()
                .map(|item| parse(ctx, item, expand_macros))
                .map(|val| -> Result<(), Error> { list.push(val?) })
                .fold(Ok(()), |v1, v2| v1.and(v2))?;
            let expr = TulispValue::SExp {
                cons: Box::new(list),
                ctxobj: None,
                span: Some(span),
            };
            let name = match car(&expr)? {
                TulispValue::Ident(ident) => ident.to_string(),
                _ => return Ok(expr),
            };
            if name == "defun" || name == "defmacro" {
                eval(ctx, &expr)?;
            }
            if *expand_macros == MacroExpand::Yes {
                Ok(macroexpand(ctx, expr)?)
            } else {
                Ok(expr)
            }
        }
        Rule::backquote => Ok(TulispValue::Backquote(Box::new(parse(
            ctx,
            value.into_inner().peek().ok_or_else(|| {
                Error::new(
                    ErrorKind::ParsingError,
                    format!("Backquote inner not found"),
                )
            })?,
            if *expand_macros != MacroExpand::No {
                &MacroExpand::Unquote
            } else {
                expand_macros
            },
        )?))),
        Rule::unquote => Ok(TulispValue::Unquote(Box::new(parse(
            ctx,
            value.into_inner().peek().ok_or_else(|| {
                Error::new(ErrorKind::ParsingError, format!("Unquote inner not found"))
            })?,
            if *expand_macros != MacroExpand::No {
                &MacroExpand::Yes
            } else {
                expand_macros
            },
        )?))),
        Rule::quote => Ok(TulispValue::Quote(Box::new(parse(
            ctx,
            value.into_inner().peek().ok_or_else(|| {
                Error::new(ErrorKind::ParsingError, format!("Quote inner not found"))
            })?,
            &MacroExpand::No,
        )?))),
        Rule::nil => Ok(TulispValue::Nil),
        Rule::ident => Ok(TulispValue::Ident(value.as_span().as_str().to_string())),
        Rule::integer => Ok(TulispValue::Int(value.as_span().as_str().parse().map_err(
            |e: std::num::ParseIntError| Error::new(ErrorKind::ParsingError, e.to_string()),
        )?)),
        Rule::float => Ok(TulispValue::Float(
            value
                .as_span()
                .as_str()
                .parse()
                .map_err(|e: std::num::ParseFloatError| {
                    Error::new(ErrorKind::ParsingError, e.to_string())
                })?,
        )),
        Rule::string => Ok(TulispValue::String(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| {
                    Error::new(
                        ErrorKind::ParsingError,
                        format!("parsing error: string inner not found"),
                    )
                })?
                .as_span()
                .as_str()
                .to_string(),
        )),
        e => Err(Error::new(
            ErrorKind::ParsingError,
            format!("unknown rule {:?}", e),
        )),
    }
}
