use pest;

use pest::{iterators::Pair, Parser};

use crate::cons::{car, cdr};
use crate::context::{ContextObject, TulispContext};
use crate::eval::{eval, eval_defmacro};
use crate::{
    cons::Cons,
    error::{Error, ErrorKind},
    value::{Span, TulispValue},
    value_ref::TulispValueRef,
};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

pub fn parse_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);

    let mut list = Cons::new();
    for ele in p.unwrap() {
        let p = match parse(ctx, ele, &MacroExpand::Yes)? {
            p @ TulispValue::List { .. } => locate_all_func(ctx, p)?,
            p => p,
        };
        list.push(p.into_ref())?;
    }
    Ok(TulispValue::List {
        cons: list,
        ctxobj: None,
        span: None,
    })
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispValueRef) -> Result<TulispValueRef, Error> {
    if !inp.is_cons() {
        return Ok(inp);
    }
    let mut expr = TulispValue::List {
        cons: Cons::new(),
        ctxobj: inp.ctxobj(),
        span: inp.span(),
    };
    for item in inp.iter() {
        let item = macroexpand(ctx, item)?;
        // TODO: switch to tailcall method to propagate inner spans.
        expr.push(item.clone())?;
    }
    let expr = expr.into_ref();
    let name = match car(expr.clone())?.as_ident() {
        Ok(id) => id,
        Err(_) => return Ok(expr.clone()),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Macro(func) => {
                let expansion = func(ctx, expr)?;
                macroexpand(ctx, expansion)
            }
            ContextObject::Defmacro { args, body } => {
                let expansion = eval_defmacro(ctx, args.clone(), body.clone(), cdr(expr)?)
                    .map_err(|e| e.with_span(inp.span()))?;
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
        let next = match ele.clone_inner() {
            e @ TulispValue::List { ctxobj: None, .. } => {
                let next = locate_all_func(ctx, e.to_owned())?;
                locate_func(ctx, next)?
            }
            e => e.clone(),
        };
        ret.push(next.into_ref())?;
    }
    Ok(TulispValue::List {
        cons: ret,
        ctxobj: None,
        span: Span::from(&expr),
    })
}

fn locate_func(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    let name = match car(expr.clone().into_ref())?.clone_inner() {
        TulispValue::Ident { value, .. } => value.to_string(),
        _ => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(_) | ContextObject::Defun { .. } => match expr {
                TulispValue::List {
                    cons: body,
                    ctxobj: None,
                    span,
                } => Ok(TulispValue::List {
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
                .map(|val| -> Result<(), Error> { list.push(val?.into_ref()).map(|_| ()) })
                .fold(Ok(()), |v1, v2| v1.and(v2))?;
            let expr = TulispValue::List {
                cons: list,
                ctxobj: None,
                span: Some(span),
            }
            .into_ref();
            let name = match car(expr.clone())?.clone_inner() {
                TulispValue::Ident { value, .. } => value.to_string(),
                _ => return Ok(expr.clone_inner()),
            };
            if name == "defun" || name == "defmacro" {
                eval(ctx, expr.clone())?;
            }
            if *expand_macros == MacroExpand::Yes {
                Ok(macroexpand(ctx, expr)?.clone_inner())
            } else {
                Ok(expr.clone_inner())
            }
        }
        Rule::cons => {
            let mut list = TulispValue::Nil;
            value
                .into_inner()
                .map(|item| parse(ctx, item, expand_macros))
                .map(|val| -> Result<(), Error> {
                    list.append(val?.into_ref())?;
                    Ok(())
                })
                .fold(Ok(()), |v1, v2| v1.and(v2))?;
            Ok(list)
        }
        Rule::backquote => Ok(TulispValue::Backquote {
            span: Some(value.as_span().into()),
            value: parse(
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
            )?
            .into_ref(),
        }),
        Rule::unquote => Ok(TulispValue::Unquote {
            span: Some(value.as_span().into()),
            value: parse(
                ctx,
                value.into_inner().peek().ok_or_else(|| {
                    Error::new(ErrorKind::ParsingError, format!("Unquote inner not found"))
                })?,
                if *expand_macros != MacroExpand::No {
                    &MacroExpand::Yes
                } else {
                    expand_macros
                },
            )?
            .into_ref(),
        }),
        Rule::quote => Ok(TulispValue::Quote {
            span: Some(value.as_span().into()),
            value: parse(
                ctx,
                value.into_inner().peek().ok_or_else(|| {
                    Error::new(ErrorKind::ParsingError, format!("Quote inner not found"))
                })?,
                &MacroExpand::No,
            )?
            .into_ref(),
        }),
        Rule::sharpquote => Ok(TulispValue::Sharpquote {
            span: Some(value.as_span().into()),
            value: parse(
                ctx,
                value.into_inner().peek().ok_or_else(|| {
                    Error::new(
                        ErrorKind::ParsingError,
                        format!("Sharpquote inner not found"),
                    )
                })?,
                &MacroExpand::No,
            )?
            .into_ref(),
        }),
        Rule::splice => Ok(TulispValue::Splice {
            span: Some(value.as_span().into()),
            value: parse(
                ctx,
                value.into_inner().peek().ok_or_else(|| {
                    Error::new(ErrorKind::ParsingError, format!("Splice inner not found"))
                })?,
                if *expand_macros != MacroExpand::No {
                    &MacroExpand::Yes
                } else {
                    expand_macros
                },
            )?
            .into_ref(),
        }),
        Rule::nil => Ok(TulispValue::Nil),
        Rule::ident => Ok(TulispValue::Ident {
            value: value.as_span().as_str().to_string(),
            span: Some(value.as_span().into()),
        }),
        Rule::integer => Ok(TulispValue::Int {
            value: value
                .as_span()
                .as_str()
                .parse()
                .map_err(|e: std::num::ParseIntError| {
                    Error::new(ErrorKind::ParsingError, e.to_string())
                })?,
            span: Some(value.as_span().into()),
        }),
        Rule::float => {
            Ok(TulispValue::Float {
                value: value.as_span().as_str().parse().map_err(
                    |e: std::num::ParseFloatError| {
                        Error::new(ErrorKind::ParsingError, e.to_string())
                    },
                )?,
                span: Some(value.as_span().into()),
            })
        }
        Rule::string => Ok(TulispValue::String {
            span: Some(value.as_span().into()),
            value: value
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
        }),
        e => Err(Error::new(
            ErrorKind::ParsingError,
            format!("unknown rule {:?}", e),
        )),
    }
}
