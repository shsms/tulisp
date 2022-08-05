use pest::{iterators::Pair, Parser};

use crate::context::{ContextObject, TulispContext};
use crate::eval::{eval, macroexpand};
use crate::{
    error::{Error, ErrorKind},
    value::{Span, TulispValue},
};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

pub fn parse_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);

    let mut list = TulispValue::Nil;
    for ele in p.unwrap() {
        let p = match parse(ctx, ele, &MacroExpand::Yes)? {
            p @ TulispValue::List { .. } => locate_all_func(ctx, p)?,
            p => p,
        };
        list.push(p.into_ref())?;
    }
    Ok(list)
}

fn locate_all_func(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    let mut ret = TulispValue::Nil;
    for ele in expr.base_iter() {
        let next = match ele.clone_inner() {
            e @ TulispValue::List { ctxobj: None, .. } => {
                let next = locate_all_func(ctx, e.to_owned())?;
                locate_func(ctx, next)?
            }
            e => e.clone(),
        };
        ret.push(next.into_ref())?;
    }
    ret.with_span(Span::from(&expr));
    Ok(ret)
}

fn locate_func(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    let name = match expr.clone().into_ref().car()?.clone_inner() {
        TulispValue::Symbol { value, .. } => value,
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
            let list = TulispValue::Nil.into_ref();
            let span: Span = value.as_span().into();
            value
                .into_inner()
                .map(|item| parse(ctx, item, expand_macros))
                .map(|val| -> Result<(), Error> { list.push(val?.into_ref()).map(|_| ()) })
                .fold(Ok(()), |v1, v2| v1.and(v2))?;
            let expr = list.with_span(Some(span));
            let name = match expr.car()?.clone_inner() {
                TulispValue::Symbol { value, .. } => value,
                _ => return Ok(expr.clone_inner()),
            };
            if name == "defun" || name == "defmacro" {
                eval(ctx, &expr)?;
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
                        "Backquote inner not found".to_string(),
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
                    Error::new(
                        ErrorKind::ParsingError,
                        "Unquote inner not found".to_string(),
                    )
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
                    Error::new(ErrorKind::ParsingError, "Quote inner not found".to_string())
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
                        "Sharpquote inner not found".to_string(),
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
                    Error::new(
                        ErrorKind::ParsingError,
                        "Splice inner not found".to_string(),
                    )
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
        Rule::ident => Ok(TulispValue::Symbol {
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
                        "parsing error: string inner not found".to_string(),
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
