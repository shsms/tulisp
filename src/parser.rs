use pest;

use pest::{iterators::Pair, Parser};

use crate::cons::{car, cdr};
use crate::context::{ContextObject, TulispContext};
use crate::eval::eval_defmacro;
use crate::{cons::Cons, value::TulispValue, Error};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

pub fn parse_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);

    let mut list = Cons::new();
    for ele in p.unwrap() {
        list.push(parse(ctx, ele, &MacroExpand::Yes)?)?;
    }
    Ok(TulispValue::SExp(Box::new(list)))
}

pub fn macroexpand(ctx: &mut TulispContext, expr: TulispValue) -> Result<TulispValue, Error> {
    match &expr {
        TulispValue::SExp(_) => {},
        _ => return Ok(expr),
    };
    let name = match car(&expr)? {
        TulispValue::Ident(ident) => ident.to_string(),
        _ => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(ContextObject::Macro(func)) => func(ctx, cdr(&expr)?),
        Some(ContextObject::Defmacro{ args, body }) => {
            eval_defmacro(ctx, &args, &body, cdr(&expr)?)
        }
        _ => Ok(expr),
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum MacroExpand {
    Yes,
    No,
    Unquote,
}

fn parse(ctx: &mut TulispContext, value: Pair<'_, Rule>, expand_macros: &MacroExpand) -> Result<TulispValue, Error> {
    match value.as_rule() {
        Rule::form => {
            let mut list = Cons::new();
            value
                .into_inner()
                .map(|item|parse(ctx, item, expand_macros))
                .map(|val| -> Result<(), Error> {
                    list.push(val?)
                })
                .fold(Ok(()), |v1, v2| v1.and(v2))?;
            let ret = TulispValue::SExp(Box::new(list));
            if *expand_macros == MacroExpand::Yes {
                Ok(macroexpand(ctx, ret)?)
            } else {
                Ok(ret)
            }
        }
        Rule::backquote => Ok(TulispValue::Backquote(Box::new(parse(
            ctx,
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Backquote inner not found")))?,
            if *expand_macros != MacroExpand::No {
                &MacroExpand::Unquote
            } else {
                expand_macros
            }
        )?))),
        Rule::unquote => Ok(TulispValue::Unquote(Box::new(parse(
            ctx,
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Unquote inner not found")))?,
            if *expand_macros != MacroExpand::No {
                &MacroExpand::Yes
            } else {
                expand_macros
            }
        )?))),
        Rule::quote => Ok(TulispValue::Quote(Box::new(parse(
            ctx,
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Quote inner not found")))?,
            &MacroExpand::No
        )?))),
        Rule::nil => Ok(TulispValue::Nil),
        Rule::ident => Ok(TulispValue::Ident(value.as_span().as_str().to_string())),
        Rule::integer => {
            Ok(TulispValue::Int(value.as_span().as_str().parse().map_err(
                |e: std::num::ParseIntError| Error::ParsingError(e.to_string()),
            )?))
        }
        Rule::float => Ok(TulispValue::Float(
            value
                .as_span()
                .as_str()
                .parse()
                .map_err(|e: std::num::ParseFloatError| Error::ParsingError(e.to_string()))?,
        )),
        Rule::string => Ok(TulispValue::String(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| {
                    Error::ParsingError(format!("parsing error: string inner not found"))
                })?
                .as_span()
                .as_str()
                .to_string(),
        )),
        e => Err(Error::ParsingError(format!("unknown rule {:?}", e))),
    }
}
