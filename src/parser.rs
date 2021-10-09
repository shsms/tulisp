use pest;

use pest::{iterators::Pair, Parser};

use crate::{cons::Cons, Error, TulispValue};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

pub fn parse_string(string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);

    let mut list = Cons::new();
    for ele in p.unwrap() {
        list.append(parse(ele)?);
    }
    Ok(TulispValue::SExp(Box::new(list)))
}

pub fn parse(value: Pair<'_, Rule>) -> Result<TulispValue, Error> {
    match value.as_rule() {
        Rule::form => Ok(TulispValue::SExp({
            let mut list = Cons::new();
            value
                .into_inner()
                .map(parse)
                .map(|val| -> Result<(), Error> {
                    list.append(val?);
                    Ok(())
                })
                .reduce(|_, _| Ok(()));
            Box::new(list)
        })),
        Rule::backquote => Ok(TulispValue::Backquote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Backquote inner not found")))?,
        )?))),
        Rule::unquote => Ok(TulispValue::Unquote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Unquote inner not found")))?,
        )?))),
        Rule::quote => Ok(TulispValue::Quote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Quote inner not found")))?,
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
