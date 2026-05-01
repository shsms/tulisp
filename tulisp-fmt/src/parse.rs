//! Parser stub.

use crate::{cst::Cst, lex::Token};

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

pub fn parse(_tokens: &[Token]) -> Result<Cst, ParseError> {
    Ok(Cst)
}
