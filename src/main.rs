#![warn(rust_2018_idioms)]

#[macro_use]
extern crate pest_derive;

mod builtin;
mod cons;
mod context;
mod eval;
mod parser;
mod tests;
mod value;
mod error;

use std::env;

use builtin::new_context;
use eval::eval_file;
use error::Error;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = new_context();
    for arg in args {
        eval_file(&mut ctx, &arg)?;
    }

    Ok(())
}
