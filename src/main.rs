#![warn(rust_2018_idioms)]

#[macro_use]
extern crate pest_derive;

mod builtin;
mod cons;
mod context;
mod eval;
mod parser;
mod value;
mod tests;

use std::env;

use builtin::new_context;
use eval::eval_file;
use value::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    NotImplemented,
    ParsingError,
    TypeMismatch,
    Undefined,
    Uninitialized,
    Unimplemented,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    span: Option<Span>,
}

impl Error {
    pub fn new(kind: ErrorKind, desc: String) -> Self {
        Error {
            kind,
            desc,
            span: None,
        }
    }
    pub fn with_span(mut self, span: Option<Span>) -> Self {
        self.span = span;
        self
    }
}

fn main() -> Result<(), Error> {
    // let string = r#"(+ 10 20 -50 (/ 4.0 -2))"#;
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))";
    // let string = "(defun test (a) (+ a 1)) (let ((vv 20)) (let ((zz (+ vv 10))) (test zz)))";
    // let string = "(defun inc-to-20 (a) (print (+ 10.0 a)) (if (< a 10) (inc-to-20 (+ a 1)) a)) (let* ((vv 2) (zz (+ vv 2))) (inc-to-20 zz))";
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (setq vv (+ vv 10)) (+ vv jj 1))";
    // let string = "(let ((vv 0)) (while (< vv 10) (setq vv (+ 1 vv))) vv)";
    // let string = "(min 10 44 2 150 89)";
    // let string = r#"(defun test () 10) (print (test))"#;

    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = new_context();
    for arg in args {
        eval_file(&mut ctx, &arg)?;
    }

    Ok(())
}

