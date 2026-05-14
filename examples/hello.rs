//! Minimal embedding example: register a Rust function, run a Lisp
//! program that calls it, and read the result back in Rust.
//!
//! Run with `cargo run --example hello`.

use tulisp::{Error, TulispContext};

fn main() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    // Expose a Rust function to Lisp. Argument and return-type
    // conversion (i64 here) are derived from the closure's signature.
    ctx.defun("square", |n: i64| -> i64 { n * n });

    // Run a Lisp program that mixes our function with built-ins.
    let program = "(+ (square 3) (square 4))";
    let result = ctx.eval_string(program)?;

    // Pull the result back into Rust.
    println!("{program} => {}", result.as_int()?);

    Ok(())
}
