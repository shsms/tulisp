//! Calling Lisp from Rust: `ctx.funcall` invokes a named function,
//! `ctx.map` applies a function across a list. Useful when the host
//! application owns the data and wants user-supplied Lisp to run
//! against it.
//!
//! Run with `cargo run --example callbacks`.

use tulisp::{Error, TulispContext, TulispObject, list};

fn main() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    // Define a Lisp function once.
    ctx.eval_string("(defun double (x) (* x 2))")?;

    // Look up the symbol and invoke it with an arg list built in Rust.
    // `list!` is the in-crate macro for constructing TulispObject lists.
    let double = ctx.intern("double");
    let result = ctx.funcall(&double, &list!(,TulispObject::from(21))?)?;
    println!("(double 21) => {}", result.as_int()?);

    // `ctx.map` is the same shape as Emacs' `mapcar`: apply a function
    // value over each element of a list. The function can be anything
    // callable — a named symbol, a `(lambda …)`, or a captured closure
    // from earlier eval.
    let nums: TulispObject = (1..=5).map(TulispObject::from).collect();
    let squarer = ctx.eval_string("(lambda (n) (* n n))")?;
    let squared = ctx.map(&squarer, &nums)?;
    println!("map (lambda (n) (* n n)) {nums} => {squared}");

    // `ctx.reduce` folds with an initial value. Handy for aggregations.
    let plus = ctx.intern("+");
    let total = ctx.reduce(&plus, &squared, &TulispObject::from(0))?;
    println!("sum of squares => {}", total.as_int()?);

    Ok(())
}
