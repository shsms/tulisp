use std::env;

use tulisp::{Error, TulispContext};

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = TulispContext::new();
    for arg in args {
        ctx.eval_file(&arg)?;
    }

    Ok(())
}
