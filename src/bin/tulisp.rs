use std::env;

use tulisp::builtin::new_context;
use tulisp::error::Error;
use tulisp::eval::eval_file;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = new_context();
    for arg in args {
        eval_file(&mut ctx, &arg)?;
    }

    Ok(())
}
