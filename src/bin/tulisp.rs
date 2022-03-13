use std::env;

use tulisp::error::Error;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = tulisp::new_context();
    for arg in args {
        ctx.eval_file(&arg)?;
    }

    Ok(())
}
