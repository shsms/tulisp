use std::env;
use std::process;

use tulisp::{Error, TulispContext};

fn run() -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut ctx = TulispContext::new();
    for arg in args {
        ctx.eval_file(&arg)?;
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        println!("{e}");
        process::exit(-1);
    }
}
