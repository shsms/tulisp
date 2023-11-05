use std::env;
use std::process;

use tulisp::{Error, TulispContext};

fn run(ctx: &mut TulispContext) -> Result<(), Error> {
    let args: Vec<String> = env::args().skip(1).collect();
    for arg in args {
        ctx.eval_file(&arg)?;
    }

    Ok(())
}

fn main() {
    let mut ctx = TulispContext::new();

    if let Err(e) = run(&mut ctx) {
        println!("{}", e.format(&ctx));
        process::exit(-1);
    }
}
