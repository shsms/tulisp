use std::env;
use std::process;

use tulisp::{Error, ErrorKind, TulispContext};

fn run() -> Result<(), Error> {
    let ctx = &mut TulispContext::new();
    let args: Vec<String> = env::args().skip(1).collect();
    for arg in args {
        ctx.eval_file(&arg)?;
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        // The reader went away, as in `tulisp foo.lisp | head`.
        // Stop quietly like other Unix filters.
        if matches!(e.kind(), ErrorKind::BrokenPipe) {
            process::exit(0);
        }
        eprintln!("{e}");
        process::exit(-1);
    }
}
