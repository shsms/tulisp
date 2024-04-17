use std::env;

use tulisp::TulispContext;

fn main() {
    let mut ctx = TulispContext::new();
    let args: Vec<String> = env::args().skip(1).collect();

    for arg in args {
        if let Err(e) = ctx.vm_eval_file(&arg) {
            println!("{}", e.format(&ctx));
            std::process::exit(-1);
        }
    }
}
