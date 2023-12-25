use tulisp::{vm, TulispContext};

fn main() {
    let mut ctx = TulispContext::new();
    let mut machine = vm::Machine::new(&mut ctx);
    if let Err(e) = machine.run(&mut ctx, 0) {
        println!("{}", e.format(&ctx));
        std::process::exit(-1);
    }
}
