use tulisp::{vm, TulispContext};

fn main() {
    let mut machine = vm::Machine::new();
    let mut ctx = TulispContext::new();
    machine.run(&mut ctx, 0).unwrap();
}
