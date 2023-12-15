use tulisp::vm;

fn main() {
    let mut machine = vm::Machine::new();
    machine.run(0).unwrap();
}
