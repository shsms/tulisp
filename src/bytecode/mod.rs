pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod interpreter;
pub(crate) use interpreter::{Bytecode, Machine};

mod stack_value;
pub(crate) use stack_value::VMStackValue;

mod compiler;
pub(crate) use compiler::Compiler;
