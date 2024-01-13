pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod interpreter;
pub(crate) use interpreter::{Bytecode, Machine, VMBindings};

mod stack_value;
pub(crate) use stack_value::VMStackValue;
