mod bytecode;
pub(crate) use bytecode::Bytecode;

pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod interpreter;
pub(crate) use interpreter::Machine;

mod compiler;
pub(crate) use compiler::{compile, Compiler, VMCompilers};
