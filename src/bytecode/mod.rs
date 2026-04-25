#[allow(clippy::module_inception)]
mod bytecode;
pub(crate) use bytecode::{Bytecode, CompiledDefun};

pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod lambda_template;
#[allow(unused_imports)]
pub(crate) use lambda_template::LambdaTemplate;

mod interpreter;
pub(crate) use interpreter::Machine;

mod compiler;
pub(crate) use compiler::{Compiler, VMCompilers, VMDefunParams, compile};
