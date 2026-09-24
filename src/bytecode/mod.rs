#[allow(clippy::module_inception)]
mod bytecode;
pub(crate) use bytecode::{Bytecode, CompiledDefun};

pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod lambda_template;
#[allow(unused_imports)]
pub(crate) use lambda_template::LambdaTemplate;

mod block;
pub(crate) use block::{Block, FormBlock, Handler};

mod interpreter;
pub(crate) use interpreter::{Machine, run, run_block, run_lambda};

mod compiler;
pub(crate) use compiler::{Compiler, VMCompilers, compile};
