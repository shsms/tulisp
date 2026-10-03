#[allow(clippy::module_inception)]
mod bytecode;
pub(crate) use bytecode::{Bytecode, CompiledDefun, CompiledDefunInner};

pub(crate) mod instruction;
pub(crate) use instruction::{Instruction, Pos};

mod lambda_template;
pub(crate) use lambda_template::{CaptureSource, LambdaTemplate};

mod block;
pub(crate) use block::{Block, FormBlock, Handler};

mod frame;
pub(crate) use frame::{Captured, Captures, Cell, FrameState, Slot};

mod interpreter;
pub(crate) use interpreter::{Machine, call_function, run, run_form_in_frame};

mod compiler;
pub(crate) use compiler::{Compiler, VMCompilers, compile};
