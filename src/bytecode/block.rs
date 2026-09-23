//! Protected bodies: code a special form runs on its own, so it can
//! act on the body's value, error or throw.

use super::bytecode::{TraceRange, assemble};
use super::instruction::Instruction;
use crate::Error;
use crate::object::wrappers::generic::{Shared, SharedMut};

/// A body compiled as its own unit. Only the instruction that holds it
/// runs it, since it reads the lexical bindings of the code around that
/// instruction.
#[derive(Clone)]
pub(crate) struct Block {
    pub(crate) instructions: SharedMut<Vec<Instruction>>,
    pub(crate) trace_ranges: Shared<Vec<TraceRange>>,
    /// Whether the block starts by binding a value its runner pushes.
    pub(crate) takes_arg: bool,
}

impl Block {
    /// Assembles INSTRUCTIONS as their own unit, so their labels and
    /// trace markers resolve within the block.
    pub(crate) fn new(instructions: Vec<Instruction>, takes_arg: bool) -> Result<Block, Error> {
        let (instructions, trace_ranges) = assemble(instructions)?;
        Ok(Block {
            instructions: SharedMut::new(instructions),
            trace_ranges: Shared::new(trace_ranges),
            takes_arg,
        })
    }
}
