use std::{collections::HashMap, fmt};

use super::Instruction;
use crate::{bytecode::compiler::VMDefunParams, object::wrappers::generic::SharedMut, TulispObject};

#[derive(Default, Clone)]
pub(crate) struct CompiledDefun {
    pub(crate) name: TulispObject,
    pub(crate) instructions: SharedMut<Vec<Instruction>>,
    pub(crate) params: VMDefunParams,
}

#[derive(Default, Clone)]
pub(crate) struct Bytecode {
    pub(crate) global: SharedMut<Vec<Instruction>>,
    pub(crate) functions: HashMap<usize, CompiledDefun>, // key: fn_name.addr_as_usize()
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, instr) in self.global.borrow().iter().enumerate() {
            write!(f, "\n{:<40}   # {}", instr.to_string(), i)?;
        }
        for (name, func) in &self.functions {
            write!(f, "\n\n{}:", name)?;
            for (i, instr) in func.instructions.borrow().iter().enumerate() {
                write!(f, "\n{:<40}   # {}", instr.to_string(), i)?;
            }
        }
        Ok(())
    }
}

impl Bytecode {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn import_functions(&mut self, other: &Bytecode) {
        self.functions.extend(other.functions.clone());
    }
}
