use std::{collections::HashMap, rc::Rc};

use super::Instruction;

#[derive(Default)]
pub(crate) struct Bytecode {
    pub(crate) global: Rc<Vec<Instruction>>,
    pub(crate) functions: HashMap<usize, Rc<Vec<Instruction>>>,
}

impl Bytecode {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)]
    pub(crate) fn print(&self) {
        println!("start:");
        for (i, instr) in self.global.iter().enumerate() {
            println!("{:<40}   # {}", instr.to_string(), i);
        }
        for (name, instr) in &self.functions {
            println!("\n{}:", name);
            for (i, instr) in instr.iter().enumerate() {
                println!("{:<40}   # {}", instr.to_string(), i);
            }
        }
    }
}
