use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Instruction;

#[derive(Default)]
pub(crate) struct Bytecode {
    pub(crate) global: Rc<RefCell<Vec<Instruction>>>,
    pub(crate) functions: HashMap<usize, Rc<RefCell<Vec<Instruction>>>>,
}

impl Bytecode {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)]
    pub(crate) fn print(&self) {
        println!("start:");
        for (i, instr) in self.global.borrow().iter().enumerate() {
            println!("{:<40}   # {}", instr.to_string(), i);
        }
        for (name, instr) in &self.functions {
            println!("\n{}:", name);
            for (i, instr) in instr.borrow().iter().enumerate() {
                println!("{:<40}   # {}", instr.to_string(), i);
            }
        }
    }

    pub(crate) fn import_functions(&mut self, other: &Bytecode) {
        for (name, instr) in &other.functions {
            self.functions.insert(*name, instr.clone());
        }
    }
}
