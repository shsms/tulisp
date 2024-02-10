use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::TulispObject;

use super::Instruction;

#[derive(Default)]
pub(crate) struct Bytecode {
    pub(crate) global: Rc<RefCell<Vec<Instruction>>>,
    pub(crate) functions: HashMap<usize, Rc<RefCell<Vec<Instruction>>>>,
    pub(crate) defun_args: HashMap<usize, Vec<TulispObject>>, // fn_name.addr_as_usize() -> arg symbol idx
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
        self.functions.extend(other.functions.clone());
        self.defun_args.extend(other.defun_args.clone());
    }
}
