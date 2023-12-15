use std::fmt::Display;

use crate::{context::Scope, Error, TulispObject};

macro_rules! compare_ops {
    ($oper:expr) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<bool, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o))
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o))
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o))
            }
        }
    }};
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<TulispObject, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            }
        }
    }};
}

#[derive(Debug)]
pub enum Pos {
    Abs(usize),
    Rel(isize),
}

/// A single instruction in the VM.
#[derive(Debug)]
pub enum Instruction {
    Push(TulispObject),
    // variables
    Store(TulispObject),
    Load(TulispObject),
    // arithmetic
    Plus,
    Minus,
    // io
    Print,
    // comparison
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // control flow
    JumpIfNil(Pos),
    JumpIfEq(Pos),
    JumpIfLt(Pos),
    JumpIfLtEq(Pos),
    JumpIfGt(Pos),
    JumpIfGtEq(Pos),
    Jump(Pos),
    // functions
    Call { pos: Pos, params: Vec<TulispObject> },
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Push(obj) => write!(f, "Push({})", obj),
            Instruction::Store(obj) => write!(f, "Store({})", obj),
            Instruction::Load(obj) => write!(f, "Load({})", obj),
            Instruction::Plus => write!(f, "Plus"),
            Instruction::Minus => write!(f, "Minus"),
            Instruction::Print => write!(f, "Print"),
            Instruction::JumpIfNil(pos) => write!(f, "JumpIfNil({:?})", pos),
            Instruction::JumpIfEq(pos) => write!(f, "JumpIfEq({:?})", pos),
            Instruction::JumpIfLt(pos) => write!(f, "JumpIfLt({:?})", pos),
            Instruction::JumpIfLtEq(pos) => write!(f, "JumpIfLtEq({:?})", pos),
            Instruction::JumpIfGt(pos) => write!(f, "JumpIfGt({:?})", pos),
            Instruction::JumpIfGtEq(pos) => write!(f, "JumpIfGtEq({:?})", pos),
            Instruction::Eq => write!(f, "Eq"),
            Instruction::Lt => write!(f, "Lt"),
            Instruction::LtEq => write!(f, "LtEq"),
            Instruction::Gt => write!(f, "Gt"),
            Instruction::GtEq => write!(f, "GtEq"),
            Instruction::Jump(pos) => write!(f, "Jump({:?})", pos),
            Instruction::Call { pos, .. } => write!(f, "Call({:?})", pos),
            Instruction::Ret => write!(f, "Ret"),
        }
    }
}

pub struct Machine {
    stack: Vec<TulispObject>,
    program: Vec<Instruction>,
    pc: usize,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            stack: Vec::new(),
            program: programs::print_range(92, 100),
            pc: 0,
        }
    }

    #[allow(dead_code)]
    fn print_stack(&self) {
        println!("Stack:");
        for obj in self.stack.iter() {
            println!("  {}", obj);
        }
    }

    pub fn run(&mut self, recursion_depth: u32) -> Result<(), Error> {
        let mut ctr: u32 = 0; // safety counter
        while self.pc < self.program.len() && ctr < 100000 {
            let instr = &self.program[self.pc];
            // self.print_stack();
            // println!(
            //     "\nDepth: {}: PC: {}; Executing: {}",
            //     recursion_depth, self.pc, instr
            // );
            ctr += 1;
            self.pc += 1;
            match instr {
                Instruction::Push(obj) => self.stack.push(obj.clone()),
                Instruction::Plus => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(binary_ops!(std::ops::Add::add)(&a, &b)?);
                }
                Instruction::Minus => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(binary_ops!(std::ops::Sub::sub)(&a, &b)?);
                }
                Instruction::Print => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                }
                Instruction::JumpIfNil(pos) => {
                    let a = self.stack.pop().unwrap();
                    if a.null() {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::JumpIfEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if a.eq(&b) {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::JumpIfLt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::lt)(&a, &b)? {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::JumpIfLtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::le)(&a, &b)? {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::JumpIfGt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::gt)(&a, &b)? {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::JumpIfGtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::ge)(&a, &b)? {
                        match pos {
                            Pos::Abs(p) => self.pc = *p,
                            Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                        }
                    }
                }
                Instruction::Jump(pos) => match pos {
                    Pos::Abs(p) => self.pc = *p,
                    Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                },
                Instruction::Eq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a.eq(&b).into());
                }
                Instruction::Lt => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack
                        .push(compare_ops!(std::cmp::PartialOrd::lt)(&a, &b)?.into());
                }
                Instruction::LtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack
                        .push(compare_ops!(std::cmp::PartialOrd::le)(&a, &b)?.into());
                }
                Instruction::Gt => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack
                        .push(compare_ops!(std::cmp::PartialOrd::gt)(&a, &b)?.into());
                }
                Instruction::GtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack
                        .push(compare_ops!(std::cmp::PartialOrd::ge)(&a, &b)?.into());
                }
                Instruction::Store(obj) => {
                    let a = self.stack.pop().unwrap();
                    obj.set(a)?;
                }
                Instruction::Load(obj) => {
                    let a = obj.get()?;
                    self.stack.push(a);
                }
                Instruction::Call { pos, params } => {
                    let pc = self.pc;
                    let mut scope = Scope::default();

                    match pos {
                        Pos::Abs(p) => self.pc = *p,
                        Pos::Rel(p) => self.pc = (self.pc as isize + p - 1) as usize,
                    }

                    for param in params {
                        let value = self.stack.pop().unwrap();
                        scope.set(param.clone(), value)?;
                    }
                    self.run(recursion_depth + 1)?;
                    scope.remove_all()?;

                    self.pc = pc;
                }
                Instruction::Ret => return Ok(()),
            }
        }
        Ok(())
    }
}

mod programs {
    use crate::TulispObject;

    use super::{Instruction, Pos};

    pub(super) fn print_range(from: i64, to: i64) -> Vec<Instruction> {
        let i = TulispObject::symbol("i".to_string(), false);
        let n = TulispObject::symbol("n".to_string(), false);
        vec![
            // print numbers 1 to 10
            Instruction::Push(from.into()), // 0
            Instruction::Store(i.clone()),  // 1
            Instruction::Push(to.into()),   // 2
            Instruction::Store(n.clone()),  // 3
            // loop:
            Instruction::Load(i.clone()), // 4
            Instruction::Print,           // 5
            Instruction::Push(1.into()),  // 6
            Instruction::Load(i.clone()), // 7
            if from < to {
                Instruction::Plus
            } else {
                Instruction::Minus
            }, // 8
            Instruction::Store(i.clone()), // 9
            Instruction::Load(i.clone()), // 10
            Instruction::Load(n.clone()), // 11
            if from < to {
                Instruction::JumpIfGtEq(Pos::Abs(4))
            } else {
                Instruction::JumpIfLtEq(Pos::Abs(4))
            }, // 12
        ]
    }
}
