use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{context::Scope, value::TulispFn, Error, TulispContext, TulispObject};

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
    Label(TulispObject),
}

/// A single instruction in the VM.
pub enum Instruction {
    Push(TulispObject),
    // variables
    StorePop(TulispObject),
    Store(TulispObject),
    Load(TulispObject),
    // arithmetic
    Add,
    Sub,
    // io
    PrintPop,
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
    Label(TulispObject),
    RustCall { func: Rc<TulispFn> },
    Call { pos: Pos, params: Vec<TulispObject> },
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Push(obj) => write!(f, "    push {}", obj),
            Instruction::StorePop(obj) => write!(f, "    store_pop {}", obj),
            Instruction::Store(obj) => write!(f, "    store {}", obj),
            Instruction::Load(obj) => write!(f, "    load {}", obj),
            Instruction::Add => write!(f, "    add"),
            Instruction::Sub => write!(f, "    sub"),
            Instruction::PrintPop => write!(f, "    print_pop"),
            Instruction::Print => write!(f, "    print"),
            Instruction::JumpIfNil(pos) => write!(f, "    jnil {:?}", pos),
            Instruction::JumpIfEq(pos) => write!(f, "    jeq {:?}", pos),
            Instruction::JumpIfLt(pos) => write!(f, "    jlt {:?}", pos),
            Instruction::JumpIfLtEq(pos) => write!(f, "    jle {:?}", pos),
            Instruction::JumpIfGt(pos) => write!(f, "    jgt {:?}", pos),
            Instruction::JumpIfGtEq(pos) => write!(f, "    jge {:?}", pos),
            Instruction::Eq => write!(f, "    ceq"),
            Instruction::Lt => write!(f, "    clt"),
            Instruction::LtEq => write!(f, "    cle"),
            Instruction::Gt => write!(f, "    cgt"),
            Instruction::GtEq => write!(f, "    cge"),
            Instruction::Jump(pos) => write!(f, "    jmp {:?}", pos),
            Instruction::Call { pos, .. } => write!(f, "    call {:?}", pos),
            Instruction::Ret => write!(f, "    ret"),
            Instruction::RustCall { .. } => write!(f, "    rustcall"),
            Instruction::Label(name) => write!(f, "{}:", name),
        }
    }
}

pub struct Machine {
    stack: Vec<TulispObject>,
    program: Vec<Instruction>,
    labels: HashMap<usize, usize>, // TulispObject.addr -> instruction index
    pc: usize,
}

macro_rules! jump_to_pos {
    ($self:ident, $pos:ident) => {
        $self.pc = {
            match $pos {
                Pos::Abs(p) => *p,
                Pos::Rel(p) => {
                    let abs_pos = ($self.pc as isize + *p) as usize;
                    *$pos = Pos::Abs(abs_pos);
                    abs_pos
                }
                Pos::Label(p) => {
                    let abs_pos = *$self.labels.get(&p.addr_as_usize()).unwrap();
                    *$pos = Pos::Abs(abs_pos);
                    abs_pos
                }
            }
        }
    };
}

impl Machine {
    pub fn new(program: Vec<Instruction>) -> Self {
        Machine {
            stack: Vec::new(),
            labels: Self::locate_labels(&program),
            // program: programs::print_range(92, 100),
            // program: programs::fib(30),
            program,
            pc: 0,
        }
    }

    fn locate_labels(program: &Vec<Instruction>) -> HashMap<usize, usize> {
        let mut labels = HashMap::new();
        for (i, instr) in program.iter().enumerate() {
            if let Instruction::Label(name) = instr {
                labels.insert(name.addr_as_usize(), i);
            }
        }
        labels
    }

    #[allow(dead_code)]
    fn print_stack(&self) {
        println!("Stack:");
        for obj in self.stack.iter() {
            println!("  {}", obj);
        }
    }

    pub fn run(&mut self, ctx: &mut TulispContext, recursion_depth: u32) -> Result<(), Error> {
        let mut ctr: u32 = 0; // safety counter
        while self.pc < self.program.len() && ctr < 100000 {
            let instr = &mut self.program[self.pc];
            // self.print_stack();
            // println!(
            //     "\nDepth: {}: PC: {}; Executing: {}",
            //     recursion_depth, self.pc, instr
            // );
            ctr += 1;
            match instr {
                Instruction::Push(obj) => self.stack.push(obj.clone()),
                Instruction::Add => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(binary_ops!(std::ops::Add::add)(&a, &b)?);
                }
                Instruction::Sub => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(binary_ops!(std::ops::Sub::sub)(&a, &b)?);
                }
                Instruction::PrintPop => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                }
                Instruction::Print => {
                    let a = self.stack.last().unwrap();
                    println!("{}", a);
                }
                Instruction::JumpIfNil(pos) => {
                    let a = self.stack.pop().unwrap();
                    if a.null() {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::JumpIfEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if a.eq(&b) {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::JumpIfLt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::lt)(&a, &b)? {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::JumpIfLtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::le)(&a, &b)? {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::JumpIfGt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::gt)(&a, &b)? {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::JumpIfGtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::ge)(&a, &b)? {
                        jump_to_pos!(self, pos);
                    }
                }
                Instruction::Jump(pos) => jump_to_pos!(self, pos),
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
                Instruction::StorePop(obj) => {
                    let a = self.stack.pop().unwrap();
                    obj.set(a)?;
                }
                Instruction::Store(obj) => {
                    let a = self.stack.last().unwrap();
                    obj.set(a.clone())?;
                }
                Instruction::Load(obj) => {
                    let a = obj.get()?;
                    self.stack.push(a);
                }
                Instruction::Call { pos, params } => {
                    let pc = self.pc;
                    jump_to_pos!(self, pos);

                    let mut scope = Scope::default();
                    for param in params {
                        let value = self.stack.pop().unwrap();
                        scope.set(param.clone(), value)?;
                    }
                    self.run(ctx, recursion_depth + 1)?;
                    scope.remove_all()?;

                    self.pc = pc;
                }
                Instruction::Ret => return Ok(()),
                Instruction::RustCall { func } => {
                    let args = self.stack.pop().unwrap();
                    self.stack.push(func(ctx, &args)?);
                }
                Instruction::Label(_) => {}
            }
            self.pc += 1;
        }
        Ok(())
    }
}

mod programs {
    use super::*;

    use crate::{list, TulispContext, TulispObject, TulispValue};

    #[allow(dead_code)]
    pub(super) fn print_range(from: i64, to: i64) -> Vec<Instruction> {
        let i = TulispObject::symbol("i".to_string(), false);
        let n = TulispObject::symbol("n".to_string(), false);
        vec![
            // print numbers 1 to 10
            Instruction::Push(from.into()),   // 0
            Instruction::StorePop(i.clone()), // 1
            Instruction::Push(to.into()),     // 2
            Instruction::StorePop(n.clone()), // 3
            // loop:
            Instruction::Load(i.clone()), // 4
            Instruction::PrintPop,        // 5
            Instruction::Push(1.into()),  // 6
            Instruction::Load(i.clone()), // 7
            if from < to {
                Instruction::Add
            } else {
                Instruction::Sub
            }, // 8
            Instruction::StorePop(i.clone()), // 9
            Instruction::Load(i.clone()), // 10
            Instruction::Load(n.clone()), // 11
            if from < to {
                Instruction::JumpIfGtEq(Pos::Abs(4))
            } else {
                Instruction::JumpIfLtEq(Pos::Abs(4))
            }, // 12
        ]
    }

    #[allow(dead_code)]
    pub(super) fn fib(num: i64) -> Vec<Instruction> {
        let n = TulispObject::symbol("n".to_string(), false);
        let fib = TulispObject::symbol("fib".to_string(), false);
        let main = TulispObject::symbol("main".to_string(), false);
        vec![
            Instruction::Jump(Pos::Label(main.clone())), // 0
            Instruction::Label(fib.clone()),             // 1
            Instruction::Push(2.into()),                 // 2
            Instruction::Load(n.clone()),                // 3
            Instruction::JumpIfGt(Pos::Rel(2)),          // 4
            Instruction::Push(1.into()),                 // 5
            Instruction::Ret,                            // 6
            Instruction::Push(1.into()),                 // 7
            Instruction::Load(n.clone()),                // 8
            Instruction::Sub,                            // 9
            Instruction::Call {
                pos: Pos::Label(fib.clone()),
                params: vec![n.clone()],
            }, // 10
            Instruction::Push(2.into()),                 // 11
            Instruction::Load(n.clone()),                // 12
            Instruction::Sub,                            // 13
            Instruction::Call {
                pos: Pos::Label(fib.clone()),
                params: vec![n.clone()],
            }, // 14
            Instruction::Add,                            // 15
            Instruction::Ret,                            // 16
            Instruction::Label(main.clone()),            // 17
            Instruction::Push(num.into()),               // 18
            Instruction::Call {
                pos: Pos::Label(fib.clone()),
                params: vec![n.clone()],
            }, // 19
            Instruction::PrintPop,                       // 20
        ]
    }

    #[allow(dead_code)]
    pub(super) fn rustcall_dotimes(ctx: &mut TulispContext, num: i64) -> Vec<Instruction> {
        let var = TulispObject::symbol("var".to_string(), false);
        let args = list!(
            list!(var.clone(), num.into()).unwrap(),
            list!(ctx.intern("print"), var).unwrap()
        )
        .unwrap();
        vec![
            Instruction::Push(args), // 0
            Instruction::RustCall {
                func: {
                    let obj = ctx.intern("dotimes").get().unwrap();
                    if let TulispValue::Func(ref func) = obj.clone_inner() {
                        func.clone()
                    } else {
                        panic!("Expected function")
                    }
                },
            },
        ]
    }
}
