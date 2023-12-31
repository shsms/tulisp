use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{value::TulispFn, Error, ErrorKind, TulispContext, TulispObject};

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

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pos::Abs(p) => write!(f, "{}", p),
            Pos::Rel(p) => write!(f, ". {}", p),
            Pos::Label(p) => write!(f, "{}", p),
        }
    }
}

/// A single instruction in the VM.
pub enum Instruction {
    // stack
    Push(TulispObject),
    Pop,
    // variables
    StorePop(usize),
    Store(usize),
    Load(usize),
    BeginScope(usize),
    EndScope(usize),
    // arithmetic
    Add,
    Sub,
    // io
    PrintPop,
    Print,
    // comparison
    Equal,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // control flow
    JumpIfNil(Pos),
    JumpIfNeq(Pos),
    JumpIfLt(Pos),
    JumpIfLtEq(Pos),
    JumpIfGt(Pos),
    JumpIfGtEq(Pos),
    Jump(Pos),
    // functions
    Label(TulispObject),
    RustCall { func: Rc<TulispFn> },
    Call(Pos),
    Ret,
    // lists
    Cons,
    List(usize),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Push(obj) => write!(f, "    push {}", obj),
            Instruction::Pop => write!(f, "    pop"),
            Instruction::StorePop(obj) => write!(f, "    store_pop {}", obj),
            Instruction::Store(obj) => write!(f, "    store {}", obj),
            Instruction::Load(obj) => write!(f, "    load {}", obj),
            Instruction::BeginScope(obj) => write!(f, "    begin_scope {}", obj),
            Instruction::EndScope(obj) => write!(f, "    end_scope {}", obj),
            Instruction::Add => write!(f, "    add"),
            Instruction::Sub => write!(f, "    sub"),
            Instruction::PrintPop => write!(f, "    print_pop"),
            Instruction::Print => write!(f, "    print"),
            Instruction::JumpIfNil(pos) => write!(f, "    jnil {}", pos),
            Instruction::JumpIfNeq(pos) => write!(f, "    jne {}", pos),
            Instruction::JumpIfLt(pos) => write!(f, "    jlt {}", pos),
            Instruction::JumpIfLtEq(pos) => write!(f, "    jle {}", pos),
            Instruction::JumpIfGt(pos) => write!(f, "    jgt {}", pos),
            Instruction::JumpIfGtEq(pos) => write!(f, "    jge {}", pos),
            Instruction::Equal => write!(f, "    equal"),
            Instruction::Eq => write!(f, "    ceq"),
            Instruction::Lt => write!(f, "    clt"),
            Instruction::LtEq => write!(f, "    cle"),
            Instruction::Gt => write!(f, "    cgt"),
            Instruction::GtEq => write!(f, "    cge"),
            Instruction::Jump(pos) => write!(f, "    jmp {}", pos),
            Instruction::Call(pos) => write!(f, "    call {}", pos),
            Instruction::Ret => write!(f, "    ret"),
            Instruction::RustCall { .. } => write!(f, "    rustcall"),
            Instruction::Label(name) => write!(f, "{}:", name),
            Instruction::Cons => write!(f, "    cons"),
            Instruction::List(len) => write!(f, "    list {}", len),
        }
    }
}

#[derive(Default, Clone, Debug)]
pub(crate) struct VMBindings {
    name: String,
    items: Vec<TulispObject>,
}

impl VMBindings {
    pub(crate) fn new(name: String) -> Self {
        Self {
            name,
            items: Vec::new(),
        }
    }

    pub fn set(&mut self, to_set: TulispObject) {
        if self.items.is_empty() {
            self.items.push(to_set);
        } else {
            *self.items.last_mut().unwrap() = to_set;
        }
    }

    pub fn set_scope(&mut self, to_set: TulispObject) {
        self.items.push(to_set);
    }

    pub fn unset(&mut self) {
        self.items.pop();
    }

    #[allow(dead_code)]
    pub fn boundp(&self) -> bool {
        !self.items.is_empty()
    }

    pub fn get(&self) -> Result<TulispObject, Error> {
        if self.items.is_empty() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Variable definition is void: {}", self.name),
            ));
        }
        return Ok(self.items.last().unwrap().clone());
    }
}

pub(crate) struct Bytecode {
    instructions: Vec<Instruction>,
    bindings: Vec<VMBindings>,
    #[allow(dead_code)]
    symbol_to_binding_idx: HashMap<usize, usize>,
}

impl Bytecode {
    pub(crate) fn new(
        instructions: Vec<Instruction>,
        bindings: Vec<VMBindings>,
        symbol_to_binding_idx: HashMap<usize, usize>,
    ) -> Self {
        Self {
            instructions,
            bindings,
            symbol_to_binding_idx,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn print(&self) {
        println!("start:");
        for (i, instr) in self.instructions.iter().enumerate() {
            println!("{:<40}   # {}", instr.to_string(), i);
        }
        println!("Number of bindings: {}", self.bindings.len());
    }
}

pub struct Machine {
    stack: Vec<TulispObject>,
    bytecode: Bytecode,
    labels: HashMap<usize, usize>, // TulispObject.addr -> instruction index
    pc: usize,
}

macro_rules! jump_to_pos {
    ($self:ident, $pos:ident) => {
        $self.pc = {
            match $pos {
                Pos::Abs(p) => *p,
                Pos::Rel(p) => {
                    let abs_pos = ($self.pc as isize + *p + 1) as usize;
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
    pub(crate) fn new(bytecode: Bytecode) -> Self {
        Machine {
            stack: Vec::new(),
            labels: Self::locate_labels(&bytecode.instructions),
            bytecode,
            // program: programs::print_range(92, 100),
            // program: programs::fib(30),
            pc: 0,
        }
    }

    fn locate_labels(program: &Vec<Instruction>) -> HashMap<usize, usize> {
        let mut labels = HashMap::new();
        for (i, instr) in program.iter().enumerate() {
            if let Instruction::Label(name) = instr {
                labels.insert(name.addr_as_usize(), i + 1);
            }
        }
        labels
    }

    #[allow(dead_code)]
    fn print_stack(&self, recursion_depth: u32) {
        println!("Stack:");
        for obj in self.stack.iter() {
            println!("  {}", obj);
        }
        println!(
            "\nDepth: {}: PC: {}; Executing: {}",
            recursion_depth, self.pc, self.bytecode.instructions[self.pc]
        );
    }

    pub fn run(&mut self, ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        self.run_impl(ctx, 0)?;
        Ok(self.stack.pop().unwrap())
    }

    fn run_impl(&mut self, ctx: &mut TulispContext, recursion_depth: u32) -> Result<(), Error> {
        while self.pc < self.bytecode.instructions.len() {
            // self.print_stack(recursion_depth);
            let instr = &mut self.bytecode.instructions[self.pc];
            match instr {
                Instruction::Push(obj) => self.stack.push(obj.clone()),
                Instruction::Pop => {
                    self.stack.pop().unwrap();
                }
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
                        continue;
                    }
                }
                Instruction::JumpIfNeq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if !a.eq(&b) {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::lt)(&a, &b)? {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::le)(&a, &b)? {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGt(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::gt)(&a, &b)? {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGtEq(pos) => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    if compare_ops!(std::cmp::PartialOrd::ge)(&a, &b)? {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::Jump(pos) => {
                    jump_to_pos!(self, pos);
                    continue;
                }
                Instruction::Equal => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a.equal(&b).into());
                }
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
                    let _ = self.bytecode.bindings[*obj].set(a);
                }
                Instruction::Store(obj) => {
                    let a = self.stack.last().unwrap();
                    let _ = self.bytecode.bindings[*obj].set(a.clone());
                }
                Instruction::Load(obj) => {
                    let a = self.bytecode.bindings[*obj].get().unwrap();
                    self.stack.push(a);
                }
                Instruction::BeginScope(obj) => {
                    let a = self.stack.pop().unwrap();
                    let _ = self.bytecode.bindings[*obj].set_scope(a);
                }
                Instruction::EndScope(obj) => {
                    let _ = self.bytecode.bindings[*obj].unset();
                }
                Instruction::Call(pos) => {
                    let pc = self.pc;
                    jump_to_pos!(self, pos);
                    self.run_impl(ctx, recursion_depth + 1)?;
                    self.pc = pc;
                }
                Instruction::Ret => return Ok(()),
                Instruction::RustCall { func } => {
                    let args = self.stack.pop().unwrap();
                    self.stack.push(func(ctx, &args)?);
                }
                Instruction::Label(_) => {}
                Instruction::Cons => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(TulispObject::cons(a, b));
                }
                Instruction::List(len) => {
                    let mut list = TulispObject::nil();
                    for _ in 0..*len {
                        let a = self.stack.pop().unwrap();
                        list = TulispObject::cons(a, list);
                    }
                    self.stack.push(list);
                }
            }
            self.pc += 1;
        }
        Ok(())
    }
}

mod programs {
    // use super::*;

    // use crate::{list, TulispContext, TulispObject, TulispValue};

    // #[allow(dead_code)]
    // pub(super) fn print_range(from: i64, to: i64) -> Vec<Instruction> {
    //     let i = TulispObject::symbol("i".to_string(), false);
    //     let n = TulispObject::symbol("n".to_string(), false);
    //     vec![
    //         // print numbers 1 to 10
    //         Instruction::Push(from.into()),   // 0
    //         Instruction::StorePop(i.clone()), // 1
    //         Instruction::Push(to.into()),     // 2
    //         Instruction::StorePop(n.clone()), // 3
    //         // loop:
    //         Instruction::Load(i.clone()), // 4
    //         Instruction::PrintPop,        // 5
    //         Instruction::Push(1.into()),  // 6
    //         Instruction::Load(i.clone()), // 7
    //         if from < to {
    //             Instruction::Add
    //         } else {
    //             Instruction::Sub
    //         }, // 8
    //         Instruction::StorePop(i.clone()), // 9
    //         Instruction::Load(i.clone()), // 10
    //         Instruction::Load(n.clone()), // 11
    //         if from < to {
    //             Instruction::JumpIfGtEq(Pos::Abs(4))
    //         } else {
    //             Instruction::JumpIfLtEq(Pos::Abs(4))
    //         }, // 12
    //     ]
    // }

    // #[allow(dead_code)]
    // pub(super) fn fib(num: i64) -> Vec<Instruction> {
    //     let n = TulispObject::symbol("n".to_string(), false);
    //     let fib = TulispObject::symbol("fib".to_string(), false);
    //     let main = TulispObject::symbol("main".to_string(), false);
    //     vec![
    //         Instruction::Jump(Pos::Label(main.clone())), // 0
    //         Instruction::Label(fib.clone()),             // 1
    //         Instruction::Push(2.into()),                 // 2
    //         Instruction::Load(n.clone()),                // 3
    //         Instruction::JumpIfGt(Pos::Rel(2)),          // 4
    //         Instruction::Push(1.into()),                 // 5
    //         Instruction::Ret,                            // 6
    //         Instruction::Push(1.into()),                 // 7
    //         Instruction::Load(n.clone()),                // 8
    //         Instruction::Sub,                            // 9
    //         Instruction::Call {
    //             pos: Pos::Label(fib.clone()),
    //             params: vec![n.clone()],
    //         }, // 10
    //         Instruction::Push(2.into()),                 // 11
    //         Instruction::Load(n.clone()),                // 12
    //         Instruction::Sub,                            // 13
    //         Instruction::Call {
    //             pos: Pos::Label(fib.clone()),
    //             params: vec![n.clone()],
    //         }, // 14
    //         Instruction::Add,                            // 15
    //         Instruction::Ret,                            // 16
    //         Instruction::Label(main.clone()),            // 17
    //         Instruction::Push(num.into()),               // 18
    //         Instruction::Call {
    //             pos: Pos::Label(fib.clone()),
    //             params: vec![n.clone()],
    //         }, // 19
    //         Instruction::PrintPop,                       // 20
    //     ]
    // }

    // #[allow(dead_code)]
    // pub(super) fn rustcall_dotimes(ctx: &mut TulispContext, num: i64) -> Vec<Instruction> {
    //     let var = TulispObject::symbol("var".to_string(), false);
    //     let args = list!(
    //         list!(var.clone(), num.into()).unwrap(),
    //         list!(ctx.intern("print"), var).unwrap()
    //     )
    //     .unwrap();
    //     vec![
    //         Instruction::Push(args), // 0
    //         Instruction::RustCall {
    //             func: {
    //                 let obj = ctx.intern("dotimes").get().unwrap();
    //                 if let TulispValue::Func(ref func) = obj.clone_inner() {
    //                     func.clone()
    //                 } else {
    //                     panic!("Expected function")
    //                 }
    //             },
    //         },
    //     ]
    // }
}
