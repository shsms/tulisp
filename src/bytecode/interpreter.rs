use super::{instruction, Instruction};
use crate::{
    bytecode::{Pos, VMStackValue},
    Error, ErrorKind, TulispContext, TulispObject,
};
use std::collections::HashMap;

#[derive(Default, Clone)]
pub(crate) struct VMBindings {
    #[allow(dead_code)]
    name: String,
    items: Vec<VMStackValue>,
}

impl VMBindings {
    pub(crate) fn new(name: String) -> Self {
        Self {
            name,
            items: Vec::new(),
        }
    }

    pub fn set(&mut self, to_set: VMStackValue) {
        if self.items.is_empty() {
            self.items.push(to_set);
        } else {
            *self.items.last_mut().unwrap() = to_set;
        }
    }

    pub fn set_scope(&mut self, to_set: &VMStackValue) {
        self.items.push(to_set.to_owned());
    }

    pub fn unset(&mut self) {
        self.items.truncate(self.items.len() - 1);
    }

    #[allow(dead_code)]
    pub fn boundp(&self) -> bool {
        !self.items.is_empty()
    }

    pub fn get(&self) -> Result<VMStackValue, Error> {
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
}

impl Bytecode {
    pub(crate) fn new(instructions: Vec<Instruction>, bindings: Vec<VMBindings>) -> Self {
        Self {
            instructions,
            bindings,
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
    stack: Vec<VMStackValue>,
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
        Ok(self.stack.pop().unwrap().into())
    }

    fn run_impl(&mut self, ctx: &mut TulispContext, recursion_depth: u32) -> Result<(), Error> {
        while self.pc < self.bytecode.instructions.len() {
            // self.print_stack(recursion_depth);
            let instr = &mut self.bytecode.instructions[self.pc];
            match instr {
                Instruction::Push(obj) => self.stack.push(obj.clone()),
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::BinaryOp(op) => {
                    let [ref b, ref a] = self.stack[(self.stack.len() - 2)..] else {
                        unreachable!()
                    };
                    let vv = match op {
                        instruction::BinaryOp::Add => a.add(b)?,
                        instruction::BinaryOp::Sub => a.sub(b)?,
                        instruction::BinaryOp::Mul => a.mul(b)?,
                        instruction::BinaryOp::Div => a.div(b)?,
                    };
                    self.stack.truncate(self.stack.len() - 2);
                    self.stack.push(vv);
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
                    let a = self.stack.last().unwrap();
                    let cmp = a.null();
                    self.stack.truncate(self.stack.len() - 1);
                    if cmp {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfNilElsePop(pos) => {
                    let a = self.stack.last().unwrap();
                    if a.null() {
                        jump_to_pos!(self, pos);
                        continue;
                    } else {
                        self.stack.truncate(self.stack.len() - 1);
                    }
                }
                Instruction::JumpIfNeq(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = !a.eq(&b);
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLt(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = a.lt(b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLtEq(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = a.le(b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGt(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = a.gt(b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGtEq(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = a.ge(b)?;
                    self.stack.truncate(minus2);
                    if cmp {
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
                    self.stack.push(a.lt(&b)?.into());
                }
                Instruction::LtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a.le(&b)?.into());
                }
                Instruction::Gt => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a.gt(&b)?.into());
                }
                Instruction::GtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a.ge(&b)?.into());
                }
                Instruction::StorePop(obj) => {
                    let a = self.stack.pop().unwrap();
                    self.bytecode.bindings[*obj].set(a);
                }
                Instruction::Store(obj) => {
                    let a = self.stack.last().unwrap();
                    self.bytecode.bindings[*obj].set(a.clone());
                }
                Instruction::Load(obj) => {
                    let a = self.bytecode.bindings[*obj].get().unwrap();
                    self.stack.push(a.into());
                }
                Instruction::BeginScope(obj) => {
                    let a = self.stack.last().unwrap();
                    self.bytecode.bindings[*obj].set_scope(a);
                    self.stack.truncate(self.stack.len() - 1);
                }
                Instruction::EndScope(obj) => {
                    if self.bytecode.bindings[*obj].items.len() > 1 {
                        self.bytecode.bindings[*obj].unset();
                    }
                }
                Instruction::Call(pos) => {
                    let pc = self.pc;
                    jump_to_pos!(self, pos);
                    self.run_impl(ctx, recursion_depth + 1)?;
                    self.pc = pc;
                }
                Instruction::Ret => return Ok(()),
                Instruction::RustCall { func } => {
                    let args = match self.stack.pop().unwrap() {
                        VMStackValue::TulispObject(obj) => obj,
                        e => panic!("Expected TulispObject as arg to rustcall. got {}", e),
                    };
                    self.stack.push(func(ctx, &args)?.into());
                }
                Instruction::Label(_) => {}
                Instruction::Cons => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack
                        .push(TulispObject::cons(a.into(), b.into()).into());
                }
                Instruction::List(len) => {
                    let mut list = TulispObject::nil();
                    for _ in 0..*len {
                        let a = self.stack.pop().unwrap();
                        list = TulispObject::cons(a.into(), list);
                    }
                    self.stack.push(list.into());
                }
                Instruction::Append(len) => {
                    let list = TulispObject::nil();

                    for elt in self.stack.drain(self.stack.len() - *len..) {
                        list.append(<VMStackValue as Into<TulispObject>>::into(elt).deep_copy()?)?;
                    }
                    self.stack.push(list.into());
                }
                Instruction::Cxr(cxr) => {
                    let a: TulispObject = self.stack.pop().unwrap().into();
                    self.stack.push(
                        match cxr {
                            instruction::Cxr::Car => a.car().unwrap(),
                            instruction::Cxr::Cdr => a.cdr().unwrap(),
                            instruction::Cxr::Caar => a.caar().unwrap(),
                            instruction::Cxr::Cadr => a.cadr().unwrap(),
                            instruction::Cxr::Cdar => a.cdar().unwrap(),
                            instruction::Cxr::Cddr => a.cddr().unwrap(),
                            instruction::Cxr::Caaar => a.caaar().unwrap(),
                            instruction::Cxr::Caadr => a.caadr().unwrap(),
                            instruction::Cxr::Cadar => a.cadar().unwrap(),
                            instruction::Cxr::Caddr => a.caddr().unwrap(),
                            instruction::Cxr::Cdaar => a.cdaar().unwrap(),
                            instruction::Cxr::Cdadr => a.cdadr().unwrap(),
                            instruction::Cxr::Cddar => a.cddar().unwrap(),
                            instruction::Cxr::Cdddr => a.cdddr().unwrap(),
                            instruction::Cxr::Caaaar => a.caaaar().unwrap(),
                            instruction::Cxr::Caaadr => a.caaadr().unwrap(),
                            instruction::Cxr::Caadar => a.caadar().unwrap(),
                            instruction::Cxr::Caaddr => a.caaddr().unwrap(),
                            instruction::Cxr::Cadaar => a.cadaar().unwrap(),
                            instruction::Cxr::Cadadr => a.cadadr().unwrap(),
                            instruction::Cxr::Caddar => a.caddar().unwrap(),
                            instruction::Cxr::Cadddr => a.cadddr().unwrap(),
                            instruction::Cxr::Cdaaar => a.cdaaar().unwrap(),
                            instruction::Cxr::Cdaadr => a.cdaadr().unwrap(),
                            instruction::Cxr::Cdadar => a.cdadar().unwrap(),
                            instruction::Cxr::Cdaddr => a.cdaddr().unwrap(),
                            instruction::Cxr::Cddaar => a.cddaar().unwrap(),
                            instruction::Cxr::Cddadr => a.cddadr().unwrap(),
                            instruction::Cxr::Cdddar => a.cdddar().unwrap(),
                            instruction::Cxr::Cddddr => a.cddddr().unwrap(),
                        }
                        .into(),
                    )
                }
            }
            self.pc += 1;
        }
        Ok(())
    }
}
