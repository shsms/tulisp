use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{value::TulispFn, Error, ErrorKind, TulispContext, TulispObject, TulispValue};

macro_rules! compare_ops {
    ($name:ident, $oper:expr) => {
        fn $name(selfobj: &VMStackValue, other: &VMStackValue) -> Result<bool, Error> {
            match selfobj {
                VMStackValue::Float(f1) => match other {
                    VMStackValue::Float(f2) => Ok($oper(f1, f2)),
                    VMStackValue::Int(i2) => Ok($oper(f1, &(*i2 as f64))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                VMStackValue::Int(i1) => match other {
                    VMStackValue::Float(f2) => Ok($oper(&(*i1 as f64), f2)),
                    VMStackValue::Int(i2) => Ok($oper(i1, i2)),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected number, found: {}", selfobj),
                )),
            }
        }
    };
}

compare_ops!(cmp_lt, std::cmp::PartialOrd::lt);
compare_ops!(cmp_le, std::cmp::PartialOrd::le);
compare_ops!(cmp_gt, std::cmp::PartialOrd::gt);
compare_ops!(cmp_ge, std::cmp::PartialOrd::ge);

macro_rules! binary_ops {
    ($name:ident, $oper:expr) => {
        fn $name(selfobj: &VMStackValue, other: &VMStackValue) -> Result<VMStackValue, Error> {
            match selfobj {
                VMStackValue::Float(f1) => match other {
                    VMStackValue::Float(f2) => Ok(VMStackValue::Float($oper(f1, f2))),
                    VMStackValue::Int(i2) => Ok(VMStackValue::Float($oper(f1, &(*i2 as f64)))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                VMStackValue::Int(i1) => match other {
                    VMStackValue::Float(f2) => Ok(VMStackValue::Float($oper(&(*i1 as f64), f2))),
                    VMStackValue::Int(i2) => Ok(VMStackValue::Int($oper(i1, i2))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected number, found: {}", selfobj),
                )),
            }
        }
    };
}

binary_ops!(arith_add, std::ops::Add::add);
binary_ops!(arith_sub, std::ops::Sub::sub);
binary_ops!(arith_mul, std::ops::Mul::mul);
binary_ops!(arith_div, std::ops::Div::div);

#[derive(Clone)]
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

#[derive(Clone, Copy)]
pub(crate) enum InstructionCxr {
    Car,
    Cdr,
    Caar,
    Cadr,
    Cdar,
    Cddr,
    Caaar,
    Caadr,
    Cadar,
    Caddr,
    Cdaar,
    Cdadr,
    Cddar,
    Cdddr,
    Caaaar,
    Caaadr,
    Caadar,
    Caaddr,
    Cadaar,
    Cadadr,
    Caddar,
    Cadddr,
    Cdaaar,
    Cdaadr,
    Cdadar,
    Cdaddr,
    Cddaar,
    Cddadr,
    Cdddar,
    Cddddr,
}

#[derive(Clone, Copy)]
pub(crate) enum InstructionBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// A single instruction in the VM.
#[derive(Clone)]
pub(crate) enum Instruction {
    // stack
    Push(VMStackValue),
    Pop,
    // variables
    StorePop(usize),
    Store(usize),
    Load(usize),
    BeginScope(usize),
    EndScope(usize),
    // arithmetic
    BinaryOp(InstructionBinaryOp),
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
    JumpIfNilElsePop(Pos),
    JumpIfNeq(Pos),
    JumpIfLt(Pos),
    JumpIfLtEq(Pos),
    JumpIfGt(Pos),
    JumpIfGtEq(Pos),
    Jump(Pos),
    // functions
    Label(TulispObject),
    #[allow(dead_code)]
    RustCall {
        func: Rc<TulispFn>,
    },
    Call(Pos),
    Ret,
    // lists
    Cons,
    List(usize),
    Append(usize),
    Cxr(InstructionCxr),
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
            Instruction::BinaryOp(op) => match op {
                InstructionBinaryOp::Add => write!(f, "    add"),
                InstructionBinaryOp::Sub => write!(f, "    sub"),
                InstructionBinaryOp::Mul => write!(f, "    mul"),
                InstructionBinaryOp::Div => write!(f, "    div"),
            },
            Instruction::PrintPop => write!(f, "    print_pop"),
            Instruction::Print => write!(f, "    print"),
            Instruction::JumpIfNil(pos) => write!(f, "    jnil {}", pos),
            Instruction::JumpIfNilElsePop(pos) => write!(f, "    jnil_else_pop {}", pos),
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
            Instruction::Append(len) => write!(f, "    append {}", len),
            Instruction::Cxr(cxr) => match cxr {
                InstructionCxr::Car => write!(f, "    car"),
                InstructionCxr::Cdr => write!(f, "    cdr"),
                InstructionCxr::Caar => write!(f, "    caar"),
                InstructionCxr::Cadr => write!(f, "    cadr"),
                InstructionCxr::Cdar => write!(f, "    cdar"),
                InstructionCxr::Cddr => write!(f, "    cddr"),
                InstructionCxr::Caaar => write!(f, "    caaar"),
                InstructionCxr::Caadr => write!(f, "    caadr"),
                InstructionCxr::Cadar => write!(f, "    cadar"),
                InstructionCxr::Caddr => write!(f, "    caddr"),
                InstructionCxr::Cdaar => write!(f, "    cdaar"),
                InstructionCxr::Cdadr => write!(f, "    cdadr"),
                InstructionCxr::Cddar => write!(f, "    cddar"),
                InstructionCxr::Cdddr => write!(f, "    cdddr"),
                InstructionCxr::Caaaar => write!(f, "    caaaar"),
                InstructionCxr::Caaadr => write!(f, "    caaadr"),
                InstructionCxr::Caadar => write!(f, "    caadar"),
                InstructionCxr::Caaddr => write!(f, "    caaddr"),
                InstructionCxr::Cadaar => write!(f, "    cadaar"),
                InstructionCxr::Cadadr => write!(f, "    cadadr"),
                InstructionCxr::Caddar => write!(f, "    caddar"),
                InstructionCxr::Cadddr => write!(f, "    cadddr"),
                InstructionCxr::Cdaaar => write!(f, "    cdaaar"),
                InstructionCxr::Cdaadr => write!(f, "    cdaadr"),
                InstructionCxr::Cdadar => write!(f, "    cdadar"),
                InstructionCxr::Cdaddr => write!(f, "    cdaddr"),
                InstructionCxr::Cddaar => write!(f, "    cddaar"),
                InstructionCxr::Cddadr => write!(f, "    cddadr"),
                InstructionCxr::Cdddar => write!(f, "    cdddar"),
                InstructionCxr::Cddddr => write!(f, "    cddddr"),
            },
        }
    }
}

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

#[derive(Clone)]
pub(crate) enum VMStackValue {
    TulispObject(TulispObject),
    Bool(bool),
    Float(f64),
    Int(i64),
}

macro_rules! impl_from_for_stack_value {
    ($name:ident, $type:ty) => {
        impl From<$type> for VMStackValue {
            fn from(val: $type) -> Self {
                VMStackValue::$name(val)
            }
        }
    };
}

impl_from_for_stack_value!(Float, f64);
impl_from_for_stack_value!(Int, i64);
impl_from_for_stack_value!(Bool, bool);

impl From<TulispObject> for VMStackValue {
    fn from(val: TulispObject) -> Self {
        match &*val.inner_ref() {
            TulispValue::Int { value } => return VMStackValue::Int(*value),
            TulispValue::Float { value } => return VMStackValue::Float(*value),
            TulispValue::T => return VMStackValue::Bool(true),
            TulispValue::Nil => return VMStackValue::Bool(false),
            _ => {}
        }
        VMStackValue::TulispObject(val)
    }
}

impl Into<TulispObject> for VMStackValue {
    fn into(self) -> TulispObject {
        match self {
            VMStackValue::TulispObject(obj) => obj,
            VMStackValue::Bool(b) => b.into(),
            VMStackValue::Float(fl) => fl.into(),
            VMStackValue::Int(i) => i.into(),
        }
    }
}

impl VMStackValue {
    pub fn null(&self) -> bool {
        match self {
            VMStackValue::TulispObject(obj) => obj.null(),
            VMStackValue::Bool(b) => !b,
            VMStackValue::Float(_) | VMStackValue::Int(_) => false,
        }
    }

    pub fn equal(&self, other: &VMStackValue) -> bool {
        match self {
            VMStackValue::TulispObject(obj1) => match other {
                VMStackValue::TulispObject(obj2) => obj1.equal(obj2),
                VMStackValue::Bool(b2) => obj1.equal(&(*b2).into()),
                VMStackValue::Float(fl2) => obj1.equal(&(*fl2).into()),
                VMStackValue::Int(i2) => obj1.equal(&(*i2).into()),
            },
            VMStackValue::Bool(b) => match other {
                VMStackValue::Bool(b2) => b == b2,
                _ => false,
            },
            VMStackValue::Float(fl) => match other {
                VMStackValue::Float(fl2) => fl == fl2,
                _ => false,
            },
            VMStackValue::Int(i) => match other {
                VMStackValue::Int(i2) => i == i2,
                _ => false,
            },
        }
    }

    pub fn eq(&self, other: &VMStackValue) -> bool {
        match self {
            VMStackValue::TulispObject(obj1) => match other {
                VMStackValue::TulispObject(obj2) => obj1.eq(obj2),
                _ => false,
            },
            VMStackValue::Bool(b) => match other {
                VMStackValue::Bool(b2) => b == b2,
                _ => false,
            },
            VMStackValue::Float(fl) => match other {
                VMStackValue::Float(fl2) => fl == fl2,
                _ => false,
            },
            VMStackValue::Int(i) => match other {
                VMStackValue::Int(i2) => i == i2,
                _ => false,
            },
        }
    }
}

impl Display for VMStackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMStackValue::TulispObject(obj) => write!(f, "{}", obj),
            VMStackValue::Bool(b) => write!(f, "{}", b),
            VMStackValue::Float(fl) => write!(f, "{}", fl),
            VMStackValue::Int(i) => write!(f, "{}", i),
        }
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
                        InstructionBinaryOp::Add => arith_add(&a, &b)?,
                        InstructionBinaryOp::Sub => arith_sub(&a, &b)?,
                        InstructionBinaryOp::Mul => arith_mul(&a, &b)?,
                        InstructionBinaryOp::Div => arith_div(&a, &b)?,
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
                    let cmp = cmp_lt(a, b)?;
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
                    let cmp = cmp_le(a, b)?;
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
                    let cmp = cmp_gt(a, b)?;
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
                    let cmp = cmp_ge(a, b)?;
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
                    self.stack.push(cmp_lt(&a, &b)?.into());
                }
                Instruction::LtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(cmp_le(&a, &b)?.into());
                }
                Instruction::Gt => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(cmp_gt(&a, &b)?.into());
                }
                Instruction::GtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(cmp_ge(&a, &b)?.into());
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
                            InstructionCxr::Car => a.car().unwrap(),
                            InstructionCxr::Cdr => a.cdr().unwrap(),
                            InstructionCxr::Caar => a.caar().unwrap(),
                            InstructionCxr::Cadr => a.cadr().unwrap(),
                            InstructionCxr::Cdar => a.cdar().unwrap(),
                            InstructionCxr::Cddr => a.cddr().unwrap(),
                            InstructionCxr::Caaar => a.caaar().unwrap(),
                            InstructionCxr::Caadr => a.caadr().unwrap(),
                            InstructionCxr::Cadar => a.cadar().unwrap(),
                            InstructionCxr::Caddr => a.caddr().unwrap(),
                            InstructionCxr::Cdaar => a.cdaar().unwrap(),
                            InstructionCxr::Cdadr => a.cdadr().unwrap(),
                            InstructionCxr::Cddar => a.cddar().unwrap(),
                            InstructionCxr::Cdddr => a.cdddr().unwrap(),
                            InstructionCxr::Caaaar => a.caaaar().unwrap(),
                            InstructionCxr::Caaadr => a.caaadr().unwrap(),
                            InstructionCxr::Caadar => a.caadar().unwrap(),
                            InstructionCxr::Caaddr => a.caaddr().unwrap(),
                            InstructionCxr::Cadaar => a.cadaar().unwrap(),
                            InstructionCxr::Cadadr => a.cadadr().unwrap(),
                            InstructionCxr::Caddar => a.caddar().unwrap(),
                            InstructionCxr::Cadddr => a.cadddr().unwrap(),
                            InstructionCxr::Cdaaar => a.cdaaar().unwrap(),
                            InstructionCxr::Cdaadr => a.cdaadr().unwrap(),
                            InstructionCxr::Cdadar => a.cdadar().unwrap(),
                            InstructionCxr::Cdaddr => a.cdaddr().unwrap(),
                            InstructionCxr::Cddaar => a.cddaar().unwrap(),
                            InstructionCxr::Cddadr => a.cddadr().unwrap(),
                            InstructionCxr::Cdddar => a.cdddar().unwrap(),
                            InstructionCxr::Cddddr => a.cddddr().unwrap(),
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
