use super::{bytecode::Bytecode, Compiler, Instruction};
use crate::{bytecode::Pos, Error, TulispContext, TulispObject};
use std::collections::HashMap;

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

pub struct Machine {
    stack: Vec<TulispObject>,
    bytecode: Bytecode,
    labels: HashMap<usize, usize>, // TulispObject.addr -> instruction index
}

macro_rules! jump_to_pos {
    ($self: ident, $pc:ident, $pos:ident) => {
        $pc = {
            match $pos {
                Pos::Abs(p) => *p,
                Pos::Rel(p) => {
                    let abs_pos = ($pc as isize + *p + 1) as usize;
                    *$pos = Pos::Abs(abs_pos);
                    abs_pos
                }
                Pos::Label(p) => {
                    let abs_pos = *$self.labels.get(&p.addr_as_usize()).unwrap();
                    *$pos = Pos::Abs(abs_pos); // TODO: uncomment
                    abs_pos
                }
            }
        }
    };
}

impl Machine {
    pub(crate) fn new(bytecode: Bytecode) -> Self {
        let labels = Self::locate_labels(&bytecode);
        Machine {
            stack: Vec::new(),
            labels,
            bytecode,
            // program: programs::print_range(92, 100),
            // program: programs::fib(30),
        }
    }

    fn locate_labels(bytecode: &Bytecode) -> HashMap<usize, usize> {
        // TODO: intern-soft and make sure that the labels are unique
        let mut labels = HashMap::new();
        for (i, instr) in bytecode.global.borrow().iter().enumerate() {
            if let Instruction::Label(name) = instr {
                labels.insert(name.addr_as_usize(), i + 1);
            }
        }
        for (_, instr) in &bytecode.functions {
            for (i, instr) in instr.borrow().iter().enumerate() {
                if let Instruction::Label(name) = instr {
                    labels.insert(name.addr_as_usize(), i + 1);
                }
            }
        }
        labels
    }

    #[allow(dead_code)]
    fn print_stack(&self, func: Option<usize>, pc: usize, recursion_depth: u32) {
        println!("Stack:");
        for obj in self.stack.iter() {
            println!("  {}", obj);
        }
        println!(
            "\nDepth: {}: PC: {}; Executing: {}",
            recursion_depth,
            pc,
            if let Some(func) = func {
                self.bytecode.functions.get(&func).unwrap().borrow()[pc].clone()
            } else {
                self.bytecode.global.borrow()[pc].clone()
            }
        );
    }

    pub fn run(&mut self, ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        self.run_impl(ctx, None, 0)?;
        Ok(self.stack.pop().unwrap().into())
    }

    fn run_impl(
        &mut self,
        ctx: &mut TulispContext,
        func: Option<usize>,
        recursion_depth: u32,
    ) -> Result<(), Error> {
        let mut pc: usize = 0;
        let program = if let Some(func) = func {
            self.bytecode.functions.get(&func).unwrap().clone()
        } else {
            self.bytecode.global.clone()
        };
        let program_size = program.borrow().len();
        let mut instr_ref = program.borrow_mut();
        while pc < program_size {
            // self.print_stack(func, pc, recursion_depth);
            let instr = &mut instr_ref[pc];
            match instr {
                Instruction::Push(obj) => self.stack.push(obj.clone()),
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::BinaryOp(op) => {
                    let [ref b, ref a] = self.stack[(self.stack.len() - 2)..] else {
                        unreachable!()
                    };

                    let vv = {
                        use crate::bytecode::instruction::BinaryOp::*;
                        match op {
                            Add => binary_ops!(|a, b| a + b)(a, b)?,
                            Sub => binary_ops!(|a, b| a - b)(a, b)?,
                            Mul => binary_ops!(|a, b| a * b)(a, b)?,
                            Div => binary_ops!(|a, b| a / b)(a, b)?,
                        }
                    };
                    self.stack.truncate(self.stack.len() - 2);
                    self.stack.push(vv);
                }
                Instruction::LoadFile => {
                    let filename = self.stack.pop().unwrap();
                    let filename = filename
                        .as_string()
                        .map_err(|err| err.with_trace(filename))?;
                    let ast = ctx.parse_file(&filename)?;
                    let bytecode = Compiler::new(ctx).compile(&ast)?;
                    // TODO: support global code in modules
                    if bytecode.global.borrow().len() > 0 {
                        return Err(Error::new(
                            crate::ErrorKind::Undefined,
                            "Cannot load a file with global code".to_string(),
                        ));
                    }
                    self.bytecode.import_functions(&bytecode);
                }
                Instruction::PrintPop => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a.fmt_string());
                }
                Instruction::Print => {
                    let a = self.stack.last().unwrap();
                    println!("{}", a.fmt_string());
                }
                Instruction::JumpIfNil(pos) => {
                    let a = self.stack.last().unwrap();
                    let cmp = a.null();
                    self.stack.truncate(self.stack.len() - 1);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfNotNil(pos) => {
                    let a = self.stack.last().unwrap();
                    let cmp = !a.null();
                    self.stack.truncate(self.stack.len() - 1);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfNilElsePop(pos) => {
                    let a = self.stack.last().unwrap();
                    if a.null() {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    } else {
                        self.stack.truncate(self.stack.len() - 1);
                    }
                }
                Instruction::JumpIfNotNilElsePop(pos) => {
                    let a = self.stack.last().unwrap();
                    if !a.null() {
                        jump_to_pos!(self, pc, pos);
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
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLt(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = compare_ops!(|a, b| a < b)(a, b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfLtEq(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = compare_ops!(|a, b| a <= b)(a, b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGt(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = compare_ops!(|a, b| a > b)(a, b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::JumpIfGtEq(pos) => {
                    let minus2 = self.stack.len() - 2;
                    let [ref b, ref a] = self.stack[minus2..] else {
                        unreachable!()
                    };
                    let cmp = compare_ops!(|a, b| a >= b)(a, b)?;
                    self.stack.truncate(minus2);
                    if cmp {
                        jump_to_pos!(self, pc, pos);
                        continue;
                    }
                }
                Instruction::Jump(pos) => {
                    jump_to_pos!(self, pc, pos);
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
                    self.stack.push(compare_ops!(|a, b| a < b)(&a, &b)?.into());
                }
                Instruction::LtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(compare_ops!(|a, b| a <= b)(&a, &b)?.into());
                }
                Instruction::Gt => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(compare_ops!(|a, b| a > b)(&a, &b)?.into());
                }
                Instruction::GtEq => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(compare_ops!(|a, b| a >= b)(&a, &b)?.into());
                }
                Instruction::StorePop(obj) => {
                    let a = self.stack.pop().unwrap();
                    obj.set(a.into()).unwrap();
                }
                Instruction::Store(obj) => {
                    let a = self.stack.last().unwrap();
                    obj.set(a.clone().into()).unwrap();
                }
                Instruction::Load(obj) => {
                    let a = obj.get()?;
                    self.stack.push(a.into());
                }
                Instruction::BeginScope(obj) => {
                    let a = self.stack.last().unwrap();
                    obj.set_scope(a.clone().into()).unwrap();
                    self.stack.truncate(self.stack.len() - 1);
                }
                Instruction::EndScope(obj) => {
                    obj.unset().unwrap();
                }
                Instruction::Call { addr, args } => {
                    let addr = *addr;
                    let func = Some(addr);
                    if args.is_none() {
                        if let Some(items) = self.bytecode.defun_args.get(&addr) {
                            *args = Some(items.clone());
                        } else {
                            return Err(Error::new(
                                crate::ErrorKind::Undefined,
                                // TODO: err name with span instead of addr
                                format!("undefined function: {}", addr),
                            ));
                        }
                    }

                    if let Some(args) = args {
                        for arg in args.iter() {
                            arg.set_scope(self.stack.pop().unwrap()).unwrap();
                        }
                    }

                    drop(instr_ref);
                    self.run_impl(ctx, func, recursion_depth + 1)?;
                    instr_ref = program.borrow_mut();
                    if let Some(args) = self.bytecode.defun_args.get(&addr) {
                        for arg in args.iter() {
                            arg.unset().unwrap();
                        }
                    }
                }
                Instruction::Ret => return Ok(()),
                Instruction::RustCall { func } => {
                    let args = self.stack.pop().unwrap();
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
                        list.append(elt.deep_copy().unwrap())?;
                    }
                    self.stack.push(list.into());
                }
                Instruction::Cxr(cxr) => {
                    let a: TulispObject = self.stack.pop().unwrap().into();

                    self.stack.push({
                        use crate::bytecode::instruction::Cxr::*;
                        match cxr {
                            Car => a.car().unwrap(),
                            Cdr => a.cdr().unwrap(),
                            Caar => a.caar().unwrap(),
                            Cadr => a.cadr().unwrap(),
                            Cdar => a.cdar().unwrap(),
                            Cddr => a.cddr().unwrap(),
                            Caaar => a.caaar().unwrap(),
                            Caadr => a.caadr().unwrap(),
                            Cadar => a.cadar().unwrap(),
                            Caddr => a.caddr().unwrap(),
                            Cdaar => a.cdaar().unwrap(),
                            Cdadr => a.cdadr().unwrap(),
                            Cddar => a.cddar().unwrap(),
                            Cdddr => a.cdddr().unwrap(),
                            Caaaar => a.caaaar().unwrap(),
                            Caaadr => a.caaadr().unwrap(),
                            Caadar => a.caadar().unwrap(),
                            Caaddr => a.caaddr().unwrap(),
                            Cadaar => a.cadaar().unwrap(),
                            Cadadr => a.cadadr().unwrap(),
                            Caddar => a.caddar().unwrap(),
                            Cadddr => a.cadddr().unwrap(),
                            Cdaaar => a.cdaaar().unwrap(),
                            Cdaadr => a.cdaadr().unwrap(),
                            Cdadar => a.cdadar().unwrap(),
                            Cdaddr => a.cdaddr().unwrap(),
                            Cddaar => a.cddaar().unwrap(),
                            Cddadr => a.cddadr().unwrap(),
                            Cdddar => a.cdddar().unwrap(),
                            Cddddr => a.cddddr().unwrap(),
                        }
                    })
                }
                // predicates
                Instruction::Null => {
                    let a = self.stack.last().unwrap().null();
                    *self.stack.last_mut().unwrap() = a.into();
                }
            }
            pc += 1;
        }
        Ok(())
    }
}
