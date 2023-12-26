use std::collections::HashMap;

use crate::{
    vm::{Instruction, Pos},
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
};

#[allow(dead_code)]
struct VMFunctions {
    defun: TulispObject,
    progn: TulispObject,
    le: TulispObject,
    plus: TulispObject,
    if_: TulispObject,
    print: TulispObject,
    setq: TulispObject,
    other: HashMap<TulispObject, Pos>,
}

impl VMFunctions {
    fn from(value: &mut TulispContext) -> Self {
        VMFunctions {
            defun: value.intern("defun"),
            progn: value.intern("progn"),
            le: value.intern("<="),
            plus: value.intern("+"),
            if_: value.intern("if"),
            print: value.intern("print"),
            setq: value.intern("setq"),
            other: HashMap::new(),
        }
    }
}

#[allow(dead_code)]
pub(crate) struct Compiler<'a> {
    ctx: &'a mut TulispContext,
    functions: VMFunctions,
}

impl<'a> Compiler<'a> {
    pub fn new(ctx: &'a mut TulispContext) -> Self {
        let functions = VMFunctions::from(ctx);
        Compiler { ctx, functions }
    }

    pub fn compile(&mut self, value: &TulispObject) -> Result<Vec<Instruction>, Error> {
        let mut result = vec![];
        for expr in value.base_iter() {
            result.append(&mut self.compile_expr(&expr)?);
        }
        Ok(result)
    }

    fn compile_expr(&mut self, expr: &TulispObject) -> Result<Vec<Instruction>, Error> {
        match &*expr.inner_ref() {
            TulispValue::Int { .. }
            | TulispValue::Float { .. }
            | TulispValue::String { .. }
            | TulispValue::Lambda { .. }
            | TulispValue::Func(_)
            | TulispValue::Macro(_)
            | TulispValue::Defmacro { .. }
            | TulispValue::Any(_)
            | TulispValue::Bounce
            | TulispValue::Nil
            | TulispValue::Quote { .. }
            | TulispValue::Sharpquote { .. }
            | TulispValue::Backquote { .. }
            | TulispValue::Unquote { .. }
            | TulispValue::Splice { .. }
            | TulispValue::T => return Ok(vec![Instruction::Push(expr.clone())]),
            TulispValue::List { cons, .. } => self
                .compile_form(cons)
                .map_err(|e| e.with_trace(expr.clone())),
            TulispValue::Symbol { .. } => Ok(vec![Instruction::Load(expr.clone())]),
        }
    }

    fn compile_1_arg_call(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
        lambda: fn(&mut Compiler, &TulispObject) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        match args.cdr_and_then(|x| Ok(x.null())) {
            Err(e) => return Err(e),
            Ok(false) => {
                return Err(Error::new(
                    ErrorKind::ArityMismatch,
                    format!("{} takes 1 argument.", name),
                ))
            }
            Ok(true) => {}
        }
        args.car_and_then(|x| lambda(self, x))
    }

    fn compile_2_arg_call(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        lambda: fn(
            &mut Compiler,
            &TulispObject,
            &TulispObject,
            &TulispObject,
        ) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        let TulispValue::List { cons: args, .. } = &*args.inner_ref() else {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                format!("{} takes 2 arguments.", name),
            ));
        };
        if args.cdr().null() {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                format!("{} takes 2 arguments.", name),
            ));
        }
        let arg1 = args.car();
        args.cdr().car_and_then(|arg2| {
            args.cdr().cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::new(
                        ErrorKind::ArityMismatch,
                        format!("{} takes 2 arguments.", name),
                    ));
                }
                lambda(self, arg1, arg2, rest)
            })
        })
    }

    fn compile_form(&mut self, cons: &crate::cons::Cons) -> Result<Vec<Instruction>, Error> {
        let name = cons.car();
        let args = cons.cdr();
        if name.eq(&self.functions.print) {
            self.compile_1_arg_call(name, args, |ctx, arg| {
                let mut result = ctx.compile_expr(arg)?;
                result.push(Instruction::Print);
                Ok(result)
            })
        } else if name.eq(&self.functions.setq) {
            self.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
                let mut result = ctx.compile_expr(arg2)?;
                result.push(Instruction::Store(arg1.clone()));
                Ok(result)
            })
        } else if name.eq(&self.functions.le) {
            self.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
                let mut result = ctx.compile_expr(arg2)?;
                result.append(&mut ctx.compile_expr(arg1)?);
                result.push(Instruction::LtEq);
                Ok(result)
            })
        } else if name.eq(&self.functions.plus) {
            self.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
                let mut result = ctx.compile_expr(arg2)?;
                result.append(&mut ctx.compile_expr(arg1)?);
                result.push(Instruction::Add);
                Ok(result)
            })
        } else if name.eq(&self.functions.if_) {
            self.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
                let mut result = ctx.compile_expr(cond)?;
                let mut then = ctx.compile_expr(then)?;
                let mut else_ = ctx.compile(else_)?;
                result.push(Instruction::JumpIfNil(Pos::Rel(then.len() as isize + 2)));
                result.append(&mut then);
                result.push(Instruction::Jump(Pos::Rel(else_.len() as isize + 1)));
                result.append(&mut else_);
                Ok(result)
            })
        } else if name.eq(&self.functions.progn) {
            Ok(self.compile(args)?)
        } else {
            Err(Error::new(
                ErrorKind::Undefined,
                format!("undefined function: {}", name),
            ))
        }
    }
}
