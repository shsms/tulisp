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
    other: HashMap<usize, Pos>, // TulispObject.addr() -> Pos
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
    keep_result: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(ctx: &'a mut TulispContext) -> Self {
        let functions = VMFunctions::from(ctx);
        Compiler {
            ctx,
            functions,
            keep_result: true,
        }
    }

    pub fn compile(&mut self, value: &TulispObject) -> Result<Vec<Instruction>, Error> {
        let mut result = vec![];
        let mut prev = None;
        let keep_result = self.keep_result;
        for expr in value.base_iter() {
            if let Some(prev) = prev {
                self.keep_result = false;
                result.append(&mut self.compile_expr(&prev)?);
            }
            prev = Some(expr);
        }
        self.keep_result = keep_result;
        if let Some(prev) = prev {
            result.append(&mut self.compile_expr(&prev)?);
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
            | TulispValue::T => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(expr.clone())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::List { cons, .. } => self
                .compile_form(cons)
                .map_err(|e| e.with_trace(expr.clone())),
            TulispValue::Symbol { .. } => Ok(vec![Instruction::Load(expr.clone())]),
        }
    }

    fn compile_expr_keep_result(&mut self, expr: &TulispObject) -> Result<Vec<Instruction>, Error> {
        let keep_result = self.keep_result;
        self.keep_result = true;
        let ret = self.compile_expr(expr);
        self.keep_result = keep_result;
        ret
    }

    fn compile_1_arg_call(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        lambda: fn(&mut Compiler, &TulispObject, &TulispObject) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        if args.null() {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                if has_rest {
                    format!("{} requires at least 1 argument.", name)
                } else {
                    format!("{} requires 1 argument.", name)
                },
            ));
        }
        args.car_and_then(|arg1| {
            args.cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::new(
                        ErrorKind::ArityMismatch,
                        format!("{} accepts only 1 argument.", name),
                    ));
                }
                lambda(self, arg1, rest)
            })
        })
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
                if has_rest {
                    format!("{} requires at least 2 arguments.", name)
                } else {
                    format!("{} requires 2 arguments.", name)
                },
            ));
        };
        if args.cdr().null() {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                if has_rest {
                    format!("{} requires at least 2 arguments.", name)
                } else {
                    format!("{} requires 2 arguments.", name)
                },
            ));
        }
        let arg1 = args.car();
        args.cdr().car_and_then(|arg2| {
            args.cdr().cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::new(
                        ErrorKind::ArityMismatch,
                        format!("{} accepts only 2 arguments.", name),
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
            self.compile_fn_print(name, args)
        } else if name.eq(&self.functions.setq) {
            self.compile_fn_setq(name, args)
        } else if name.eq(&self.functions.le) {
            self.compile_fn_le(name, args)
        } else if name.eq(&self.functions.plus) {
            self.compile_fn_plus(name, args)
        } else if name.eq(&self.functions.if_) {
            self.compile_fn_if(name, args)
        } else if name.eq(&self.functions.progn) {
            self.compile_fn_progn(name, args)
        } else {
            Err(Error::new(
                ErrorKind::Undefined,
                format!("undefined function: {}", name),
            ))
        }
    }
}

/// Compilers for specific lisp functions.
impl Compiler<'_> {
    fn compile_fn_print(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        self.compile_1_arg_call(name, args, false, |compiler, arg, _| {
            let mut result = compiler.compile_expr_keep_result(arg)?;
            if compiler.keep_result {
                result.push(Instruction::Print);
            } else {
                result.push(Instruction::PrintPop);
            }
            Ok(result)
        })
    }

    fn compile_fn_setq(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        self.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
            let mut result = compiler.compile_expr_keep_result(arg2)?;
            if compiler.keep_result {
                result.push(Instruction::Store(arg1.clone()));
            } else {
                result.push(Instruction::StorePop(arg1.clone()));
            }
            Ok(result)
        })
    }

    fn compile_fn_le(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        self.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
            if !compiler.keep_result {
                Ok(vec![])
            } else {
                let mut result = compiler.compile_expr(arg2)?;
                result.append(&mut compiler.compile_expr(arg1)?);
                result.push(Instruction::LtEq);
                Ok(result)
            }
        })
    }

    fn compile_fn_plus(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        self.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
            if !compiler.keep_result {
                Ok(vec![])
            } else {
                let mut result = compiler.compile_expr(arg2)?;
                result.append(&mut compiler.compile_expr(arg1)?);
                result.push(Instruction::Add);
                Ok(result)
            }
        })
    }

    fn compile_fn_if(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        self.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
            let mut result = ctx.compile_expr_keep_result(cond)?;
            let mut then = ctx.compile_expr(then)?;
            let mut else_ = ctx.compile(else_)?;
            result.push(Instruction::JumpIfNil(Pos::Rel(then.len() as isize + 2)));
            result.append(&mut then);
            result.push(Instruction::Jump(Pos::Rel(else_.len() as isize + 1)));
            result.append(&mut else_);
            Ok(result)
        })
    }

    fn compile_fn_progn(
        &mut self,
        _name: &TulispObject,
        args: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        Ok(self.compile(args)?)
    }
}
