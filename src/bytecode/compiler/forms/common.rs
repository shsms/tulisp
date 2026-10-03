use crate::{
    Error, TulispContext, TulispObject, TulispValue,
    bytecode::{Instruction, compiler::compiler::compile_expr_keep_result},
};

/// CODE, followed by a `Pop` when the form's value is unused.
pub(super) fn pop_unless_kept(ctx: &TulispContext, mut code: Vec<Instruction>) -> Vec<Instruction> {
    if !ctx.compiler.as_ref().unwrap().keep_result {
        code.push(Instruction::Pop);
    }
    code
}

/// ARGS in order, each value kept, then OP, and a `Pop` when the form's
/// value is unused. Like a function call, OP runs even then, so its
/// errors are raised.
pub(super) fn compile_args_then(
    ctx: &mut TulispContext,
    args: &[TulispObject],
    op: Instruction,
) -> Result<Vec<Instruction>, Error> {
    let mut code = vec![];
    for arg in args {
        code.append(&mut compile_expr_keep_result(ctx, arg)?);
    }
    code.push(op);
    Ok(pop_unless_kept(ctx, code))
}

impl TulispContext {
    pub(crate) fn compile_1_arg_call(
        &mut self,
        _name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        mut lambda: impl FnMut(
            &mut TulispContext,
            &TulispObject,
            &TulispObject,
        ) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        if args.null() {
            return Err(Error::too_few_arguments());
        }
        args.car_and_then(|arg1| {
            args.cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::too_many_arguments());
                }
                lambda(self, arg1, rest)
            })
        })
    }

    pub(crate) fn compile_2_arg_call(
        &mut self,
        _name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        mut lambda: impl FnMut(
            &mut TulispContext,
            &TulispObject,
            &TulispObject,
            &TulispObject,
        ) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        let (TulispValue::List { cons: args, .. }, _) = &*args.inner_ref() else {
            return Err(Error::too_few_arguments());
        };
        if args.cdr().null() {
            return Err(Error::too_few_arguments());
        }
        let arg1 = args.car();
        args.cdr().car_and_then(|arg2| {
            args.cdr().cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::too_many_arguments());
                }
                lambda(self, arg1, arg2, rest)
            })
        })
    }
}
