use crate::{bytecode::Instruction, Error, ErrorKind, TulispContext, TulispObject, TulispValue};

impl TulispContext {
    pub(crate) fn compile_1_arg_call(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        lambda: fn(
            &mut TulispContext,
            &TulispObject,
            &TulispObject,
        ) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        if args.null() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,  // TODO: change to ArityMismatch
                if has_rest {
                    format!("{} requires at least 1 argument", name)
                } else {
                    format!("{} requires exactly 1 argument", name)
                },
            ));
        }
        args.car_and_then(|arg1| {
            args.cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,  // TODO: change to ArityMismatch
                        format!("{} requires exactly 1 argument", name),
                    ));
                }
                lambda(self, arg1, rest)
            })
        })
    }

    pub(crate) fn compile_2_arg_call(
        &mut self,
        name: &TulispObject,
        args: &TulispObject,
        has_rest: bool,
        lambda: fn(
            &mut TulispContext,
            &TulispObject,
            &TulispObject,
            &TulispObject,
        ) -> Result<Vec<Instruction>, Error>,
    ) -> Result<Vec<Instruction>, Error> {
        let TulispValue::List { cons: args, .. } = &*args.inner_ref() else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,  // TODO: change to ArityMismatch
                if has_rest {
                    format!("{} requires at least 2 arguments", name)
                } else {
                    format!("{} requires exactly 2 arguments", name)
                },
            ));
        };
        if args.cdr().null() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,  // TODO: change to ArityMismatch
                if has_rest {
                    format!("{} requires at least 2 arguments", name)
                } else {
                    format!("{} requires exactly 2 arguments", name)
                },
            ));
        }
        let arg1 = args.car();
        args.cdr().car_and_then(|arg2| {
            args.cdr().cdr_and_then(|rest| {
                if !has_rest && !rest.null() {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,  // TODO: change to ArityMismatch
                        format!("{} requires exactly 2 arguments", name),
                    ));
                }
                lambda(self, arg1, arg2, rest)
            })
        })
    }
}
