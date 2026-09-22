use crate::{Error, TulispContext, TulispObject, TulispValue, bytecode::Instruction};

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
