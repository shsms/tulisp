use std::rc::Rc;

use crate::{eval::eval_and_then, Error, ErrorKind, TulispContext, TulispObject, TulispValue};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn fround(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        match args.cdr_and_then(|x| Ok(x.null())) {
            Err(err) => return Err(err),
            Ok(false) => {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected exatly 1 argument for fround. Got args: {}", args),
                )
                .with_span(args.span()));
            }
            Ok(true) => {}
        }
        args.car_and_then(|arg| {
            eval_and_then(ctx, &arg, |x| {
                if x.floatp() {
                    Ok(f64::round(x.as_float().unwrap()).into())
                } else {
                    Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected float for fround. Got: {}", x),
                    )
                    .with_span(x.span()))
                }
            })
        })
    }
    intern_set_func!(ctx, fround);
}
