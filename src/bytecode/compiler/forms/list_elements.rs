use super::common::compile_args_then;
use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{Instruction, instruction::Cxr},
};

pub(super) fn compile_fn_cxr(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let cxr = match name.to_string().as_str() {
        "car" => Cxr::Car,
        "cdr" => Cxr::Cdr,
        "caar" => Cxr::Caar,
        "cadr" => Cxr::Cadr,
        "cdar" => Cxr::Cdar,
        "cddr" => Cxr::Cddr,
        "caaar" => Cxr::Caaar,
        "caadr" => Cxr::Caadr,
        "cadar" => Cxr::Cadar,
        "caddr" => Cxr::Caddr,
        "cdaar" => Cxr::Cdaar,
        "cdadr" => Cxr::Cdadr,
        "cddar" => Cxr::Cddar,
        "cdddr" => Cxr::Cdddr,
        "caaaar" => Cxr::Caaaar,
        "caaadr" => Cxr::Caaadr,
        "caadar" => Cxr::Caadar,
        "caaddr" => Cxr::Caaddr,
        "cadaar" => Cxr::Cadaar,
        "cadadr" => Cxr::Cadadr,
        "caddar" => Cxr::Caddar,
        "cadddr" => Cxr::Cadddr,
        "cdaaar" => Cxr::Cdaaar,
        "cdaadr" => Cxr::Cdaadr,
        "cdadar" => Cxr::Cdadar,
        "cdaddr" => Cxr::Cdaddr,
        "cddaar" => Cxr::Cddaar,
        "cddadr" => Cxr::Cddadr,
        "cdddar" => Cxr::Cdddar,
        "cddddr" => Cxr::Cddddr,
        _ => return Err(Error::new(ErrorKind::Undefined, "unknown cxr".to_string())),
    };
    ctx.compile_1_arg_call(name, args, false, |ctx, arg1, _| {
        compile_args_then(ctx, std::slice::from_ref(arg1), Instruction::Cxr(cxr))
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{eval_assert_error, eval_assert_error_line},
    };

    // A cxr runs even when its value is not kept, as in Emacs 30.1.
    #[test]
    fn a_discarded_cxr_still_runs() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(progn (car 5) 2)",
            "ERR TypeMismatch: Expected list, got: 5",
        );
        eval_assert_error_line(
            ctx,
            "(progn (cadr 7) 2)",
            "ERR TypeMismatch: Expected list, got: 7",
        );
    }

    // Every cxr raises `TypeMismatch` on a non-cons argument.
    #[test]
    fn a_cxr_of_a_non_list_is_a_type_error() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(car 5)",
            "ERR TypeMismatch: Expected list, got: 5\n<eval_string>:1.1-1.7:  at (car 5)\n",
        );
        eval_assert_error(
            ctx,
            "(cdr 5)",
            "ERR TypeMismatch: Expected list, got: 5\n<eval_string>:1.1-1.7:  at (cdr 5)\n",
        );
        eval_assert_error(
            ctx,
            "(cadr 7)",
            "ERR TypeMismatch: Expected list, got: 7\n<eval_string>:1.1-1.8:  at (cadr 7)\n",
        );
        eval_assert_error(
            ctx,
            "(cdddr \"abc\")",
            "ERR TypeMismatch: Expected list, got: \"abc\"\n<eval_string>:1.1-1.13:  at (cdddr \"abc\")\n",
        );
    }
}
