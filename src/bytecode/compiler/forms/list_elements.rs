use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr, instruction::Cxr},
};

pub(super) fn compile_fn_cxr(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result =
        ctx.compile_1_arg_call(name, args, false, |ctx, arg1, _| compile_expr(ctx, arg1))?;
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        let name = name.to_string();
        match name.as_str() {
            "car" => result.push(Instruction::Cxr(Cxr::Car)),
            "cdr" => result.push(Instruction::Cxr(Cxr::Cdr)),
            "caar" => result.push(Instruction::Cxr(Cxr::Caar)),
            "cadr" => result.push(Instruction::Cxr(Cxr::Cadr)),
            "cdar" => result.push(Instruction::Cxr(Cxr::Cdar)),
            "cddr" => result.push(Instruction::Cxr(Cxr::Cddr)),
            "caaar" => result.push(Instruction::Cxr(Cxr::Caaar)),
            "caadr" => result.push(Instruction::Cxr(Cxr::Caadr)),
            "cadar" => result.push(Instruction::Cxr(Cxr::Cadar)),
            "caddr" => result.push(Instruction::Cxr(Cxr::Caddr)),
            "cdaar" => result.push(Instruction::Cxr(Cxr::Cdaar)),
            "cdadr" => result.push(Instruction::Cxr(Cxr::Cdadr)),
            "cddar" => result.push(Instruction::Cxr(Cxr::Cddar)),
            "cdddr" => result.push(Instruction::Cxr(Cxr::Cdddr)),
            "caaaar" => result.push(Instruction::Cxr(Cxr::Caaaar)),
            "caaadr" => result.push(Instruction::Cxr(Cxr::Caaadr)),
            "caadar" => result.push(Instruction::Cxr(Cxr::Caadar)),
            "caaddr" => result.push(Instruction::Cxr(Cxr::Caaddr)),
            "cadaar" => result.push(Instruction::Cxr(Cxr::Cadaar)),
            "cadadr" => result.push(Instruction::Cxr(Cxr::Cadadr)),
            "caddar" => result.push(Instruction::Cxr(Cxr::Caddar)),
            "cadddr" => result.push(Instruction::Cxr(Cxr::Cadddr)),
            "cdaaar" => result.push(Instruction::Cxr(Cxr::Cdaaar)),
            "cdaadr" => result.push(Instruction::Cxr(Cxr::Cdaadr)),
            "cdadar" => result.push(Instruction::Cxr(Cxr::Cdadar)),
            "cdaddr" => result.push(Instruction::Cxr(Cxr::Cdaddr)),
            "cddaar" => result.push(Instruction::Cxr(Cxr::Cddaar)),
            "cddadr" => result.push(Instruction::Cxr(Cxr::Cddadr)),
            "cdddar" => result.push(Instruction::Cxr(Cxr::Cdddar)),
            "cddddr" => result.push(Instruction::Cxr(Cxr::Cddddr)),
            _ => return Err(Error::new(ErrorKind::Undefined, "unknown cxr".to_string())),
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, test_utils::eval_assert_error};

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
