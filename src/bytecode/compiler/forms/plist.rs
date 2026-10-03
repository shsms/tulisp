use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr},
};

pub(super) fn compile_fn_plist_get(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, plist, property, _| {
        // `plist-get` raises no error in Emacs, so when its value is not
        // kept only its arguments run.
        let mut result = compile_expr(ctx, plist)?;
        result.append(&mut compile_expr(ctx, property)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::PlistGet);
        }
        Ok(result)
    })
}

#[cfg(test)]
mod tests {
    use crate::test_utils::eval_assert_equal_fresh;

    // `plist-get` evaluates PLIST before PROPERTY, as in Emacs 30.1.
    // When its value is not kept, only its arguments run: a malformed
    // plist, where tulisp raises and Emacs does not, raises nothing.
    #[test]
    fn plist_get_evaluates_its_arguments_in_order() {
        eval_assert_equal_fresh(
            "(let ((seen nil))
               (list (plist-get (progn (push 1 seen) '(a 1)) (progn (push 2 seen) 'a))
                     (reverse seen)))",
            "'(1 (1 2))",
        );
        eval_assert_equal_fresh(
            "(let ((seen nil))
               (plist-get (progn (push 1 seen) '(a 1)) (progn (push 2 seen) 'a))
               (reverse seen))",
            "'(1 2)",
        );
        eval_assert_equal_fresh("(progn (plist-get '(a . b) 'c) 2)", "2");
    }
}
