use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr},
};

pub(super) fn compile_fn_plist_get(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    // With a PREDICATE, it is a plain call to the function.
    if crate::lists::length(args)? == 3 {
        return super::other_functions::compile_fn_defun_call(ctx, name, args);
    }
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

    // `plist-get` takes Emacs's optional PREDICATE, called with the key in the
    // plist and PROPERTY; nil means `eq`.
    #[test]
    fn plist_get_takes_a_predicate() {
        let cases = [
            (r#"(plist-get '("a" 1 "b" 2) "b" #'equal)"#, "2"),
            (r#"(plist-get '("a" 1 "b" 2) "b")"#, "nil"),
            ("(plist-get '(a 1 b 2) 'b nil)", "2"),
            (
                "(list (plist-get '(1 a 3 b) 2 #'<) (plist-get '(3 a 1 b) 2 #'<))",
                "'(a b)",
            ),
            (r#"(funcall 'plist-get '("a" 1) "a" 'equal)"#, "1"),
        ];
        for (program, expected) in cases {
            eval_assert_equal_fresh(program, expected);
        }
    }

    // With a PREDICATE, a malformed plist is read up to where it breaks, as in
    // Emacs: a key with no value is not passed to PREDICATE, and a plist that
    // loops back stops with nil.
    #[test]
    fn plist_get_with_a_predicate_takes_a_malformed_plist() {
        let cases = [
            ("(plist-get '(a 1 b . 2) 'b #'eq)", "nil"),
            ("(plist-get '(a . 1) 'a #'eq)", "nil"),
            ("(plist-get '(a (1) . x) 'a #'equal)", "'(1)"),
            (
                "(let ((n 0)) (plist-get '(a 1 b) 'b (lambda (k p) (setq n (1+ n)) nil)) n)",
                "1",
            ),
            (
                "(let ((l (list 'a 1 'b 2))) (setcdr (cdddr l) l) (list (plist-get l 'c #'eq) (plist-get l 'b #'eq)))",
                "'(nil 2)",
            ),
        ];
        for (program, expected) in cases {
            eval_assert_equal_fresh(program, expected);
        }
    }
}
