use crate::{Error, TulispContext, TulispObject};

/// The text OBJ compares by: a string's text or a symbol's name.
fn text(obj: &TulispObject) -> Result<String, Error> {
    if obj.symbolp() {
        obj.symbol_name()
    } else {
        obj.as_string()
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    let less = |a: TulispObject, b: TulispObject| Ok::<_, Error>(text(&a)? < text(&b)?);
    let greater = |a: TulispObject, b: TulispObject| Ok::<_, Error>(text(&a)? > text(&b)?);
    let equal = |a: TulispObject, b: TulispObject| Ok::<_, Error>(text(&a)? == text(&b)?);
    ctx.defun("string<", less);
    ctx.defun("string>", greater);
    ctx.defun("string=", equal);
    ctx.defun("string-lessp", less);
    ctx.defun("string-greaterp", greater);
    ctx.defun("string-equal", equal);
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{eval_assert, eval_assert_error_line, eval_assert_not},
    };

    #[test]
    fn test_string_comparison() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, r#"(string< "hello" "world")"#);
        eval_assert(ctx, r#"(string> "world" "hello")"#);
        eval_assert(ctx, r#"(string= "hello" "hello")"#);
        eval_assert(ctx, r#"(string-lessp "hello" "world")"#);
        eval_assert(ctx, r#"(string-greaterp "world" "hello")"#);
        eval_assert(ctx, r#"(string-equal "hello" "hello")"#);

        eval_assert_not(ctx, r#"(string< "hello" "hello")"#);
        eval_assert_not(ctx, r#"(string< "world" "hello")"#);
        eval_assert_not(ctx, r#"(string> "hello" "world")"#);
        eval_assert_not(ctx, r#"(string> "hello" "hello")"#);
        eval_assert_not(ctx, r#"(string= "hello" "world")"#);
        eval_assert_not(ctx, r#"(string= "world" "hello")"#);
        eval_assert_not(ctx, r#"(string-lessp "hello" "hello")"#);
        eval_assert_not(ctx, r#"(string-lessp "world" "hello")"#);
        eval_assert_not(ctx, r#"(string-greaterp "hello" "world")"#);
        eval_assert_not(ctx, r#"(string-greaterp "hello" "hello")"#);
        eval_assert_not(ctx, r#"(string-equal "hello" "world")"#);
        eval_assert_not(ctx, r#"(string-equal "world" "hello")"#);
    }

    // A symbol compares by its name, as in Emacs; nil is "nil".
    #[test]
    fn a_symbol_compares_by_its_name() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, r#"(string= 'abc "abc")"#);
        eval_assert(ctx, "(string< 'abc 'abd)");
        eval_assert(ctx, r#"(string> 'b "a")"#);
        eval_assert(ctx, r#"(string-equal "a" 'a)"#);
        eval_assert(ctx, r#"(string-lessp 'a "b")"#);
        eval_assert(ctx, r#"(string-greaterp "b" 'a)"#);
        eval_assert(ctx, r#"(string= nil "nil")"#);
        eval_assert_error_line(
            ctx,
            r#"(string= 1 "1")"#,
            "ERR TypeMismatch: Expected string, got: 1",
        );
    }
}
