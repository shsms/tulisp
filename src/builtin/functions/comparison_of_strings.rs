use crate::TulispContext;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("string<", |a: String, b: String| -> bool { a < b });
    ctx.defun("string>", |a: String, b: String| -> bool { a > b });
    ctx.defun("string=", |a: String, b: String| -> bool { a == b });
    ctx.defun("string-lessp", |a: String, b: String| -> bool { a < b });
    ctx.defun("string-greaterp", |a: String, b: String| -> bool { a > b });
    ctx.defun("string-equal", |a: String, b: String| -> bool { a == b });
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{eval_assert, eval_assert_not},
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
}
