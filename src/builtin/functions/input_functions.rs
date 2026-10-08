//! Emacs's `read`, which reads text as a Lisp value.

use crate::{Error, TulispContext, TulispObject, parse::read_one};

pub(crate) fn add(ctx: &mut TulispContext) {
    // Tulisp has no buffers or input streams, so STREAM is a string.
    ctx.defun(
        "read",
        |ctx: &mut TulispContext, stream: TulispObject| -> Result<TulispObject, Error> {
            Ok(stream.with_str(|text| read_one(ctx, text))??.0)
        },
    );
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_results;

    #[test]
    fn read_reads_the_first_value() {
        assert_results(&[
            (r#"(read "(a b)")"#, "(a b)"),
            (r#"(read "  1 2")"#, "1"),
            (r#"(read "(a . b)")"#, "(a . b)"),
            (r##"(read "#'f")"##, "#'f"),
            (r#"(read "?a")"#, "97"),
            (r#"(read "\"x\"")"#, r#""x""#),
            // The text is data: nothing in it runs.
            (r#"(read "(setq x 1)")"#, "(setq x 1)"),
            (r#"(read "")"#, "(ERR (end-of-file))"),
            (r#"(read "  ; c")"#, "(ERR (end-of-file))"),
            (r#"(read "(a")"#, "(ERR (end-of-file))"),
            (r#"(read "(a .")"#, "(ERR (end-of-file))"),
            (r#"(read "(a . b")"#, "(ERR (end-of-file))"),
            ("(read \"(a . b ;x\")", "(ERR (end-of-file))"),
            (r##"(read "#'")"##, "(ERR (end-of-file))"),
            (r#"(read "`")"#, "(ERR (end-of-file))"),
            (r#"(read ", ")"#, "(ERR (end-of-file))"),
            (r#"(read ",@")"#, "(ERR (end-of-file))"),
            (r#"(read "\"a\\")"#, "(ERR (end-of-file))"),
            (r#"(read "'")"#, "(ERR (end-of-file))"),
            (r#"(read ",")"#, "(ERR (end-of-file))"),
            (r#"(read "?")"#, "(ERR (end-of-file))"),
            (r#"(read "?\\")"#, "(ERR (end-of-file))"),
            (r#"(read "\"ab")"#, "(ERR (end-of-file))"),
            (r#"(read 5)"#, "(ERR (wrong-type-argument stringp 5))"),
        ]);
    }

    #[test]
    fn a_bad_token_is_invalid_read_syntax() {
        assert_results(&[
            (
                r#"(car (condition-case e (read ")") (error e)))"#,
                "invalid-read-syntax",
            ),
            (
                r#"(car (condition-case e (read ". a") (error e)))"#,
                "invalid-read-syntax",
            ),
            (
                r##"(car (condition-case e (read "#") (error e)))"##,
                "invalid-read-syntax",
            ),
            (
                r#"(car (condition-case e (read "(a . b c)") (error e)))"#,
                "invalid-read-syntax",
            ),
        ]);
    }
}
