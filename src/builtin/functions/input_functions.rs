//! Emacs's `read` and `read-from-string`, which read text as a Lisp value.

use super::strings::{char_span, or_nil};
use crate::{Error, TulispContext, TulispObject, parse::read_one};

pub(crate) fn add(ctx: &mut TulispContext) {
    // Tulisp has no buffers or input streams, so STREAM is a string.
    ctx.defun(
        "read",
        |ctx: &mut TulispContext, stream: TulispObject| -> Result<TulispObject, Error> {
            Ok(stream.with_str(|text| read_one(ctx, text))??.0)
        },
    );

    // START and END count characters as `substring` does, and so does the index
    // of the end of the value read.
    ctx.defun(
        "read-from-string",
        |ctx: &mut TulispContext,
         string: TulispObject,
         start: Option<i64>,
         end: Option<i64>|
         -> Result<TulispObject, Error> {
            let read = string.with_str(|text| {
                let span = char_span(text, start, end)?;
                Some(
                    read_one(ctx, &text[span.clone()])
                        .map(|(value, used)| (value, text[..span.start + used].chars().count())),
                )
            })?;
            let Some(read) = read else {
                return Err(Error::out_of_range(format!(
                    "{string}, {}, {}",
                    or_nil(start),
                    or_nil(end)
                )));
            };
            let (value, index) = read?;
            // A string's length in characters fits an `i64`.
            let index = i64::try_from(index).unwrap_or(i64::MAX);
            Ok(TulispObject::cons(value, TulispObject::from(index)))
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
    fn read_from_string_gives_where_the_value_ends() {
        assert_results(&[
            (r#"(read-from-string "(a b) c")"#, "((a b) . 5)"),
            (r#"(read-from-string "abc def" 4)"#, "(def . 7)"),
            (r#"(read-from-string "abc def" -3)"#, "(def . 7)"),
            (r#"(read-from-string "abc def" 0 2)"#, "(ab . 2)"),
            (r#"(read-from-string "1 ")"#, "(1 . 1)"),
            (r#"(read-from-string "a)")"#, "(a . 1)"),
            (r#"(read-from-string "é b")"#, "(é . 1)"),
            ("(read-from-string \"; c\n x\")", "(x . 6)"),
            (r#"(read-from-string "x;c")"#, "(x . 1)"),
            (r#"(read-from-string "\"x\"y")"#, r#"("x" . 3)"#),
            (r##"(read-from-string "#'f")"##, "(#'f . 3)"),
            (r#"(read-from-string "")"#, "(ERR (end-of-file))"),
            (
                r#"(read-from-string "abc" 5)"#,
                r#"(ERR (args-out-of-range "\"abc\", 5, nil"))"#,
            ),
            (
                r#"(read-from-string "abc" 2 1)"#,
                r#"(ERR (args-out-of-range "\"abc\", 2, 1"))"#,
            ),
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
