//! Functions on strings and characters.
//!
//! The string a function works on is borrowed, not copied; a short argument
//! such as a prefix or a needle is copied.

use crate::{Error, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "substring",
        |string: TulispObject, from: Option<i64>, to: Option<i64>| {
            let part = string
                .with_str(|text| char_span(text, from, to).map(|span| text[span].to_string()))?;
            part.ok_or_else(|| {
                Error::out_of_range(format!("{string}, {}, {}", or_nil(from), or_nil(to)))
            })
        },
    );

    ctx.defun(
        "string-search",
        |needle: String, haystack: TulispObject, start: Option<i64>| {
            let start = start.unwrap_or(0);
            let found = haystack.with_str(|haystack| search(&needle, haystack, start))?;
            found.ok_or_else(|| Error::out_of_range(start.to_string()))
        },
    );
}

/// `nil` for an absent index, as Emacs shows one in an error.
fn or_nil(index: Option<i64>) -> String {
    index.map_or_else(|| "nil".to_string(), |index| index.to_string())
}

/// The bytes of TEXT from character FROM to character TO, as `substring` counts
/// them: a negative index counts from the end, and an absent one is the start
/// or the end. `None` when the span is not inside TEXT.
fn char_span(text: &str, from: Option<i64>, to: Option<i64>) -> Option<std::ops::Range<usize>> {
    let byte = |at: Option<i64>, default: usize| match at {
        None => Some(default),
        Some(at) if at < 0 => byte_at(text, i64::try_from(text.chars().count()).ok()? + at),
        Some(at) => byte_at(text, at),
    };
    let (start, end) = (byte(from, 0)?, byte(to, text.len())?);
    (start <= end).then_some(start..end)
}

/// Where character AT starts in TEXT, in bytes; the end of TEXT for AT equal to
/// its length. `None` past that.
fn byte_at(text: &str, at: i64) -> Option<usize> {
    let at = usize::try_from(at).ok()?;
    text.char_indices()
        .map(|(byte, _)| byte)
        .chain([text.len()])
        .nth(at)
}

/// Where NEEDLE first is in HAYSTACK at or after character START, in
/// characters; `Some(None)` when it is not there, and `None` when START is not
/// inside HAYSTACK.
fn search(needle: &str, haystack: &str, start: i64) -> Option<Option<i64>> {
    let from = byte_at(haystack, start)?;
    Some(haystack[from..].find(needle).map(|at| {
        let skipped = haystack[from..from + at].chars().count();
        start + i64::try_from(skipped).unwrap_or(i64::MAX)
    }))
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{assert_results, eval_assert_equal};

    #[test]
    fn substring_counts_characters() {
        assert_results(&[
            (r#"(substring "héllo" 1 3)"#, r#""él""#),
            (r#"(substring "hello" -3)"#, r#""llo""#),
            (r#"(substring "hello" nil 2)"#, r#""he""#),
            (r#"(substring "hello" 1 -1)"#, r#""ell""#),
            (r#"(substring "abc" 3)"#, r#""""#),
            (r#"(substring "" 0)"#, r#""""#),
            (r#"(substring "abc" 0 0)"#, r#""""#),
        ]);
    }

    #[test]
    fn substring_outside_the_string_is_an_error() {
        let ctx = &mut TulispContext::new();
        for (program, message) in [
            (
                r#"(substring "abc" 2 9)"#,
                r#"Args out of range: "abc", 2, 9"#,
            ),
            (
                r#"(substring "abc" -4)"#,
                r#"Args out of range: "abc", -4, nil"#,
            ),
            (
                r#"(substring "abc" 2 1)"#,
                r#"Args out of range: "abc", 2, 1"#,
            ),
            (
                r#"(substring "abc" 0 -4)"#,
                r#"Args out of range: "abc", 0, -4"#,
            ),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {program} (error (error-message-string e)))"),
                &format!("{message:?}"),
            );
        }
        assert_results(&[(
            r#"(substring "abc" 1.0)"#,
            "(ERR (wrong-type-argument integerp 1.0))",
        )]);
    }

    #[test]
    fn string_search_finds_characters() {
        assert_results(&[
            (r#"(string-search "l" "héllo")"#, "2"),
            (r#"(string-search "l" "héllo" 3)"#, "3"),
            (r#"(string-search "z" "abc")"#, "nil"),
            (r#"(string-search "" "abc")"#, "0"),
            (r#"(string-search "" "abc" 3)"#, "3"),
            (r#"(string-search "é" "aéé" 2)"#, "2"),
            (r#"(string-search "" "")"#, "0"),
            (
                r#"(string-search "a" "abc" 4)"#,
                "(ERR (args-out-of-range \"4\"))",
            ),
            (
                r#"(string-search "a" "abc" -1)"#,
                "(ERR (args-out-of-range \"-1\"))",
            ),
            (
                r#"(string-search "" "" 1)"#,
                "(ERR (args-out-of-range \"1\"))",
            ),
            (
                r#"(string-search 'a "abc")"#,
                "(ERR (wrong-type-argument stringp a))",
            ),
        ]);
    }
}
