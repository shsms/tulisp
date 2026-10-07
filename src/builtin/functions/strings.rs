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
}
