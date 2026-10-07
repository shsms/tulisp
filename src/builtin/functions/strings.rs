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

    ctx.defun(
        "string-prefix-p",
        |prefix: String, string: TulispObject, ignore_case: Option<TulispObject>| {
            string.with_str(|text| same_chars(prefix.chars(), text.chars(), ignore_case.is_some()))
        },
    );

    ctx.defun(
        "string-suffix-p",
        |suffix: String, string: TulispObject, ignore_case: Option<TulispObject>| {
            string.with_str(|text| {
                same_chars(
                    suffix.chars().rev(),
                    text.chars().rev(),
                    ignore_case.is_some(),
                )
            })
        },
    );

    // As Emacs's `(string= STRING "")`, so a symbol stands for its name.
    ctx.defun("string-empty-p", |string: TulispObject| {
        if string.symbolp() {
            return Ok(string.symbol_name()?.is_empty());
        }
        string.with_str(str::is_empty)
    });

    ctx.defun(
        "string-replace",
        |ctx: &mut TulispContext, from: String, to: String, in_string: TulispObject| {
            if from.is_empty() {
                let data = TulispObject::cons(TulispObject::from(0), TulispObject::nil());
                return Err(ctx.signal("wrong-length-argument", data));
            }
            in_string.with_str(|text| text.replace(&from, &to))
        },
    );

    ctx.defun("upcase", |obj: TulispObject| {
        change_case(&obj, upcase_char, str::to_uppercase)
    });

    ctx.defun("downcase", |obj: TulispObject| {
        change_case(&obj, downcase_char, str::to_lowercase)
    });
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

/// Whether the characters of PART come first in TEXT; with IGNORE_CASE, a
/// character matches its other case.
fn same_chars(
    part: impl Iterator<Item = char>,
    mut text: impl Iterator<Item = char>,
    ignore_case: bool,
) -> bool {
    let fold = |c: char| if ignore_case { downcase_char(c) } else { c };
    part.into_iter()
        .all(|p| text.next().is_some_and(|t| fold(t) == fold(p)))
}

fn downcase_char(c: char) -> char {
    one_char(c.to_lowercase()).unwrap_or(c)
}

/// The one character in CHARS, if there is exactly one.
fn one_char(mut chars: impl Iterator<Item = char>) -> Option<char> {
    let first = chars.next()?;
    chars.next().is_none().then_some(first)
}

/// OBJ, a string or a character, with ON_TEXT or ON_CHAR applied. A code past
/// Unicode is a character to Emacs, which leaves it as it is.
fn change_case(
    obj: &TulispObject,
    on_char: fn(char) -> char,
    on_text: fn(&str) -> String,
) -> Result<TulispObject, Error> {
    if let Ok(code) = i64::try_from(obj)
        && code >= 0
    {
        let changed = u32::try_from(code)
            .ok()
            .and_then(char::from_u32)
            .map_or(code, |c| i64::from(u32::from(on_char(c))));
        return Ok(TulispObject::from(changed));
    }
    obj.with_str(on_text).map(TulispObject::from).map_err(|_| {
        Error::wrong_type_argument(
            "char-or-string-p",
            obj.clone(),
            format!("Expected character or string, got: {obj}"),
        )
    })
}

/// C in the other case, when that is one character; else C. `ß` is the
/// exception Emacs makes: its upper case is `ẞ`.
fn upcase_char(c: char) -> char {
    if c == 'ß' {
        return 'ẞ';
    }
    one_char(c.to_uppercase()).unwrap_or(c)
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

    #[test]
    fn prefixes_and_suffixes() {
        assert_results(&[
            (r#"(string-prefix-p "gi" "git")"#, "t"),
            (r#"(string-prefix-p "GI" "git" t)"#, "t"),
            (r#"(string-prefix-p "GI" "git")"#, "nil"),
            (r#"(string-prefix-p "" "git")"#, "t"),
            (r#"(string-prefix-p "gitx" "git")"#, "nil"),
            (r#"(string-prefix-p "ẞ" "ßx" t)"#, "t"),
            (r#"(string-prefix-p "ss" "ßx" t)"#, "nil"),
            (r#"(string-prefix-p "é" "Éa" t)"#, "t"),
            (r#"(string-suffix-p "it" "git")"#, "t"),
            (r#"(string-suffix-p "IT" "git" t)"#, "t"),
            (r#"(string-suffix-p "É" "aé" t)"#, "t"),
            (r#"(string-suffix-p "" "x")"#, "t"),
        ]);
    }

    #[test]
    fn string_empty_p_takes_a_symbol_by_its_name() {
        assert_results(&[
            (r#"(string-empty-p "")"#, "t"),
            (r#"(string-empty-p "a")"#, "nil"),
            ("(string-empty-p nil)", "nil"),
            ("(string-empty-p 'a)", "nil"),
            (
                "(string-empty-p 5)",
                "(ERR (wrong-type-argument stringp 5))",
            ),
        ]);
    }

    #[test]
    fn string_replace_replaces_every_match() {
        assert_results(&[
            (r#"(string-replace "o" "0" "foo")"#, r#""f00""#),
            (r#"(string-replace "ab" "" "xaby")"#, r#""xy""#),
            (r#"(string-replace "aa" "b" "aaa")"#, r#""ba""#),
            (r#"(string-replace "é" "e" "éé")"#, r#""ee""#),
            (r#"(string-replace "a" "b" "")"#, r#""""#),
            (
                r#"(string-replace "" "x" "foo")"#,
                "(ERR (wrong-length-argument 0))",
            ),
        ]);
    }

    #[test]
    fn case_of_strings() {
        assert_results(&[
            (r#"(upcase "abé")"#, r#""ABÉ""#),
            (r#"(upcase "ß")"#, r#""SS""#),
            (r#"(upcase "ﬁ")"#, r#""FI""#),
            (r#"(upcase "σας")"#, r#""ΣΑΣ""#),
            (r#"(downcase "AB")"#, r#""ab""#),
            (r#"(downcase "ΣΑΣ")"#, r#""σας""#),
        ]);
    }

    #[test]
    fn case_of_characters() {
        assert_results(&[
            ("(upcase 97)", "65"),
            ("(downcase 65)", "97"),
            ("(upcase ?ß)", "7838"),
            ("(upcase ?ǆ)", "452"),
            ("(upcase ?ﬁ)", "64257"),
            ("(downcase ?İ)", "304"),
            ("(upcase 4194303)", "4194303"),
            (
                "(upcase -1)",
                "(ERR (wrong-type-argument char-or-string-p -1))",
            ),
            (
                "(upcase 1.5)",
                "(ERR (wrong-type-argument char-or-string-p 1.5))",
            ),
            (
                "(upcase 'a)",
                "(ERR (wrong-type-argument char-or-string-p a))",
            ),
        ]);
    }
}
