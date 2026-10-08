//! Functions on strings and characters.
//!
//! The string a function works on is borrowed, not copied; a short argument
//! such as a prefix or a needle is copied.

use crate::{Error, Number, Rest, TulispContext, TulispObject};

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
        change_case(&obj, upcase_char, upcase_text)
    });

    ctx.defun("downcase", |obj: TulispObject| {
        change_case(&obj, downcase_char, downcase_text)
    });

    ctx.defun("capitalize", |obj: TulispObject| {
        change_case(&obj, titlecase_char, capitalize_words)
    });

    ctx.defun(
        "string-to-number",
        |string: TulispObject, base: Option<i64>| {
            let base = base.unwrap_or(10);
            let Some(base) = u32::try_from(base).ok().filter(|b| (2..=16).contains(b)) else {
                return Err(Error::out_of_range(base.to_string()));
            };
            string.with_str(|text| leading_number(text, base))?
        },
    );

    ctx.defun("number-to-string", |number: Number| number.to_string());

    ctx.defun("char-to-string", |character: TulispObject| {
        char_of(&character).map(String::from)
    });

    ctx.defun("string", |characters: Rest<TulispObject>| {
        characters
            .iter()
            .map(char_of)
            .collect::<Result<String, Error>>()
    });

    ctx.defun("string-to-char", |string: TulispObject| {
        string.with_str(|text| text.chars().next().map_or(0, |c| c as i64))
    });
}

/// `nil` for an absent index, as Emacs shows one in an error.
pub(crate) fn or_nil(index: Option<i64>) -> String {
    index.map_or_else(|| "nil".to_string(), |index| index.to_string())
}

/// The bytes of TEXT from character FROM to character TO, as `substring` counts
/// them: a negative index counts from the end, and an absent one is the start
/// or the end. `None` when the span is not inside TEXT.
pub(crate) fn char_span(
    text: &str,
    from: Option<i64>,
    to: Option<i64>,
) -> Option<std::ops::Range<usize>> {
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

/// Whether the characters of PART come first in TEXT; with IGNORE_CASE, two
/// characters match when their upper cases do, as in Emacs's `compare-strings`.
fn same_chars(
    part: impl Iterator<Item = char>,
    mut text: impl Iterator<Item = char>,
    ignore_case: bool,
) -> bool {
    let fold = |c: char| if ignore_case { upcase_char(c) } else { c };
    part.into_iter()
        .all(|p| text.next().is_some_and(|t| fold(t) == fold(p)))
}

/// C in lower case, when that is one character; else C. The Kelvin sign is the
/// exception Emacs makes: it stays as it is.
fn downcase_char(c: char) -> char {
    if c == '\u{212A}' {
        return c;
    }
    one_char(c.to_lowercase()).unwrap_or(c)
}

/// The one character in CHARS, if there is exactly one.
fn one_char(mut chars: impl Iterator<Item = char>) -> Option<char> {
    let first = chars.next()?;
    chars.next().is_none().then_some(first)
}

/// OBJ, a string or a character, with ON_TEXT or ON_CHAR applied.
fn change_case(
    obj: &TulispObject,
    on_char: fn(char) -> char,
    on_text: fn(&str) -> String,
) -> Result<TulispObject, Error> {
    if let Ok(code) = i64::try_from(obj)
        && code >= 0
    {
        return Ok(TulispObject::from(change_code(code, on_char)));
    }
    obj.with_str(on_text).map(TulispObject::from).map_err(|_| {
        Error::wrong_type_argument(
            "char-or-string-p",
            obj.clone(),
            format!("Expected character or string, got: {obj}"),
        )
    })
}

/// The character code CODE with ON_CHAR applied, as Emacs does it: the modifier
/// bits above the character (Meta, Control and the like) are kept, and a code
/// that is not a Unicode character, such as one of Emacs's raw bytes, stays as
/// it is, as does one with all six modifier bits set.
fn change_code(code: i64, on_char: fn(char) -> char) -> i64 {
    const MODIFIERS: i64 = 0xFC0_0000;
    if code >= MODIFIERS {
        return code;
    }
    let (modifiers, base) = (code & MODIFIERS, code & !MODIFIERS);
    u32::try_from(base)
        .ok()
        .and_then(char::from_u32)
        .map_or(code, |c| modifiers | i64::from(u32::from(on_char(c))))
}

/// C in upper case, when that is one character; else C. The exceptions Emacs
/// makes: `ß` upcases to `ẞ`, `ı` and `ſ` stay as they are, and a Greek letter
/// with a small iota below takes its one-character title case.
fn upcase_char(c: char) -> char {
    match c {
        'ß' => 'ẞ',
        'ı' | 'ſ' => c,
        _ => iota_title(c)
            .or_else(|| one_char(c.to_uppercase()))
            .unwrap_or(c),
    }
}

/// TEXT in upper case, where `ı` and `ſ` stay as they are, as in Emacs.
fn upcase_text(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        if matches!(c, 'ı' | 'ſ') {
            out.push(c);
        } else {
            out.extend(c.to_uppercase());
        }
    }
    out
}

/// The character `capitalize` makes of C.
fn titlecase_char(c: char) -> char {
    title_exception(c).unwrap_or_else(|| upcase_char(c))
}

/// The title case of C, for the letters whose title case is one character other
/// than their upper case: the digraphs, Georgian letters, which are their own
/// title case, the Greek letters with a small iota below, and `ı` and `ſ`,
/// which Emacs titles though it does not upcase them.
fn title_exception(c: char) -> Option<char> {
    match c {
        'ı' => Some('I'),
        'ſ' => Some('S'),
        'Ǆ' | 'ǅ' | 'ǆ' => Some('ǅ'),
        'Ǉ' | 'ǈ' | 'ǉ' => Some('ǈ'),
        'Ǌ' | 'ǋ' | 'ǌ' => Some('ǋ'),
        'Ǳ' | 'ǲ' | 'ǳ' => Some('ǲ'),
        '\u{10D0}'..='\u{10FA}' | '\u{10FD}'..='\u{10FF}' => Some(c),
        _ => iota_title(c),
    }
}

/// The title case of a Greek letter with a small iota below, such as `ᾳ` or
/// `ᾀ`: one character, where its upper case is two.
fn iota_title(c: char) -> Option<char> {
    let code = u32::from(c);
    let title = match code {
        0x1F80..=0x1F87 | 0x1F90..=0x1F97 | 0x1FA0..=0x1FA7 => code + 8,
        0x1FB3 | 0x1FC3 | 0x1FF3 => code + 9,
        0x1F88..=0x1F8F | 0x1F98..=0x1F9F | 0x1FA8..=0x1FAF | 0x1FBC | 0x1FCC | 0x1FFC => code,
        _ => return None,
    };
    char::from_u32(title)
}

/// TEXT in lower case.
fn downcase_text(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    let mut before = None;
    while let Some(c) = chars.next() {
        push_lower(&mut out, c, before, chars.peek().copied());
        before = Some(c);
    }
    out
}

/// TEXT with each word's first letter in title case and the rest in lower case.
/// `don't` is two words, as in Emacs.
fn capitalize_words(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    let mut before = None;
    while let Some(c) = chars.next() {
        if !is_word(c) {
            out.push(c);
        } else if before.is_some_and(is_word) {
            push_lower(&mut out, c, before, chars.peek().copied());
        } else {
            push_title(&mut out, c);
        }
        before = Some(c);
    }
    out
}

/// Pushes C in lower case, with the characters BEFORE and AFTER it. A capital
/// sigma becomes the final `ς` when it ends a word: after a word character and
/// not before one, as in Emacs. The Kelvin sign stays as it is, as `downcase`
/// keeps it when given it as a character.
fn push_lower(out: &mut String, c: char, before: Option<char>, after: Option<char>) {
    if c == 'Σ' {
        let ends_word = before.is_some_and(is_word) && !after.is_some_and(is_word);
        out.push(if ends_word { 'ς' } else { 'σ' });
    } else if c == '\u{212A}' {
        out.push(c);
    } else {
        out.extend(c.to_lowercase());
    }
}

/// Whether C belongs to a word: a letter, a digit, or a combining mark from the
/// common blocks, such as the acute accent in `x\u{301}`. Emacs's syntax table
/// differs: it also counts most symbols, some punctuation and the combining
/// marks of other blocks, but not some characters counted here, such as `½`,
/// `ª` and `①`.
fn is_word(c: char) -> bool {
    c.is_alphanumeric()
        || matches!(
            c,
            '\u{300}'..='\u{36F}'
                | '\u{1AB0}'..='\u{1AFF}'
                | '\u{1DC0}'..='\u{1DFF}'
                | '\u{20D0}'..='\u{20FF}'
                | '\u{FE20}'..='\u{FE2F}'
        )
}

/// Pushes FIRST, the first letter of a word, in title case. One that upper case
/// makes into several letters, such as `ß` into `SS`, keeps only the first of
/// them in upper case; `ŉ` gives `ʼN`, and the iota below a letter such as `ᾲ`
/// stays a combining iota, as in Emacs.
fn push_title(out: &mut String, first: char) {
    if let Some(title) = title_exception(first) {
        out.push(title);
        return;
    }
    if first == 'ŉ' {
        out.push_str("ʼN");
        return;
    }
    let mut upper = first.to_uppercase();
    if let Some(head) = upper.next() {
        out.push(head);
    }
    for rest in upper {
        if rest == 'Ι' {
            out.push('\u{345}');
        } else {
            out.extend(rest.to_lowercase());
        }
    }
}

/// The character with code OBJ, or the error Emacs gives for one that is not a
/// character.
fn char_of(obj: &TulispObject) -> Result<char, Error> {
    i64::try_from(obj)
        .ok()
        .and_then(|code| u32::try_from(code).ok())
        .and_then(char::from_u32)
        .ok_or_else(|| {
            Error::wrong_type_argument(
                "characterp",
                obj.clone(),
                format!("Expected character, got: {obj}"),
            )
        })
}

/// The number at the start of TEXT, after spaces and tabs, as Emacs's
/// `string-to-number` reads it in BASE; 0 when there is none. Only base 10
/// reads a fraction or an exponent.
fn leading_number(text: &str, base: u32) -> Result<TulispObject, Error> {
    let text = text.trim_start_matches([' ', '\t']);
    let (negative, unsigned) = match text.as_bytes().first() {
        Some(b'-') => (true, &text[1..]),
        Some(b'+') => (false, &text[1..]),
        _ => (false, text),
    };
    let sign = if negative { "-" } else { "" };
    let digits = |s: &str, base: u32| s.find(|c: char| !c.is_digit(base)).unwrap_or(s.len());

    let lead = digits(unsigned, base);
    if base != 10 {
        return integer(sign, &unsigned[..lead], base);
    }
    let mut end = lead;
    let has_dot = unsigned[end..].starts_with('.');
    if has_dot {
        end += 1;
    }
    let trail = digits(&unsigned[end..], 10);
    end += trail;
    if lead + trail == 0 {
        return Ok(TulispObject::from(0));
    }
    let exponent = &unsigned[end..];
    if let Some(after) = exponent.strip_prefix(['e', 'E']) {
        let special = if after.starts_with("+INF") {
            Some(f64::INFINITY)
        } else if after.starts_with("+NaN") {
            Some(f64::NAN)
        } else {
            None
        };
        if let Some(value) = special {
            return Ok(TulispObject::from(if negative { -value } else { value }));
        }
        let signed = after.strip_prefix(['+', '-']).unwrap_or(after);
        let exponent_digits = digits(signed, 10);
        if exponent_digits > 0 {
            let length = end + 1 + (after.len() - signed.len()) + exponent_digits;
            return float(sign, &unsigned[..length]);
        }
    }
    if has_dot && trail > 0 {
        return float(sign, &unsigned[..end]);
    }
    integer(sign, &unsigned[..lead], 10)
}

/// DIGITS, with SIGN, as an integer in BASE: 0 for no digits, and an error for
/// one too large for tulisp's integers.
fn integer(sign: &str, digits: &str, base: u32) -> Result<TulispObject, Error> {
    if digits.is_empty() {
        return Ok(TulispObject::from(0));
    }
    i64::from_str_radix(&format!("{sign}{digits}"), base)
        .map(TulispObject::from)
        .map_err(|_| Error::arith_error(format!("integer overflow: {sign}{digits}")))
}

/// MANTISSA, with SIGN, which `leading_number` checked, as a float.
fn float(sign: &str, mantissa: &str) -> Result<TulispObject, Error> {
    format!("{sign}{mantissa}")
        .parse::<f64>()
        .map(TulispObject::from)
        .map_err(|e| Error::arith_error(format!("{e}: {sign}{mantissa}")))
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
            // Letters that share an upper case match, as in Emacs.
            (r#"(string-prefix-p "σ" "ς" t)"#, "t"),
            (r#"(string-suffix-p "Σ" "aς" t)"#, "t"),
            ("(string-prefix-p \"\u{B5}\" \"\u{3BC}\" t)", "t"),
            (r#"(string-prefix-p "ᾳ" "ᾼ" t)"#, "t"),
            (r#"(string-prefix-p "ı" "I" t)"#, "nil"),
            ("(string-prefix-p \"\u{212A}\" \"k\" t)", "nil"),
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
            // Emacs keeps `ı` and `ſ`.
            (r#"(upcase "ıſa")"#, r#""ıſA""#),
            ("(downcase \"A\u{212A}\")", "\"a\u{212A}\""),
            // A capital sigma that ends a word becomes the final sigma.
            (
                r#"(downcase "ΑΣ ΑΣ1 ΑΣ'Α 1Σ ΣΣ")"#,
                r#""ας ασ1 ας'α 1ς σς""#,
            ),
            ("(downcase \"ΑΣ\u{301}\")", "\"ασ\u{301}\""),
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
            // Meta-a: the modifier bits are kept.
            ("(upcase 134217825)", "134217793"),
            // With all six modifier bits set, the code stays as it is.
            ("(upcase 264241249)", "264241249"),
            ("(upcase ?ı)", "305"),
            ("(upcase ?ſ)", "383"),
            ("(downcase ?\u{212A})", "8490"),
            ("(upcase ?ᾳ)", "8124"),
            ("(upcase ?ᾀ)", "8072"),
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

    #[test]
    fn capitalize_strings_and_characters() {
        assert_results(&[
            (
                r#"(capitalize "hello wORLD foo-bar 3rd")"#,
                r#""Hello World Foo-Bar 3rd""#,
            ),
            (
                r#"(capitalize "don't it's o'neil")"#,
                r#""Don'T It'S O'Neil""#,
            ),
            (r#"(capitalize "x_y z.w")"#, r#""X_Y Z.W""#),
            (r#"(capitalize "ÉCOLE élève")"#, r#""École Élève""#),
            (r#"(capitalize "ΣΑΣ abc")"#, r#""Σας Abc""#),
            (r#"(capitalize "ǆemal")"#, r#""ǅemal""#),
            (r#"(capitalize "ß")"#, r#""Ss""#),
            (r#"(capitalize "é")"#, r#""É""#),
            ("(capitalize ?ß)", "7838"),
            ("(capitalize ?a)", "65"),
            ("(capitalize ?ǆ)", "453"),
            ("(capitalize ?ა)", "4304"),
            ("(capitalize ?ı)", "73"),
            ("(capitalize ?ᾳ)", "8124"),
            (r#"(capitalize "ᾳx ᾀx")"#, r#""ᾼx ᾈx""#),
            (r#"(capitalize "გამარჯობა")"#, r#""გამარჯობა""#),
            (r#"(capitalize "ŉa")"#, r#""ʼNa""#),
            ("(capitalize \"ᾲa a\u{212A}\")", "\"Ὰ\u{345}a A\u{212A}\""),
            (r#"(capitalize "ΟΣ 1Σ ΣΣ")"#, r#""Ος 1ς Σς""#),
            ("(capitalize \"x\u{301}y\")", "\"X\u{301}y\""),
        ]);
    }

    #[test]
    fn string_to_number_reads_the_leading_number() {
        assert_results(&[
            (r#"(string-to-number " 42abc")"#, "42"),
            (r#"(string-to-number "1.5")"#, "1.5"),
            (r#"(string-to-number "x")"#, "0"),
            (r#"(string-to-number "-12")"#, "-12"),
            (r#"(string-to-number "+7")"#, "7"),
            (r#"(string-to-number ".5")"#, "0.5"),
            (r#"(string-to-number "-.5")"#, "-0.5"),
            (r#"(string-to-number "5.")"#, "5"),
            (r#"(string-to-number "1e3")"#, "1000.0"),
            (r#"(string-to-number "1E3")"#, "1000.0"),
            (r#"(string-to-number "1e+3")"#, "1000.0"),
            (r#"(string-to-number "1.5e3")"#, "1500.0"),
            (r#"(string-to-number "1.e3")"#, "1000.0"),
            (r#"(string-to-number ".e3")"#, "0"),
            (r#"(string-to-number "1e")"#, "1"),
            (r#"(string-to-number "-1.5e-2")"#, "-0.015"),
            (r#"(string-to-number "1e400")"#, "1.0e+INF"),
            (r#"(string-to-number "1e+INF")"#, "1.0e+INF"),
            (r#"(string-to-number "-1.0e+INF")"#, "-1.0e+INF"),
            (r#"(string-to-number "0.0e+NaN")"#, "0.0e+NaN"),
            (r#"(string-to-number "1.0e+INFx")"#, "1.0e+INF"),
            (r#"(string-to-number "1.0e-INF")"#, "1.0"),
            (r#"(string-to-number "-")"#, "0"),
            (r#"(string-to-number "  ")"#, "0"),
            (r#"(string-to-number "  12  ")"#, "12"),
            ("(string-to-number \" \\t8\")", "8"),
            ("(string-to-number \"\\n8\")", "0"),
            ("(string-to-number \"\\r8\")", "0"),
            (r#"(string-to-number "0x10")"#, "0"),
            (r#"(string-to-number "١٢")"#, "0"),
            (
                r#"(string-to-number "9223372036854775807")"#,
                "9223372036854775807",
            ),
            (
                r#"(string-to-number "-9223372036854775808")"#,
                "-9223372036854775808",
            ),
        ]);
    }

    #[test]
    fn string_to_number_in_another_base() {
        assert_results(&[
            (r#"(string-to-number "ff" 16)"#, "255"),
            (r#"(string-to-number "FF" 16)"#, "255"),
            (r#"(string-to-number "-ff" 16)"#, "-255"),
            (r#"(string-to-number "11" 2)"#, "3"),
            (r#"(string-to-number "10" 8)"#, "8"),
            (r#"(string-to-number "1.5" 16)"#, "1"),
            (
                r#"(string-to-number "7fffffffffffffff" 16)"#,
                "9223372036854775807",
            ),
            (
                r#"(string-to-number "1" 17)"#,
                "(ERR (args-out-of-range \"17\"))",
            ),
            (
                r#"(string-to-number "1" 1)"#,
                "(ERR (args-out-of-range \"1\"))",
            ),
            (
                r#"(string-to-number "z" 36)"#,
                "(ERR (args-out-of-range \"36\"))",
            ),
        ]);
    }

    // Emacs reads a larger integer as a bignum; tulisp has none.
    #[test]
    fn string_to_number_refuses_an_integer_too_large() {
        let ctx = &mut TulispContext::new();
        for call in [
            r#"(string-to-number "9223372036854775808")"#,
            r#"(string-to-number "-9223372036854775809")"#,
            r#"(string-to-number "8000000000000000" 16)"#,
        ] {
            let program = format!("(condition-case e {call} (error (car e)))");
            eval_assert_equal(ctx, &program, "'arith-error");
        }
    }

    #[test]
    fn number_to_string_prints_as_prin1() {
        assert_results(&[
            ("(number-to-string 7)", r#""7""#),
            ("(number-to-string 1.0)", r#""1.0""#),
            ("(number-to-string -0.5)", r#""-0.5""#),
            ("(number-to-string 1e20)", r#""1e+20""#),
            ("(number-to-string 0.1)", r#""0.1""#),
            ("(number-to-string 100.0)", r#""100.0""#),
            ("(number-to-string -0.0)", r#""-0.0""#),
            (
                "(number-to-string 9223372036854775807)",
                r#""9223372036854775807""#,
            ),
            (
                r#"(number-to-string "x")"#,
                r#"(ERR (wrong-type-argument numberp "x"))"#,
            ),
        ]);
    }

    #[test]
    fn characters_and_strings() {
        assert_results(&[
            ("(char-to-string 233)", r#""é""#),
            ("(char-to-string 1114111)", "\"\u{10FFFF}\""),
            (
                "(char-to-string -1)",
                "(ERR (wrong-type-argument characterp -1))",
            ),
            (
                "(char-to-string 'a)",
                "(ERR (wrong-type-argument characterp a))",
            ),
            ("(string 97 98)", r#""ab""#),
            ("(string)", r#""""#),
            ("(string 233)", r#""é""#),
            ("(string -1)", "(ERR (wrong-type-argument characterp -1))"),
            ("(string 'a)", "(ERR (wrong-type-argument characterp a))"),
            (r#"(string-to-char "é")"#, "233"),
            (r#"(string-to-char "")"#, "0"),
            (r#"(string-to-char "ab")"#, "97"),
            ("(string-to-char \"\\n\")", "10"),
        ]);
    }
}
