//! Emacs's `format`.

use crate::{Error, TulispObject};

/// No float has a digit other than zero past this many after the point:
/// 2^-1074, the smallest subnormal, ends there.
const MAX_FRACTION_DIGITS: usize = 1074;

/// Emacs's error for a string `format` cannot make that long.
fn string_too_long() -> Error {
    Error::lisp_error("Maximum string size exceeded")
}

/// NUMBER with the decimal DIGIT appended, or `usize::MAX` if that overflows:
/// a width or precision too large for any string.
fn add_digit(number: usize, digit: char) -> usize {
    let digit = digit.to_digit(10).unwrap_or_default() as usize;
    number.saturating_mul(10).saturating_add(digit)
}

/// Appends COUNT copies of CH to OUT, or returns an error if there is not the
/// memory for them.
fn push_repeated(out: &mut String, ch: char, count: usize) -> Result<(), Error> {
    let bytes = count
        .checked_mul(ch.len_utf8())
        .ok_or_else(string_too_long)?;
    out.try_reserve(bytes).map_err(|_| string_too_long())?;
    out.extend(std::iter::repeat_n(ch, count));
    Ok(())
}

/// Formats IN_STRING with ARGS as Emacs's `format` does, for the specs Tulisp
/// supports. A format string it cannot use, one that asks for more ARGS than
/// there are, or a spec whose argument has the wrong type is an `error`, with
/// Emacs's text.
pub(crate) fn format_string(
    in_string: &str,
    args: impl IntoIterator<Item = TulispObject>,
) -> Result<String, Error> {
    let mut args = args.into_iter();
    let mut output = String::new();
    let mut in_chars = in_string.chars().peekable();
    // Supports `%[-][0]WIDTH[.PRECISION]TYPE` where TYPE is one of `s S d f`,
    // plus `%%` for a literal percent. The `-` flag left-aligns and the `0`
    // flag pads numerics with zeros. PRECISION applies to `%f` (digits after
    // the decimal point). See the Emacs manual for the full format-spec
    // grammar:
    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
    while let Some(ch) = in_chars.next() {
        if ch != '%' {
            output.push(ch);
            continue;
        }
        let mut left_align = false;
        let mut zero_pad = false;
        let mut width: usize = 0;
        loop {
            match in_chars.peek() {
                Some('-') => {
                    left_align = true;
                    in_chars.next();
                }
                Some('0') if width == 0 => {
                    zero_pad = true;
                    in_chars.next();
                }
                Some(c) if c.is_ascii_digit() => {
                    width = add_digit(width, *c);
                    in_chars.next();
                }
                _ => break,
            }
        }
        let mut precision: Option<usize> = None;
        if in_chars.peek() == Some(&'.') {
            in_chars.next();
            let mut p: usize = 0;
            while let Some(c) = in_chars.peek() {
                if !c.is_ascii_digit() {
                    break;
                }
                p = add_digit(p, *c);
                in_chars.next();
            }
            precision = Some(p);
        }
        let Some(type_char) = in_chars.next() else {
            return Err(Error::lisp_error(
                "Format string ends in middle of format specifier",
            ));
        };
        // A width whose digits overflowed is too wide for any string, as in
        // Emacs.
        if width == usize::MAX {
            return Err(string_too_long());
        }
        if type_char == '%' {
            output.push('%');
            continue;
        }
        let Some(next_arg) = args.next() else {
            return Err(Error::lisp_error("Not enough arguments for format string"));
        };
        if matches!(type_char, 'd' | 'f') && !next_arg.numberp() {
            return Err(Error::lisp_error(
                "Format specifier doesn\u{2019}t match argument type",
            ));
        }
        // In Emacs a `%d` precision pads with zeros, so one whose digits
        // overflowed is too wide for any string there.
        if type_char == 'd' && precision == Some(usize::MAX) {
            return Err(string_too_long());
        }
        let formatted = match type_char {
            's' => next_arg.fmt_string(),
            'S' => next_arg.to_string(),
            'd' => next_arg.try_int()?.to_string(),
            'f' => {
                let v = next_arg.try_float()?;
                match precision {
                    Some(p) => {
                        let shown = p.min(MAX_FRACTION_DIGITS);
                        let mut formatted = format!("{v:.shown$}");
                        if v.is_finite() {
                            push_repeated(&mut formatted, '0', p - shown)?;
                        }
                        formatted
                    }
                    None => v.to_string(),
                }
            }
            _ => {
                return Err(Error::lisp_error(format!(
                    "Invalid format operation %{type_char}"
                )));
            }
        };
        let len = formatted.chars().count();
        if width > len {
            let pad_char = if zero_pad && !left_align && matches!(type_char, 'd' | 'f') {
                '0'
            } else {
                ' '
            };
            if left_align {
                output.push_str(&formatted);
                push_repeated(&mut output, pad_char, width - len)?;
            } else {
                push_repeated(&mut output, pad_char, width - len)?;
                output.push_str(&formatted);
            }
        } else {
            output.push_str(&formatted);
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert, eval_assert_equal, eval_assert_error_line};
    use crate::{Error, TulispContext};

    #[test]
    fn format_fills_in_and_pads_its_specs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(format "Hello, %s! %%%d %f %s %d" "world" 22.8 22.8 10 10)"#,
            r#""Hello, world! %22 22.8 10 10""#,
        );
        // Width: right-aligned by default, left-aligned with `-`.
        eval_assert_equal(ctx, r#"(format "[%10s]" "hi")"#, r#""[        hi]""#);
        eval_assert_equal(ctx, r#"(format "[%-10s]" "hi")"#, r#""[hi        ]""#);
        // Zero-pad for numerics.
        eval_assert_equal(ctx, r#"(format "%05d" 42)"#, r#""00042""#);
        // Shorter than width stays as-is (no truncation).
        eval_assert_equal(ctx, r#"(format "[%3s]" "hello")"#, r#""[hello]""#);
        // Width applies to %d too.
        eval_assert_equal(ctx, r#"(format "[%5d]" 7)"#, r#""[    7]""#);
        eval_assert_equal(ctx, r#"(format "[%-5d]" 7)"#, r#""[7    ]""#);
    }

    #[test]
    fn format_prints_any_number_of_digits() {
        // No float has digits past the 1074th after the point, so those are
        // zeros, as Emacs prints them.
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(length (format "%.70000f" 1.5))"#, "70002");
        eval_assert(
            ctx,
            r#"(equal (format "%.70000f" 0.1)
                      (concat (format "%.1074f" 0.1) (make-string (- 70000 1074) ?0)))"#,
        );
        eval_assert(
            ctx,
            r#"(equal (format "%.3000f" 5e-324)
                      (concat (format "%.1074f" 5e-324) (make-string (- 3000 1074) ?0)))"#,
        );
    }

    #[test]
    fn format_prints_the_last_digits_and_infinity_as_emacs() -> Result<(), Error> {
        // The 1074th digit after the point, at index 1075, is the last that
        // is not zero.
        let ctx = &mut TulispContext::new();
        let printed = ctx
            .eval_string(r#"(format "%.3000f" 5e-324)"#)?
            .as_string()?;
        assert_eq!(&printed[1069..1079], "7265625000");
        eval_assert_equal(ctx, r#"(format "%.2000f" 1.0e+INF)"#, r#""inf""#);
        Ok(())
    }

    #[test]
    fn a_precision_too_large_is_no_error_where_nothing_uses_it() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(format "%.99999999999999999999s|%.9999999999s" "ab" "cd")"#,
            r#""ab|cd""#,
        );
        eval_assert_equal(ctx, r#"(format "%.99999999999999999999%%d" 7)"#, r#""%7""#);
        eval_assert_equal(
            ctx,
            r#"(format "%.99999999999999999999f" 1.0e+INF)"#,
            r#""inf""#,
        );
        // The arguments are checked first, as in Emacs.
        eval_assert_error_line(
            ctx,
            r#"(format "%.99999999999999999999d")"#,
            "ERR LispError: Not enough arguments for format string",
        );
    }

    #[test]
    fn a_format_spec_too_wide_is_an_error() {
        // Emacs's error, for a width or precision whose digits overflow, and
        // for one too large to allocate.
        let ctx = &mut TulispContext::new();
        for program in [
            r#"(format "%99999999999999999999d" 1)"#,
            r#"(format "%.99999999999999999999f" 1.0)"#,
            r#"(format "%9000000000000000000d" 1)"#,
            r#"(format "%-9000000000000000000s" "a")"#,
            r#"(format "%18446744073709551617d" 1)"#,
            r#"(format "%.18446744073709551617f" 1.0)"#,
            r#"(format "%.99999999999999999999d" 7)"#,
            r#"(format "%99999999999999999999%")"#,
        ] {
            eval_assert_error_line(ctx, program, "ERR LispError: Maximum string size exceeded");
        }
    }

    // A format string `format` cannot use is an `error`, as in Emacs.
    #[test]
    fn a_bad_format_string_is_an_error() {
        let ctx = &mut TulispContext::new();
        // Each text is Emacs 30's.
        for (program, text) in [
            (
                r#"(format "100%")"#,
                "Format string ends in middle of format specifier",
            ),
            (r#"(format "%q" 1)"#, "Invalid format operation %q"),
            (r#"(format "%s")"#, "Not enough arguments for format string"),
            (
                r#"(format "%d" "x")"#,
                "Format specifier doesn\u{2019}t match argument type",
            ),
            (
                r#"(format "%f" "x")"#,
                "Format specifier doesn\u{2019}t match argument type",
            ),
        ] {
            eval_assert_error_line(ctx, program, &format!("ERR LispError: {text}"));
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {program} (error (car e)))"),
                "'error",
            );
        }
        // A float too large for %d is an arith-error from the conversion, not
        // the argument-type error. Emacs prints the integer; tulisp has no
        // bignums.
        eval_assert_equal(
            ctx,
            r#"(condition-case nil (format "%d" 1e30) (arith-error 'arith))"#,
            "'arith",
        );
    }
}
