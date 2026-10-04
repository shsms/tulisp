//! Emacs's `format`.

use std::{iter::Peekable, str::Chars};

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

/// One `%` spec of a format string.
#[derive(Default)]
struct Spec {
    /// The `-` flag: pad on the right.
    left: bool,
    /// The `0` flag: pad a number with zeros after its sign.
    zero: bool,
    /// The `+` flag: sign a number that is not negative with `+`.
    plus: bool,
    /// The space flag: sign a number that is not negative with a space.
    space: bool,
    /// The `#` flag: the alternate form, such as `0x` before a hex number.
    alt: bool,
    width: usize,
    precision: Option<usize>,
    conversion: char,
}

impl Spec {
    /// The sign of a number under this spec: `-` when NEGATIVE, or else what
    /// the `+` or space flag asks for.
    fn sign(&self, negative: bool) -> &'static str {
        if negative {
            "-"
        } else if self.plus {
            "+"
        } else if self.space {
            " "
        } else {
            ""
        }
    }

    /// Reads the spec after a `%`, up to and including its conversion.
    fn read(chars: &mut Peekable<Chars<'_>>) -> Result<Spec, Error> {
        let mut spec = Spec::default();
        loop {
            match chars.peek() {
                Some('-') => spec.left = true,
                Some('+') => spec.plus = true,
                Some(' ') => spec.space = true,
                Some('#') => spec.alt = true,
                Some('0') if spec.width == 0 => spec.zero = true,
                Some(c) if c.is_ascii_digit() => spec.width = add_digit(spec.width, *c),
                _ => break,
            }
            chars.next();
        }
        if chars.next_if_eq(&'.').is_some() {
            let mut digits = 0;
            while let Some(c) = chars.next_if(char::is_ascii_digit) {
                digits = add_digit(digits, c);
            }
            spec.precision = Some(digits);
        }
        spec.conversion = chars
            .next()
            .ok_or_else(|| Error::lisp_error("Format string ends in middle of format specifier"))?;
        Ok(spec)
    }
}

/// An argument formatted by its spec, before padding.
struct Field {
    sign: &'static str,
    /// What goes between the sign and the body, such as `0x`.
    prefix: &'static str,
    body: String,
    /// Whether the `0` flag pads it with zeros.
    zero_pad: bool,
}

impl Field {
    fn text(body: String) -> Field {
        Field {
            sign: "",
            prefix: "",
            body,
            zero_pad: false,
        }
    }
}

/// Formats IN_STRING with ARGS as Emacs's `format` does, for the specs Tulisp
/// supports: `%[FLAGS][WIDTH][.PRECISION]CONVERSION`. A format string it cannot
/// use, one that asks for more ARGS than there are, or a spec whose argument
/// has the wrong type is an `error`, with Emacs's text.
pub(crate) fn format_string(
    in_string: &str,
    args: impl IntoIterator<Item = TulispObject>,
) -> Result<String, Error> {
    let mut args = args.into_iter();
    let mut output = String::new();
    let mut in_chars = in_string.chars().peekable();
    while let Some(ch) = in_chars.next() {
        if ch != '%' {
            output.push(ch);
            continue;
        }
        let spec = Spec::read(&mut in_chars)?;
        // A width whose digits overflowed is too wide for any string, as in
        // Emacs.
        if spec.width == usize::MAX {
            return Err(string_too_long());
        }
        if spec.conversion == '%' {
            output.push('%');
            continue;
        }
        let Some(arg) = args.next() else {
            return Err(Error::lisp_error("Not enough arguments for format string"));
        };
        let field = convert(&spec, &arg)?;
        pad(&mut output, &spec, &field)?;
    }
    Ok(output)
}

/// ARG formatted by SPEC's conversion.
fn convert(spec: &Spec, arg: &TulispObject) -> Result<Field, Error> {
    if matches!(spec.conversion, 'd' | 'x' | 'X' | 'o' | 'f' | 'e') && !arg.numberp() {
        return Err(Error::lisp_error(
            "Format specifier doesn\u{2019}t match argument type",
        ));
    }
    match spec.conversion {
        's' => Ok(Field::text(cut(arg.fmt_string(), spec.precision))),
        'S' => Ok(Field::text(cut(arg.to_string(), spec.precision))),
        'd' | 'x' | 'X' | 'o' => integer(spec, arg.try_int()?),
        'f' | 'e' => float(spec, arg.try_float()?),
        other => Err(invalid_operation(other)),
    }
}

/// Emacs's error for a conversion `format` does not know.
fn invalid_operation(conversion: char) -> Error {
    Error::lisp_error(format!("Invalid format operation %{conversion}"))
}

/// VALUE under SPEC's float conversion: `%f` or `%e`.
fn float(spec: &Spec, value: f64) -> Result<Field, Error> {
    let sign = spec.sign(value.is_sign_negative());
    if !value.is_finite() {
        let body = if value.is_nan() { "nan" } else { "inf" };
        return Ok(Field {
            sign,
            prefix: "",
            body: body.to_string(),
            zero_pad: false,
        });
    }
    let precision = spec.precision.unwrap_or(6);
    let body = match spec.conversion {
        'f' => fixed_form(value.abs(), precision, spec.alt)?,
        'e' => exponent_form(value.abs(), precision, spec.alt)?,
        other => return Err(invalid_operation(other)),
    };
    Ok(Field {
        sign,
        prefix: "",
        body,
        zero_pad: true,
    })
}

/// VALUE, not negative, as `%f` prints it: PRECISION digits after the point.
/// With ALT, the point stays when no digits follow it.
fn fixed_form(value: f64, precision: usize, alt: bool) -> Result<String, Error> {
    let shown = precision.min(MAX_FRACTION_DIGITS);
    let mut body = format!("{value:.shown$}");
    push_repeated(&mut body, '0', precision - shown)?;
    if alt && precision == 0 {
        body.push('.');
    }
    Ok(body)
}

/// VALUE, not negative, as `%e` prints it: one digit, the point and PRECISION
/// digits, then `e` and the exponent with its sign and at least two digits.
/// With ALT, the point stays when no digits follow it.
fn exponent_form(value: f64, precision: usize, alt: bool) -> Result<String, Error> {
    // A float has fewer significant digits than this; the rest are zeros.
    let shown = precision.min(MAX_FRACTION_DIGITS);
    // Rust's form, like `1.23e4`.
    let rust = format!("{value:.shown$e}");
    let (mantissa, exponent) = rust.split_once('e').unwrap_or((&rust, "0"));
    let exponent: i32 = exponent.parse().unwrap_or(0);
    let mut body = mantissa.to_string();
    push_repeated(&mut body, '0', precision - shown)?;
    if alt && precision == 0 {
        body.push('.');
    }
    let exp_sign = if exponent < 0 { '-' } else { '+' };
    body.push_str(&format!("e{exp_sign}{:02}", exponent.unsigned_abs()));
    Ok(body)
}

/// VALUE under SPEC's integer conversion: `%d`, `%x`, `%X` or `%o`.
fn integer(spec: &Spec, value: i64) -> Result<Field, Error> {
    // In Emacs a precision pads with zeros, so one whose digits overflowed is
    // too wide for any string there.
    if spec.precision == Some(usize::MAX) {
        return Err(string_too_long());
    }
    let magnitude = value.unsigned_abs();
    let (digits, alt_prefix) = match spec.conversion {
        'd' => (magnitude.to_string(), ""),
        'x' => (format!("{magnitude:x}"), "0x"),
        'X' => (format!("{magnitude:X}"), "0X"),
        'o' => (format!("{magnitude:o}"), "0"),
        other => return Err(invalid_operation(other)),
    };
    let mut body = String::new();
    match spec.precision {
        Some(0) if value == 0 => {}
        Some(precision) => {
            push_repeated(&mut body, '0', precision.saturating_sub(digits.len()))?;
            body.push_str(&digits);
        }
        None => body = digits,
    }
    // `#` puts `0x` before a hex number that is not zero, and `0` before octal
    // digits that do not start with 0.
    let wants_prefix = if alt_prefix == "0" {
        !body.starts_with('0')
    } else {
        value != 0
    };
    Ok(Field {
        sign: spec.sign(value < 0),
        prefix: if spec.alt && wants_prefix {
            alt_prefix
        } else {
            ""
        },
        body,
        // A precision gives the digits; the width pads with spaces.
        zero_pad: spec.precision.is_none(),
    })
}

/// The first PRECISION characters of TEXT, or all of it with no precision.
fn cut(text: String, precision: Option<usize>) -> String {
    match precision {
        Some(precision) => text.chars().take(precision).collect(),
        None => text,
    }
}

/// Writes FIELD to OUT, padded to SPEC's width: with zeros after the sign when
/// SPEC asks for them and FIELD takes them, or else with spaces.
fn pad(out: &mut String, spec: &Spec, field: &Field) -> Result<(), Error> {
    let len = field.sign.len() + field.prefix.len() + field.body.chars().count();
    let fill = spec.width.saturating_sub(len);
    if spec.left {
        out.push_str(field.sign);
        out.push_str(field.prefix);
        out.push_str(&field.body);
        push_repeated(out, ' ', fill)
    } else if spec.zero && field.zero_pad {
        out.push_str(field.sign);
        out.push_str(field.prefix);
        push_repeated(out, '0', fill)?;
        out.push_str(&field.body);
        Ok(())
    } else {
        push_repeated(out, ' ', fill)?;
        out.push_str(field.sign);
        out.push_str(field.prefix);
        out.push_str(&field.body);
        Ok(())
    }
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
            r#""Hello, world! %22 22.800000 10 10""#,
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

    // `%f` shows 6 digits by default, and NaN prints as `nan`, as in Emacs.
    #[test]
    fn format_f_defaults_to_six_digits() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(format "%f" 1.5)"#, r#""1.500000""#);
        eval_assert_equal(ctx, r#"(format "%f" 3)"#, r#""3.000000""#);
        eval_assert_equal(ctx, r#"(format "%f" -0.0)"#, r#""-0.000000""#);
        eval_assert_equal(
            ctx,
            r#"(format "%f|%f" 0.0e+NaN -0.0e+NaN)"#,
            r#""nan|-nan""#,
        );
    }

    // Zero padding goes after the sign, and not into `inf` or `nan`.
    #[test]
    fn zero_padding_goes_after_the_sign() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(format "%05d" -5)"#, r#""-0005""#);
        eval_assert_equal(ctx, r#"(format "%08.2f" -1.5)"#, r#""-0001.50""#);
        eval_assert_equal(ctx, r#"(format "%05.1f" -0.0)"#, r#""-00.0""#);
        eval_assert_equal(ctx, r#"(format "%-05d|" -5)"#, r#""-5   |""#);
        eval_assert_equal(
            ctx,
            r#"(format "%08f|%5f|" -1.0e+INF 1.0e+INF)"#,
            r#""    -inf|  inf|""#,
        );
        eval_assert_equal(ctx, r#"(format "%08f" 0.0e+NaN)"#, r#""     nan""#);
        eval_assert_equal(ctx, r#"(format "%05s|" "ab")"#, r#""   ab|""#);
    }

    // A precision keeps the first characters of `%s` and `%S`, and gives `%d`
    // at least that many digits, as in Emacs.
    #[test]
    fn format_precision_cuts_text_and_pads_digits() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (r#"(format "%.2s" "hello")"#, r#""he""#),
            (
                r#"(format "%5.2s|%-5.2s|" "hello" "hello")"#,
                r#""   he|he   |""#,
            ),
            (r#"(format "%.2S" "hello")"#, r#""\"h""#),
            (r#"(format "%.2s|%.2s" 'symbol 1.5)"#, r#""sy|1.""#),
            (r#"(format "%.1s" "éa")"#, r#""é""#),
            (r#"(format "%.3d|%.3d" 5 -5)"#, r#""005|-005""#),
            (
                r#"(format "%6.3d|%06.3d|%-6.3d|" 5 5 -5)"#,
                r#""   005|   005|-005  |""#,
            ),
            (r#"(format "%.0d|%.0d" 0 5)"#, r#""|5""#),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
    }

    // The `+` and space flags sign a number that is not negative.
    #[test]
    fn format_sign_flags() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (
                r#"(format "%+d %+d % d % d" 5 -5 5 -5)"#,
                r#""+5 -5  5 -5""#,
            ),
            (
                r#"(format "%+f|% f|%+.0f" 1.5 1.5 0.0)"#,
                r#""+1.500000| 1.500000|+0""#,
            ),
            (
                r#"(format "%+f|% f|%+f" 1.0e+INF 0.0e+NaN -1.0e+INF)"#,
                r#""+inf| nan|-inf""#,
            ),
            (
                r#"(format "%+ d|%-+6d|%+05d|% 05d|" 5 5 5 5)"#,
                r#""+5|+5    |+0005| 0005|""#,
            ),
            (r#"(format "%+s|% S" "a" 5)"#, r#""a|5""#),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
    }

    // `%x`, `%X` and `%o` print an integer in hex or octal, with `0x`, `0X` or
    // a leading `0` under the `#` flag, as in Emacs.
    #[test]
    fn format_hex_and_octal() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (r#"(format "%x %X %o" 255 255 8)"#, r#""ff FF 10""#),
            (r#"(format "%x|%o|%x" -255 -8 255.9)"#, r#""-ff|-10|ff""#),
            (r#"(format "%X" 3735928559)"#, r#""DEADBEEF""#),
            (
                r#"(format "%#x %#X %#o|%#x|%#o" 255 255 8 0 0)"#,
                r#""0xff 0XFF 010|0|0""#,
            ),
            (
                r#"(format "%08x|%-8x|%.4x|%+x" 255 255 255 255)"#,
                r#""000000ff|ff      |00ff|+ff""#,
            ),
            (
                r#"(format "%#08x|%#.3o|%#5o|%-#6x|%#.0x|" 255 8 8 255 0)"#,
                r#""0x0000ff|010|  010|0xff  ||""#,
            ),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
        eval_assert_error_line(
            ctx,
            r#"(format "%x" "a")"#,
            "ERR LispError: Format specifier doesn\u{2019}t match argument type",
        );
    }

    // `%e` prints a float with one digit before the point and a signed exponent
    // of at least two digits, as C and Emacs do.
    #[test]
    fn format_exponent_form() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (
                r#"(format "%e|%.2e|%e" 12345.678 12345.678 0)"#,
                r#""1.234568e+04|1.23e+04|0.000000e+00""#,
            ),
            (
                r#"(format "%#e|%#.2e" 1.5 1.5)"#,
                r#""1.500000e+00|1.50e+00""#,
            ),
            (
                r#"(format "%e|%e|%e" 1e300 1e-300 5e-324)"#,
                r#""1.000000e+300|1.000000e-300|4.940656e-324""#,
            ),
            (
                r#"(format "%.0e|%.0e|%.0e|%.3e" 15.0 0.5 2.5 9.9996)"#,
                r#""2e+01|5e-01|2e+00|1.000e+01""#,
            ),
            (
                r#"(format "%012.3e|%+e|%e" -1.5 1.5 -0.0)"#,
                r#""-001.500e+00|+1.500000e+00|-0.000000e+00""#,
            ),
            (
                r#"(format "%e|%08e" 1.0e+INF 0.0e+NaN)"#,
                r#""inf|     nan""#,
            ),
            (r#"(format "%#.0e|%.0e" 2.0 2.0)"#, r#""2.e+00|2e+00""#),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
        eval_assert_error_line(
            ctx,
            r#"(format "%E" 1.5)"#,
            "ERR LispError: Invalid format operation %E",
        );
    }

    // The `#` flag keeps the point of `%.0f`, as in Emacs.
    #[test]
    fn format_alt_keeps_the_point() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(format "%#.0f|%.0f" 2.0 2.0)"#, r#""2.|2""#);
    }
}
