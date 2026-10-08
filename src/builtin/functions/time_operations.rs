use crate::{Error, Number, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("current-time", || {
        let usec_since_epoch = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as i64;
        Ok(TulispObject::cons(
            usec_since_epoch.into(),
            1_000_000_000.into(),
        ))
    });

    ctx.defun("time-less-p", |t1: TulispObject, t2: TulispObject| {
        time_operation(t1, t2, |a, b, _| (a < b).into())
    });

    ctx.defun("time-equal-p", |t1: TulispObject, t2: TulispObject| {
        time_operation(t1, t2, |a, b, _| (a == b).into())
    });

    ctx.defun("time-subtract", |t1: TulispObject, t2: TulispObject| {
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a - b).into(), hz.into())
        })
    });

    ctx.defun("time-add", |t1: TulispObject, t2: TulispObject| {
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a + b).into(), hz.into())
        })
    });

    fn time_operation(
        t1: TulispObject,
        t2: TulispObject,
        op: impl Fn(i64, i64, i64) -> TulispObject,
    ) -> Result<TulispObject, Error> {
        let (ticks1, hz1) = ticks_hz_from_obj(&t1)?;
        let (ticks2, hz2) = ticks_hz_from_obj(&t2)?;

        if hz1 == hz2 {
            Ok(op(ticks1, ticks2, hz1))
        } else if hz1 > hz2 {
            let factor = hz1 / hz2;
            Ok(op(ticks1, ticks2 * factor, hz1))
        } else {
            let factor = hz2 / hz1;
            Ok(op(ticks1 * factor, ticks2, hz2))
        }
    }

    ctx.defun("format-seconds", format_seconds);
}

/// The `(TICKS . HZ)` of OBJ, an integer number of seconds or such a pair. As
/// in Emacs, an HZ that is not positive is an invalid time.
fn ticks_hz_from_obj(obj: &TulispObject) -> Result<(i64, i64), Error> {
    if obj.integerp() {
        if let Ok(ticks) = obj.as_int() {
            Ok((ticks, 1))
        } else {
            Err(Error::type_mismatch("expected integer".to_string()).with_trace(obj.clone()))
        }
    } else if let Some(cons) = obj.as_list_cons() {
        if let (Ok(ticks), Ok(hz)) = (cons.car().as_int(), cons.cdr().as_int()) {
            if hz <= 0 {
                return Err(Error::lisp_error("Invalid time specification".to_string())
                    .with_trace(obj.clone()));
            }
            Ok((ticks, hz))
        } else {
            Err(
                Error::type_mismatch("expected (ticks . hz) pair".to_string())
                    .with_trace(obj.clone()),
            )
        }
    } else {
        Err(Error::type_mismatch(format!(
            "expected integer or (ticks . hz) pair. found: {obj}"
        )))
    }
}

/// The units `format-seconds` knows, as Emacs lists them: the letter, the
/// unit's name and its length in seconds.
const UNITS: [(char, &str, i64); 5] = [
    ('y', "year", 31_536_000),
    ('d', "day", 86_400),
    ('h', "hour", 3_600),
    ('m', "minute", 60),
    ('s', "second", 1),
];

/// The `format-seconds` flags, which print nothing: `%z` cuts the text before
/// the first non-zero unit, but no further than `%z`, and `%x` cuts the text
/// from the first of the trailing zero units to the end, wherever `%x` stands.
/// When every unit is zero, both follow Emacs's `format-seconds`.
const FLAGS: [char; 2] = ['z', 'x'];

/// Emacs's `format-seconds`, step by step as Emacs 30 does it: the specs are
/// checked first, then each unit's first spec is replaced by its part of
/// SECONDS. Then `%z` and `%x` cut the text, as [`FLAGS`] says, and the result
/// is trimmed of whitespace, as in Emacs.
fn format_seconds(format: String, seconds: TulispObject) -> Result<String, Error> {
    let mut text: Vec<char> = format.chars().collect();
    check_specs(&text)?;
    let (mut whole, fraction) = whole_seconds(&seconds)?;
    let mut leading_zero_pos: Option<usize> = None;
    let mut trailing_zero_pos: Option<usize> = None;
    for (letter, name, length) in UNITS {
        let Some(found) = unit_spec(&text, letter) else {
            continue;
        };
        let count = whole.div_euclid(length);
        whole = whole.rem_euclid(length);
        let value = if length == 1
            && let Some(fraction) = fraction
        {
            Number::Float(count as f64 + fraction)
        } else {
            Number::Int(count)
        };
        let is_zero = value == Number::Int(0);
        if leading_zero_pos.is_none() && !is_zero {
            leading_zero_pos = Some(found.start);
        }
        if !is_zero {
            trailing_zero_pos = None;
        } else if trailing_zero_pos.is_none() {
            trailing_zero_pos = Some(found.start);
        }
        let mut formatted = format_unit(value, &found)?;
        if found.named {
            formatted.push_str(&format!(" {name}{}", if count == 1 { "" } else { "s" }));
        }
        text.splice(found.start..found.end, formatted.chars());
    }
    // Emacs looks at the flags after the units, in this order.
    let chop_leading = unit_spec(&text, 'z').map(|found| match leading_zero_pos {
        Some(pos) => pos.min(found.start),
        None => found.start + 2,
    });
    let chop_trailing = unit_spec(&text, 'x').is_some();
    let before = text.clone();
    if chop_trailing && let Some(pos) = trailing_zero_pos {
        text.truncate(pos);
    }
    if let Some(pos) = chop_leading {
        if pos > text.len() {
            return Err(Error::out_of_range(format!(
                "format-seconds: {pos} past the end of the text"
            )));
        }
        text.drain(..pos);
    }
    if text.is_empty() {
        text = before;
    }
    // Drop the `%z` and `%x` flags, then turn `%%` into `%`.
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < text.len() {
        if text[i] == '%'
            && text
                .get(i + 1)
                .is_some_and(|c| FLAGS.contains(&c.to_ascii_lowercase()))
        {
            i += 2;
            continue;
        }
        out.push(text[i]);
        i += 1;
    }
    let out = out.replace("%%", "%");
    Ok(out
        .trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r'))
        .to_string())
}

/// Checks each `%` spec of TEXT, as Emacs does before it formats anything: a
/// known unit or `%`, no unit twice, and units in decreasing size when a `%z`
/// or `%x` is there.
fn check_specs(text: &[char]) -> Result<(), Error> {
    let mut used: Vec<char> = Vec::new();
    let mut flag = false;
    let mut larger = false;
    let mut prev: Option<i64> = None;
    let mut start = 0;
    while let Some((end, spec)) =
        (start..text.len()).find_map(|at| (text[at] == '%').then(|| spec_at(text, at)).flatten())
    {
        start = end;
        if spec == '%' {
            continue;
        }
        let lower = spec.to_ascii_lowercase();
        let unit = UNITS.iter().find(|unit| unit.0 == lower);
        if unit.is_none() && !FLAGS.contains(&lower) {
            return Err(Error::lisp_error(format!(
                "Bad format specifier: \u{2018}{spec}\u{2019}"
            )));
        }
        if used.contains(&lower) {
            return Err(Error::lisp_error(format!(
                "Multiple instances of specifier: \u{2018}{spec}\u{2019}"
            )));
        }
        match unit {
            None => flag = true,
            Some(&(_, _, length)) => {
                if !larger {
                    larger = prev.is_some_and(|prev| length > prev);
                    prev = Some(length);
                }
            }
        }
        used.push(lower);
    }
    if flag && larger {
        return Err(Error::lisp_error(
            "Units are not in decreasing order of size".to_string(),
        ));
    }
    Ok(())
}

/// Where the spec that starts with the `%` at AT in TEXT ends, and its letter,
/// as Emacs's `%\.?[0-9]*\(,[0-9]\)?\(.\)` matches it, trying the longest parts
/// first.
fn spec_at(text: &[char], at: usize) -> Option<(usize, char)> {
    for dot in [true, false] {
        let after_dot = if dot {
            if text.get(at + 1) != Some(&'.') {
                continue;
            }
            at + 2
        } else {
            at + 1
        };
        for digits in (0..=digits_at(text, after_dot)).rev() {
            let after_digits = after_dot + digits;
            for comma in [true, false] {
                let after_comma = if comma {
                    if text.get(after_digits) != Some(&',')
                        || !text.get(after_digits + 1).is_some_and(char::is_ascii_digit)
                    {
                        continue;
                    }
                    after_digits + 2
                } else {
                    after_digits
                };
                if let Some(&spec) = text.get(after_comma)
                    && spec != '\n'
                {
                    return Some((after_comma + 1, spec));
                }
            }
        }
    }
    None
}

/// A unit's spec in the text: where it is, its width and its decimals.
struct UnitSpec {
    start: usize,
    end: usize,
    /// `N` or `.N` before the letter.
    width: Option<String>,
    /// The digits after a `,`.
    decimals: Option<String>,
    /// Whether the letter is upper case, so the unit's name follows.
    named: bool,
}

/// The first spec in TEXT for the unit LETTER, as Emacs's
/// `%\(\.?[0-9]+\)?\(,[0-9]+\)?\(LETTER\)` finds it, in either case. LETTER
/// is no digit, `.` or `,`, so the longest parts are the only ones that can
/// match.
fn unit_spec(text: &[char], letter: char) -> Option<UnitSpec> {
    (0..text.len())
        .filter(|&at| text[at] == '%')
        .find_map(|at| {
            let dot = usize::from(text.get(at + 1) == Some(&'.'));
            let digits = digits_at(text, at + 1 + dot);
            let width_end = if digits > 0 {
                at + 1 + dot + digits
            } else {
                at + 1
            };
            let mut after = width_end;
            let mut decimals = None;
            if text.get(after) == Some(&',') {
                let digits = digits_at(text, after + 1);
                if digits > 0 {
                    decimals = Some(text[after + 1..after + 1 + digits].iter().collect());
                    after += 1 + digits;
                }
            }
            let found = *text.get(after)?;
            (found.to_ascii_lowercase() == letter).then(|| UnitSpec {
                start: at,
                end: after + 1,
                width: (width_end > at + 1).then(|| text[at + 1..width_end].iter().collect()),
                decimals,
                named: found != letter,
            })
        })
}

/// How many ASCII digits TEXT has from I on.
fn digits_at(text: &[char], i: usize) -> usize {
    text.get(i..)
        .unwrap_or_default()
        .iter()
        .take_while(|c| c.is_ascii_digit())
        .count()
}

/// VALUE written for SPEC by Emacs's `format`, with the spec `format-seconds`
/// gives it: `%W.Nf` with decimals, a `.` in the width becoming a `0`, or else
/// `%Wd`, which drops a fraction.
fn format_unit(value: Number, spec: &UnitSpec) -> Result<String, Error> {
    let width = spec.width.as_deref().unwrap_or("");
    let format = match &spec.decimals {
        Some(decimals) => {
            let width = match width.strip_prefix('.') {
                Some(digits) => format!("0{digits}"),
                None => width.to_string(),
            };
            format!("%{width}.{decimals}f")
        }
        None => format!("%{width}d"),
    };
    super::format::format_string(&format, [TulispObject::from(value)])
}

/// SECONDS rounded to a whole number, ties to even, and its fraction, as Emacs
/// takes them: an integer has no fraction, and a float or a `(TICKS . HZ)` time
/// has the part above its floor.
fn whole_seconds(seconds: &TulispObject) -> Result<(i64, Option<f64>), Error> {
    if seconds.integerp() {
        return Ok((seconds.as_int()?, None));
    }
    let x = if seconds.floatp() {
        seconds.as_float()?
    } else {
        let (ticks, hz) = ticks_hz_from_obj(seconds)?;
        ticks as f64 / hz as f64
    };
    let whole = crate::number::f64_to_i64_checked(x.round_ties_even(), "format-seconds")?;
    Ok((whole, Some(x - x.floor())))
}

#[cfg(test)]
mod tests {
    use crate::{
        Error, TulispContext,
        test_utils::{
            assert_results, eval_assert, eval_assert_equal, eval_assert_error_line, eval_assert_not,
        },
    };

    #[test]
    fn test_current_time() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        super::add(&mut ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();

        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as i64;

        let now_minus_10ms = now - 10_000_000;

        assert!(t1.car()?.as_int()? <= now);
        assert!(t1.car()?.as_int()? > now_minus_10ms);

        assert_eq!(t1.cdr()?.as_int()?, 1_000_000_000);

        Ok(())
    }

    #[test]
    fn test_time_less_p() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        super::add(ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();
        let t2 = ctx.eval_string("(current-time)").unwrap();

        eval_assert(ctx, &format!("(time-less-p '{} '{})", t1, t2));

        eval_assert_not(ctx, &format!("(time-less-p '{} '{})", t2, t1));

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506644000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506645 . 1000000) '(1758549821506644000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506645 . 1000000) '(1758549821506646000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-less-p 1758549821 '(1758549821506646000 . 1000000000))",
        );

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) 1758549822)",
        );

        eval_assert_not(
            ctx,
            "(time-less-p 1758549822 '(1758549821506646000 . 1000000000))",
        );

        assert_eq!(
            ctx.eval_string("(time-less-p '(test . 10) 1758549822)")
                .unwrap_err()
                .to_string(),
            r#"ERR TypeMismatch: expected (ticks . hz) pair
<eval_string>:1.15-1.25:  at (test . 10)
<eval_string>:1.1-1.37:  at (time-less-p '(test . 10) 1758549822)"#
        );

        assert_eq!(
            ctx.eval_string("(time-less-p 'test 1758549822)")
                .unwrap_err()
                .to_string(),
            r#"ERR TypeMismatch: expected integer or (ticks . hz) pair. found: test
<eval_string>:1.1-1.30:  at (time-less-p 'test 1758549822)"#
        );

        Ok(())
    }

    #[test]
    fn test_time_equal_p() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();
        let t2 = ctx.eval_string("(current-time)").unwrap();

        eval_assert(ctx, &format!("(time-equal-p '{} '{})", t1, t1));

        eval_assert_not(ctx, &format!("(time-equal-p '{} '{})", t1, t2));

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821506645000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) '(1758549821506645000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645001 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) '(1758549821506645001 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821000000 . 1000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-equal-p 1758549821 '(1758549821000000 . 1000000))",
        );

        Ok(())
    }

    #[test]
    fn test_time_add_subtract() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        let t1 = "(1758549821506645000 . 1000000000)";

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1000 . 1000000000))"),
            "'(1758549821506646000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1 . 1000000))"),
            "'(1758549821506646000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1 . 1))"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add 1 '{t1})"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} 1)"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1000 . 1000000000))"),
            "'(1758549821506644000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1 . 1000000))"),
            "'(1758549821506644000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1 . 1))"),
            "'(1758549820506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} 1)"),
            "'(1758549820506645000 . 1000000000)",
        );

        Ok(())
    }

    #[test]
    fn format_seconds_with_a_bad_spec_is_an_error() {
        let ctx = &mut TulispContext::new();
        super::add(ctx);
        for (program, text) in [
            (
                r#"(format-seconds "%q" 1)"#,
                "Bad format specifier: \u{2018}q\u{2019}",
            ),
            // A `.` or `,` anywhere but first in a spec is a bad spec.
            (
                r#"(format-seconds "%1.y" 1)"#,
                "Bad format specifier: \u{2018}.\u{2019}",
            ),
            (
                r#"(format-seconds "%1,y" 1)"#,
                "Bad format specifier: \u{2018},\u{2019}",
            ),
            (
                r#"(format-seconds "%..y" 1)"#,
                "Bad format specifier: \u{2018}.\u{2019}",
            ),
            (
                r#"(format-seconds "%,,y" 1)"#,
                "Bad format specifier: \u{2018},\u{2019}",
            ),
        ] {
            eval_assert_error_line(ctx, program, &format!("ERR LispError: {text}"));
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {program} (error (car e)))"),
                "'error",
            );
        }
    }

    #[test]
    fn format_seconds_pads_with_zeros_after_a_dot() {
        let ctx = &mut TulispContext::new();
        super::add(ctx);
        // As Emacs 30 gives them.
        eval_assert_equal(ctx, r#"(format-seconds "%.2s" 5)"#, r#""05""#);
        eval_assert_equal(ctx, r#"(format-seconds "%.3h" 3600)"#, r#""001""#);
    }

    // Each result is the one Emacs 30 gives.
    #[test]
    fn format_seconds_follows_emacs() {
        let ctx = &mut TulispContext::new();
        super::add(ctx);
        for (spec, seconds, expected) in [
            ("%1,2s", "3661.5", "3662.50"),
            ("%,2s", "3661.5", "3662.50"),
            ("%2s", "3661.5", "3662"),
            ("%.2,1s", "3661.5", "3662.5"),
            ("%y %d %h %m %s", "3661.5", "0 0 1 1 2"),
            ("%.s", "3661.5", "%.s"),
            ("%5d", "3661.5", "0"),
            ("%.5d", "3661.5", "00000"),
            (
                "%Y, %D, %H, %M, %z%S",
                "3661.5",
                "1 hour, 1 minute, 2 seconds",
            ),
            ("%Y, %D, %H, %M, %z%S", "90", "1 minute, 30 seconds"),
            ("%Y, %D, %H, %M, %z%S", "0", "0 seconds"),
            ("%3z", "3661.5", "z"),
            ("%%", "3661.5", "%"),
            ("%x", "3661.5", ""),
            ("%S", "3661.5", "3662 seconds"),
            ("%m:%s", "3661", "61:1"),
            ("%h:%m:%x%s", "7200", "2:"),
            ("%S", "1", "1 second"),
            ("%s", "1.5", "2"),
            ("%s", "2.5", "2"),
            ("%,1s", "2.25", "2.2"),
            ("%s", "-61", "-61"),
            ("%m %s", "-61", "-2 59"),
            ("%,2s", "'(1 . 4)", "0.25"),
            ("%s", "'(10 . 4)", "2"),
            ("%%s", "5", "%5"),
            ("%.3S", "1", "001 second"),
            ("%5s", "5", "5"),
            ("a %5s b", "5", "a     5 b"),
            ("%3,1s", "2.25", "2.2"),
            ("%.4,1s", "2.25", "02.2"),
            ("%Z%s", "1", "1"),
            ("%.3d", "-1", "-001"),
            ("%.0s", "0", ""),
            ("%h%x", "0", "0"),
            ("%z%h %m", "60", "0 1"),
            ("%y %s", "-9223372036854775807", "-292471208678 14632193"),
            (
                "%y %d %h %m %s",
                "9223372036854775807",
                "292471208677 195 15 30 7",
            ),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(format-seconds {spec:?} {seconds})"),
                &format!("{expected:?}"),
            );
        }
        for (spec, message) in [
            ("%,s", "Bad format specifier: \u{2018},\u{2019}"),
            ("%,", "Bad format specifier: \u{2018},\u{2019}"),
            ("%a", "Bad format specifier: \u{2018}a\u{2019}"),
            ("%5", "Bad format specifier: \u{2018}5\u{2019}"),
            ("%.", "Bad format specifier: \u{2018}.\u{2019}"),
            ("%y%y", "Multiple instances of specifier: \u{2018}y\u{2019}"),
            ("%s%z%h", "Units are not in decreasing order of size"),
            ("%99999999999999999999999s", "Maximum string size exceeded"),
        ] {
            eval_assert_error_line(
                ctx,
                &format!("(format-seconds {spec:?} 1)"),
                &format!("ERR LispError: {message}"),
            );
        }
        eval_assert_equal(
            ctx,
            r#"(condition-case e (format-seconds "%h%x%z" 0) (error (car e)))"#,
            "'args-out-of-range",
        );
        assert_results(&[
            (
                r#"(format-seconds "%s" '(5 . -1))"#,
                r#"(ERR (error "Invalid time specification"))"#,
            ),
            (
                "(time-add '(1 . 2) '(1 . 0))",
                r#"(ERR (error "Invalid time specification"))"#,
            ),
        ]);
    }

    #[test]
    fn test_format_seconds() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%y years, %d days, %h hours, %m minutes, %s seconds" '(31536061 . 1))"#,
            r#""1 years, 0 days, 0 hours, 1 minutes, 1 seconds""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63072000 . 1))"#,
            r#""2y 0d 0:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63115200 . 1))"#,
            r#""2y 0d 12:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63115201 . 1))"#,
            r#""2y 0d 12:0:1""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158400 . 1))"#,
            r#""2y 1d 0:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158401 . 1))"#,
            r#""2y 1d 0:0:1""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158461 . 1))"#,
            r#""2y 1d 0:1:1""#,
        );

        Ok(())
    }
}
