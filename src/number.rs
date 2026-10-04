use std::fmt::Display;

use crate::{Error, TulispObject};

/// A Lisp number: an integer or a float. A `defun` parameter or return value of
/// this type takes or gives either kind.
///
/// `==` and `<` compare as Lisp's `=` and `<` do, across the two kinds, so
/// `Number::Int(1) == Number::Float(1.0)`; they also compare with an `i64` or
/// an `f64`. The `checked_*` methods do arithmetic as the Lisp operators do,
/// with an error on integer overflow.
///
/// ```rust
/// use tulisp::Number;
///
/// assert_eq!(Number::Int(1), Number::Float(1.0));
/// assert_eq!(Number::Int(7).checked_div(Number::Int(2)).unwrap(), 3);
/// ```
#[derive(Debug, Clone, Copy)]
pub enum Number {
    /// An integer.
    Int(i64),
    /// A float.
    Float(f64),
}

impl Default for Number {
    fn default() -> Self {
        Number::Int(0)
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Number::Int(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Number::Float(value)
    }
}

impl TryFrom<&TulispObject> for Number {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value.as_number()
    }
}

impl TryFrom<TulispObject> for Number {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        Number::try_from(&value)
    }
}

impl From<Number> for TulispObject {
    fn from(value: Number) -> Self {
        match value {
            Number::Int(v) => v.into(),
            Number::Float(v) => v.into(),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(v) => write!(f, "{}", v),
            // Match Emacs' float printer:
            // - `f64::INFINITY` / `NEG_INFINITY` print as
            //   `1.0e+INF` / `-1.0e+INF`
            // - NaN prints with the sign bit reflected in the
            //   leading mantissa: `0.0e+NaN` (positive bit) or
            //   `-0.0e+NaN` (negative bit). Both still round-trip
            //   through `read` because Emacs accepts the same forms.
            // - Whole-value finite floats keep the trailing `.0`
            //   via `{:?}` (`2.0` => `"2.0"`, not `"2"`).
            Number::Float(v) => {
                if v.is_infinite() {
                    if *v < 0.0 {
                        f.write_str("-1.0e+INF")
                    } else {
                        f.write_str("1.0e+INF")
                    }
                } else if v.is_nan() {
                    if v.is_sign_negative() {
                        f.write_str("-0.0e+NaN")
                    } else {
                        f.write_str("0.0e+NaN")
                    }
                } else {
                    write!(f, "{:?}", v)
                }
            }
        }
    }
}

/// Convert `value` to `i64`, raising `ArithError` for NaN, ±inf,
/// or values outside `i64`'s range. `f64 as i64` saturates these
/// silently — for `truncate` / `floor` / `ceiling` / `round` and
/// the `try_int` extractor, the saturated sentinel is misleading.
/// `op` names the caller for the error message.
#[inline]
pub(crate) fn f64_to_i64_checked(value: f64, op: &str) -> Result<i64, Error> {
    if !value.is_finite() {
        return Err(Error::arith_error(format!(
            "{op}: cannot convert {value} to integer"
        )));
    }
    // `i64::MAX as f64` rounds up to `9.223…e18`, so the
    // representable bound is `< MAX_F64`. `MIN_F64` is exact
    // (`-2^63` is exactly representable in f64).
    const I64_MAX_F64: f64 = 9.223372036854776e18;
    const I64_MIN_F64: f64 = -9.223372036854776e18;
    if !(I64_MIN_F64..I64_MAX_F64).contains(&value) {
        return Err(Error::arith_error(format!(
            "{op}: float {value} out of range for integer"
        )));
    }
    Ok(value as i64)
}

/// Floored remainder for floats — `a - b*floor(a/b)`, taking the
/// divisor's sign, matching Emacs `mod`. A zero divisor yields NaN
/// (the comparisons below are all false for NaN, so it's returned
/// unadjusted), which Emacs also surfaces for float `mod`.
#[inline]
fn floor_mod_f64(a: f64, b: f64) -> f64 {
    let m = a % b;
    if m != 0.0 && (m < 0.0) != (b < 0.0) {
        m + b
    } else {
        m
    }
}

impl Number {
    /// Adds, as Emacs `+` does. An integer and a float add as floats.
    ///
    /// Returns an `ArithError` when two integers overflow, where Emacs
    /// would make a bignum. Floats give `inf` instead.
    ///
    /// ```rust
    /// use tulisp::Number;
    ///
    /// assert_eq!(
    ///     Number::Int(2).checked_add(Number::Float(0.5)).unwrap(),
    ///     Number::Float(2.5)
    /// );
    /// assert!(Number::Int(i64::MAX).checked_add(Number::Int(1)).is_err());
    /// ```
    pub fn checked_add(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_add(r)
                .map(Number::Int)
                .ok_or_else(|| Error::arith_error(format!("integer overflow: {} + {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 + r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l + r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l + r)),
        }
    }

    /// Subtracts RHS, as Emacs `-` does. Returns an `ArithError` when two
    /// integers overflow.
    pub fn checked_sub(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_sub(r)
                .map(Number::Int)
                .ok_or_else(|| Error::arith_error(format!("integer overflow: {} - {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 - r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l - r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l - r)),
        }
    }

    /// Multiplies, as Emacs `*` does. Returns an `ArithError` when two
    /// integers overflow.
    pub fn checked_mul(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_mul(r)
                .map(Number::Int)
                .ok_or_else(|| Error::arith_error(format!("integer overflow: {} * {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 * r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l * r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l * r)),
        }
    }

    /// The same number, as a `Number::Float`.
    pub(crate) fn to_float(self) -> Number {
        match self {
            Number::Int(value) => Number::Float(value as f64),
            Number::Float(_) => self,
        }
    }

    /// Divides by RHS, as Emacs `/` does: two integers give an integer,
    /// truncated toward zero.
    ///
    /// Returns an `ArithError` when both are integers and RHS is 0, or on
    /// `i64::MIN / -1`. With a float operand, a zero divisor gives an
    /// infinity or NaN, as in Emacs.
    pub fn checked_div(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(_), Number::Int(0)) => {
                Err(Error::arith_error("Division by zero".to_string()))
            }
            (Number::Int(l), Number::Int(r)) => l
                .checked_div(r)
                .map(Number::Int)
                .ok_or_else(|| Error::arith_error(format!("integer overflow: {} / {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 / r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l / r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l / r)),
        }
    }

    /// Floored modulo, as Emacs `mod`: the result takes the divisor's
    /// sign (`(mod -7 3)` => 2, `(mod 7 -3)` => -2).
    ///
    /// Returns an `ArithError` when both are integers and RHS is 0. With
    /// a float operand, a zero divisor gives NaN.
    pub fn checked_mod(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(_), Number::Int(0)) => {
                Err(Error::arith_error("Division by zero".to_string()))
            }
            // `i64::MIN % -1` overflows; any integer mod -1 is 0.
            (Number::Int(_), Number::Int(-1)) => Ok(Number::Int(0)),
            (Number::Int(l), Number::Int(r)) => {
                let m = l % r;
                let m = if m != 0 && (m < 0) != (r < 0) {
                    m + r
                } else {
                    m
                };
                Ok(Number::Int(m))
            }
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(floor_mod_f64(l as f64, r))),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(floor_mod_f64(l, r as f64))),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(floor_mod_f64(l, r))),
        }
    }
}

impl Number {
    /// Same kind and same value, as Emacs `eql` compares numbers.
    /// Floats compare by bit pattern: `0.0` and `-0.0` differ, and a
    /// NaN equals itself. `PartialEq` is the cross-kind `=`.
    pub(crate) fn eql(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::Int(l), Number::Int(r)) => l == r,
            (Number::Float(l), Number::Float(r)) => l.to_bits() == r.to_bits(),
            _ => false,
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Int(l), Number::Int(r)) => l == r,
            (Number::Int(l), Number::Float(r)) => (*l as f64) == *r,
            (Number::Float(l), Number::Int(r)) => *l == (*r as f64),
            (Number::Float(l), Number::Float(r)) => l == r,
        }
    }
}

impl PartialEq<i64> for Number {
    fn eq(&self, other: &i64) -> bool {
        match self {
            Number::Int(l) => l == other,
            Number::Float(l) => *l == (*other as f64),
        }
    }
}

impl PartialEq<f64> for Number {
    fn eq(&self, other: &f64) -> bool {
        match self {
            Number::Int(l) => (*l as f64) == *other,
            Number::Float(l) => l == other,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Int(l), Number::Int(r)) => l.partial_cmp(r),
            (Number::Int(l), Number::Float(r)) => (*l as f64).partial_cmp(r),
            (Number::Float(l), Number::Int(r)) => l.partial_cmp(&(*r as f64)),
            (Number::Float(l), Number::Float(r)) => l.partial_cmp(r),
        }
    }
}

impl PartialOrd<i64> for Number {
    fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
        match self {
            Number::Int(l) => l.partial_cmp(other),
            Number::Float(l) => l.partial_cmp(&(*other as f64)),
        }
    }
}

impl PartialOrd<f64> for Number {
    fn partial_cmp(&self, other: &f64) -> Option<std::cmp::Ordering> {
        match self {
            Number::Int(l) => (*l as f64).partial_cmp(other),
            Number::Float(l) => l.partial_cmp(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Number;
    use crate::test_utils::eval_assert_equal;
    use crate::{TulispContext, TulispObject};

    // A value that is no number gives an error traced to it, by value and by
    // reference.
    #[test]
    fn try_from_traces_a_value_that_is_no_number() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("'((x))").unwrap().car().unwrap();
        let expected =
            "ERR TypeMismatch: Expected number, got: (x)\n<eval_string>:1.3-1.5:  at (x)";
        let err = Number::try_from(&list).unwrap_err().with_file_names(ctx);
        assert_eq!(err.to_string(), expected);
        let err = Number::try_from(list).unwrap_err().with_file_names(ctx);
        assert_eq!(err.to_string(), expected);
        assert_eq!(
            Number::try_from(&TulispObject::from(2.5)).unwrap(),
            Number::Float(2.5)
        );
    }

    // A float with a whole value prints with a trailing `.0`.
    #[test]
    fn a_whole_float_prints_with_a_point() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(format "%S" 1.0)"#, r#""1.0""#);
        eval_assert_equal(ctx, r#"(format "%S" (+ 1.0 1))"#, r#""2.0""#);
        eval_assert_equal(ctx, r#"(format "%S" 0.5)"#, r#""0.5""#);
    }
}
