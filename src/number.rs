use std::fmt::Display;

use crate::{Error, TulispObject, TulispValue};

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Int(i64),
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

impl TryFrom<TulispObject> for Number {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        match &value.inner_ref().0 {
            TulispValue::Number { value } => Ok(*value),
            _ => Err(Error::type_mismatch(format!(
                "Expected number, got: {}",
                value
            ))),
        }
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

/// Convert `value` to `i64`, raising `OutOfRange` for NaN, ±inf,
/// or values outside `i64`'s range. `f64 as i64` saturates these
/// silently — for `truncate` / `floor` / `ceiling` / `round` and
/// the `try_int` extractor, the saturated sentinel is misleading.
/// `op` names the caller for the error message.
#[inline]
pub(crate) fn f64_to_i64_checked(value: f64, op: &str) -> Result<i64, Error> {
    if !value.is_finite() {
        return Err(Error::out_of_range(format!(
            "{op}: cannot convert {value} to integer"
        )));
    }
    // `i64::MAX as f64` rounds up to `9.223…e18`, so the
    // representable bound is `< MAX_F64`. `MIN_F64` is exact
    // (`-2^63` is exactly representable in f64).
    const I64_MAX_F64: f64 = 9.223372036854776e18;
    const I64_MIN_F64: f64 = -9.223372036854776e18;
    if !(I64_MIN_F64..I64_MAX_F64).contains(&value) {
        return Err(Error::out_of_range(format!(
            "{op}: float {value} out of range for integer"
        )));
    }
    Ok(value as i64)
}

impl Number {
    /// Like `Add` but raises `OutOfRange` on `Int + Int` overflow
    /// instead of wrapping. Float operands fall through to `f64::add`,
    /// which produces `inf` rather than overflowing — matches Emacs
    /// (which would promote to bignum on integer overflow).
    pub(crate) fn checked_add(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_add(r)
                .map(Number::Int)
                .ok_or_else(|| Error::out_of_range(format!("integer overflow: {} + {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 + r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l + r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l + r)),
        }
    }

    /// `Sub` counterpart with `Int - Int` overflow detection.
    pub(crate) fn checked_sub(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_sub(r)
                .map(Number::Int)
                .ok_or_else(|| Error::out_of_range(format!("integer overflow: {} - {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 - r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l - r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l - r)),
        }
    }

    /// `Mul` counterpart with `Int * Int` overflow detection.
    pub(crate) fn checked_mul(self, rhs: Number) -> Result<Number, Error> {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => l
                .checked_mul(r)
                .map(Number::Int)
                .ok_or_else(|| Error::out_of_range(format!("integer overflow: {} * {}", l, r))),
            (Number::Int(l), Number::Float(r)) => Ok(Number::Float(l as f64 * r)),
            (Number::Float(l), Number::Int(r)) => Ok(Number::Float(l * r as f64)),
            (Number::Float(l), Number::Float(r)) => Ok(Number::Float(l * r)),
        }
    }
}

impl std::ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l + r),
            (Number::Int(l), Number::Float(r)) => Number::Float(l as f64 + r),
            (Number::Float(l), Number::Int(r)) => Number::Float(l + r as f64),
            (Number::Float(l), Number::Float(r)) => Number::Float(l + r),
        }
    }
}

impl std::ops::Add<i64> for Number {
    type Output = Number;

    fn add(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Int(l + rhs),
            Number::Float(l) => Number::Float(l + rhs as f64),
        }
    }
}

impl std::ops::Add<f64> for Number {
    type Output = Number;

    fn add(self, rhs: f64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Float(l as f64 + rhs),
            Number::Float(l) => Number::Float(l + rhs),
        }
    }
}

impl std::ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l - r),
            (Number::Int(l), Number::Float(r)) => Number::Float(l as f64 - r),
            (Number::Float(l), Number::Int(r)) => Number::Float(l - r as f64),
            (Number::Float(l), Number::Float(r)) => Number::Float(l - r),
        }
    }
}

impl std::ops::Sub<i64> for Number {
    type Output = Number;

    fn sub(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Int(l - rhs),
            Number::Float(l) => Number::Float(l - rhs as f64),
        }
    }
}

impl std::ops::Sub<f64> for Number {
    type Output = Number;

    fn sub(self, rhs: f64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Float(l as f64 - rhs),
            Number::Float(l) => Number::Float(l - rhs),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l * r),
            (Number::Int(l), Number::Float(r)) => Number::Float(l as f64 * r),
            (Number::Float(l), Number::Int(r)) => Number::Float(l * r as f64),
            (Number::Float(l), Number::Float(r)) => Number::Float(l * r),
        }
    }
}

impl std::ops::Mul<i64> for Number {
    type Output = Number;

    fn mul(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Int(l * rhs),
            Number::Float(l) => Number::Float(l * rhs as f64),
        }
    }
}

impl std::ops::Mul<f64> for Number {
    type Output = Number;

    fn mul(self, rhs: f64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Float(l as f64 * rhs),
            Number::Float(l) => Number::Float(l * rhs),
        }
    }
}

impl std::ops::Div for Number {
    type Output = Number;

    /// Match Emacs Lisp `/`: integer division when both operands are
    /// integers, float division otherwise. Callers must guard against
    /// `Int(0)` divisor before calling — Rust's `i64::div` panics for
    /// that, but `f64::div` returns `inf`/`nan` (which Emacs surfaces).
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l / r),
            (Number::Int(l), Number::Float(r)) => Number::Float(l as f64 / r),
            (Number::Float(l), Number::Int(r)) => Number::Float(l / r as f64),
            (Number::Float(l), Number::Float(r)) => Number::Float(l / r),
        }
    }
}

impl std::ops::Div<i64> for Number {
    type Output = Number;

    fn div(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Int(l / rhs),
            Number::Float(l) => Number::Float(l / rhs as f64),
        }
    }
}

impl std::ops::Div<f64> for Number {
    type Output = Number;

    fn div(self, rhs: f64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Float(l as f64 / rhs),
            Number::Float(l) => Number::Float(l / rhs),
        }
    }
}

impl std::ops::Rem for Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l % r),
            (Number::Int(l), Number::Float(r)) => Number::Float(l as f64 % r),
            (Number::Float(l), Number::Int(r)) => Number::Float(l % r as f64),
            (Number::Float(l), Number::Float(r)) => Number::Float(l % r),
        }
    }
}

impl std::ops::Rem<i64> for Number {
    type Output = Number;

    fn rem(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Int(l % rhs),
            Number::Float(l) => Number::Float(l % rhs as f64),
        }
    }
}

impl std::ops::Rem<f64> for Number {
    type Output = Number;

    fn rem(self, rhs: f64) -> Self::Output {
        match self {
            Number::Int(l) => Number::Float(l as f64 % rhs),
            Number::Float(l) => Number::Float(l % rhs),
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
