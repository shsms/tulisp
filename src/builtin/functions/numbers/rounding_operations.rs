use crate::{Error, TulispContext, number::f64_to_i64_checked};

/// Combine N and optional DIVISOR into a single f64, matching Elisp's
/// `(floor N &optional DIVISOR)` family which all divide N by DIVISOR before
/// applying the rounding operation.
fn combined(n: f64, divisor: Option<f64>) -> Result<f64, Error> {
    match divisor {
        None => Ok(n),
        Some(0.0) => Err(Error::out_of_range(
            "arithmetic error: divide by zero".to_string(),
        )),
        Some(d) => Ok(n / d),
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("floor", |n: f64, divisor: Option<f64>| {
        f64_to_i64_checked(combined(n, divisor)?.floor(), "floor")
    });

    ctx.defun("ceiling", |n: f64, divisor: Option<f64>| {
        f64_to_i64_checked(combined(n, divisor)?.ceil(), "ceiling")
    });

    ctx.defun("truncate", |n: f64, divisor: Option<f64>| {
        f64_to_i64_checked(combined(n, divisor)?.trunc(), "truncate")
    });

    ctx.defun("round", |n: f64, divisor: Option<f64>| {
        // Banker's rounding (round half to even) to match Elisp.
        let v = combined(n, divisor)?;
        let rounded = if (v - v.trunc()).abs() == 0.5 {
            let truncated = f64_to_i64_checked(v.trunc(), "round")?;
            if truncated % 2 == 0 {
                truncated
            } else if v > 0.0 {
                truncated + 1
            } else {
                truncated - 1
            }
        } else {
            f64_to_i64_checked(v.round(), "round")?
        };
        Ok(rounded)
    });

    ctx.defun("ffloor", |x: f64| x.floor());
    ctx.defun("fceiling", |x: f64| x.ceil());
    ctx.defun("fround", |x: f64| x.round());
    ctx.defun("ftruncate", |x: f64| x.trunc());
}
