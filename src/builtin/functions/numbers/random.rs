//! Emacs's `random`.

use crate::{Error, TulispContext, TulispObject, TulispValue, context::random::Random};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("random", random);
}

/// `(random &optional LIMIT)`: a number from 0 to LIMIT - 1 for a positive
/// integer LIMIT, and an `args-out-of-range` error for any other integer. Any
/// other LIMIT gives any integer. Before that, t seeds the numbers from the
/// system, and a string seeds them from its text, so the numbers after the same
/// string are the same.
fn random(ctx: &mut TulispContext, limit: Option<TulispObject>) -> Result<i64, Error> {
    let limit = limit.unwrap_or_default();
    if limit.integerp() {
        let limit = limit.as_int()?;
        let Some(limit) = u64::try_from(limit).ok().filter(|limit| *limit > 0) else {
            return Err(Error::out_of_range(limit.to_string()));
        };
        // LIMIT is at most `i64::MAX`, so the number fits.
        return Ok(ctx.random.below(limit) as i64);
    }
    if limit.stringp() {
        ctx.random = limit.with_str(Random::from_text)?;
    } else if matches!(limit.inner_ref().0, TulispValue::T) {
        ctx.random = Random::from_system();
    }
    Ok(ctx.random.next() as i64)
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert, eval_assert_equal};

    #[test]
    fn random_stays_below_its_limit() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(random 1)", "0");
        eval_assert(
            ctx,
            "(let ((ok t)) (dotimes (_ 200) (let ((n (random 3))) (unless (and (>= n 0) (< n 3)) (setq ok nil)))) ok)",
        );
        for program in [
            "(random)",
            "(random nil)",
            "(random t)",
            "(random 1.5)",
            "(random 'a)",
        ] {
            eval_assert(ctx, &format!("(integerp {program})"));
        }
        for limit in ["0", "-5"] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e (random {limit}) (args-out-of-range 'refused))"),
                "'refused",
            );
        }
    }

    // A string seed repeats the numbers after it, here and in another context.
    #[test]
    fn a_string_seed_repeats_the_numbers() {
        let program = r#"(progn (random "seed") (list (random 1000) (random) (random 10)))"#;
        let ctx = &mut TulispContext::new();
        let first = ctx.eval_string(program).unwrap();
        let again = ctx.eval_string(program).unwrap();
        let elsewhere = TulispContext::new().eval_string(program).unwrap();
        assert!(first.equal(&again), "{first} {again}");
        assert!(first.equal(&elsewhere), "{first} {elsewhere}");
        let other = ctx
            .eval_string(r#"(progn (random "other") (list (random 1000) (random) (random 10)))"#)
            .unwrap();
        assert!(!first.equal(&other), "{first} {other}");
    }

    #[test]
    fn set_random_seed_repeats_the_numbers() {
        let ctx = &mut TulispContext::new();
        ctx.set_random_seed(5);
        let first = ctx.eval_string("(list (random) (random 7))").unwrap();
        ctx.set_random_seed(5);
        let again = ctx.eval_string("(list (random) (random 7))").unwrap();
        assert!(first.equal(&again), "{first} {again}");
    }
}
