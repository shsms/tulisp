use crate::{
    Error, TulispObject, TulispValue,
    cons::{CycleCheck, ListBuilder},
};

/// Emacs's `append`: copies every list in `args` but the last, and the
/// characters of every string there, and shares the last with the
/// result. The last may be any value; a non-list one becomes the dotted
/// tail.
pub(crate) fn append(
    mut args: impl DoubleEndedIterator<Item = TulispObject>,
) -> Result<TulispObject, Error> {
    let Some(last) = args.next_back() else {
        return Ok(TulispObject::nil());
    };
    let mut builder = ListBuilder::new();
    for arg in args {
        if let TulispValue::String { value, .. } = &arg.inner_ref().0 {
            // A string adds its characters, as in Emacs: `(append "ab"
            // nil)` is (97 98).
            for c in value.chars() {
                builder.push(i64::from(u32::from(c)).into());
            }
        } else {
            builder.push_all(&arg)?;
        }
    }
    Ok(builder.build_with_tail(last))
}

/// Returns the number of elements in the given list, or the number of
/// characters if the argument is a string. Errors on a circular list
/// rather than infloop'ing.
pub fn length(list: &TulispObject) -> Result<i64, Error> {
    if list.stringp() {
        let n = list.as_string()?.chars().count();
        return n
            .try_into()
            .map_err(|e: _| Error::out_of_range(format!("{}", e)));
    }
    let mut cur = list.clone();
    let mut cycle = CycleCheck::new();
    let mut count: i64 = 0;
    loop {
        if cur.null() {
            return Ok(count);
        }
        if !cur.consp() {
            return Err(Error::type_mismatch(format!("expected list, got: {cur}")));
        }
        cur = cur.cdr()?;
        count += 1;
        cycle.step(&cur)?;
    }
}

/// The only element of `list`, which must be a list of one element,
/// or nil for an empty list when `empty_is_nil`.
pub(crate) fn sole_element(list: &TulispObject, empty_is_nil: bool) -> Result<TulispObject, Error> {
    if empty_is_nil && list.null() {
        return Ok(TulispObject::nil());
    }
    if list.consp() && list.cdr()?.null() {
        return list.car();
    }
    let expected = if empty_is_nil { "at most one" } else { "one" };
    Err(Error::type_mismatch(format!(
        "Expected a list of {expected} element, got: {list}"
    )))
}

/// Returns the last link in the given list.
pub fn last(list: &TulispObject, n: Option<i64>) -> Result<TulispObject, Error> {
    if list.null() {
        return Ok(list.clone());
    }
    if !list.consp() {
        return Err(Error::type_mismatch(format!(
            "expected list, got: {}",
            list
        )));
    }

    let len = length(list)?;
    if let Some(n) = n {
        if n < 0 {
            return Err(Error::out_of_range(format!(
                "n must be positive. got: {}",
                n
            )));
        }
        if n < len {
            return nthcdr(len - n, list.clone());
        }
    } else {
        return nthcdr(len - 1, list.clone());
    }
    Ok(list.clone())
}

/// Takes the cdr of LIST N times and returns it.
///
/// In a list whose cdrs loop back, as in Emacs, it counts the cells of the loop
/// and skips the full rounds, so the time it takes grows with the list's
/// length, not with N.
pub fn nthcdr(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
    let mut next = list;
    let mut cycle = CycleCheck::new();
    let mut step = 0;
    while step < n {
        if next.null() {
            return Ok(next);
        }
        next = next.cdr()?;
        step += 1;
        if let Some(loop_len) = cycle.looped(&next) {
            let steps_left = (n - step) % i64::from(loop_len);
            for _ in 0..steps_left {
                next = next.cdr()?;
            }
            return Ok(next);
        }
    }
    Ok(next)
}

/// Returns the n-th element in the given list.
pub fn nth(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
    nthcdr(n, list).and_then(|x| x.car())
}
