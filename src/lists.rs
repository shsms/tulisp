use crate::{Error, TulispObject};

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
    // Floyd's tortoise / hare: hare advances two cells per step; if
    // it ever lands on the same cell as the tortoise (advancing by
    // one), the list is circular. Without this check `setcdr` cycles
    // (now reachable via the `setcdr` defun) would infloop here.
    let mut slow = list.clone();
    let mut fast = list.clone();
    let mut count: i64 = 0;
    loop {
        if fast.null() {
            return Ok(count);
        }
        if !fast.consp() {
            return Err(Error::type_mismatch(format!("expected list, got: {fast}")));
        }
        fast = fast.cdr()?;
        count += 1;
        if fast.null() {
            return Ok(count);
        }
        if !fast.consp() {
            return Err(Error::type_mismatch(format!("expected list, got: {fast}")));
        }
        fast = fast.cdr()?;
        count += 1;
        slow = slow.cdr()?;
        if slow.eq_ptr(&fast) {
            return Err(Error::out_of_range("Circular list".to_string()));
        }
    }
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

    // TODO: emacs' implementation uses the `safe-length` function for this, but
    // we don't have a `safe-length` function yet, because we don't have a way
    // to detect cycles in lists.
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

/// Repeatedly takes the the CDR of the list n-times, and returns the n-th cdr
/// of the given list.
pub fn nthcdr(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
    let mut next = list;
    for _ in 0..n {
        if next.null() {
            return Ok(next);
        }
        next = next.cdr()?;
    }
    Ok(next)
}

/// Returns the n-th element in the given list.
pub fn nth(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
    nthcdr(n, list).and_then(|x| x.car())
}

