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
            return Err(Error::wrong_type_argument(
                "listp",
                cur.clone(),
                format!("expected list, got: {cur}"),
            ));
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

/// The last N links of `list`, one when `n` is `None`, as Emacs Lisp's `last`
/// gives them. A dotted tail is no link, an `n` of 0 gives the tail after the
/// last link, and a negative `n` gives nil.
///
/// ```rust
/// use tulisp::{TulispContext, lists};
///
/// let mut ctx = TulispContext::new();
/// let list = ctx.eval_string("'(a b . c)").unwrap();
/// assert_eq!(lists::last(&list, None).unwrap().to_string(), "(b . c)");
/// assert_eq!(lists::last(&list, Some(0)).unwrap().to_string(), "c");
/// ```
pub fn last(list: &TulispObject, n: Option<i64>) -> Result<TulispObject, Error> {
    let n = n.unwrap_or(1);
    if n < 0 {
        return Ok(TulispObject::nil());
    }
    let mut links = 0;
    let mut cell = list.clone();
    let mut cycle = CycleCheck::new();
    while cell.consp() {
        links += 1;
        cell = cell.cdr()?;
        cycle.step(&cell)?;
    }
    if n >= links {
        return Ok(list.clone());
    }
    nthcdr(links - n, list)
}

/// Takes the cdr of LIST N times and returns it.
///
/// In a list whose cdrs loop back, as in Emacs, it counts the cells of the loop
/// and skips the full rounds, so the time it takes grows with the list's
/// length, not with N.
///
/// ```rust
/// use tulisp::{TulispContext, lists};
///
/// let mut ctx = TulispContext::new();
/// let list = ctx.eval_string("'(a b c)").unwrap();
/// assert_eq!(lists::nthcdr(1, &list).unwrap().to_string(), "(b c)");
/// ```
pub fn nthcdr(n: i64, list: &TulispObject) -> Result<TulispObject, Error> {
    let mut next = list.clone();
    let mut cycle = CycleCheck::new();
    let mut step = 0;
    while step < n {
        if next.null() {
            return Ok(next);
        }
        next = match next.cdr() {
            Ok(cdr) => cdr,
            Err(_) => {
                return Err(Error::wrong_type_argument(
                    "listp",
                    list.clone(),
                    format!("Expected list, got: {next}"),
                )
                .with_trace(next));
            }
        };
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

/// Returns the n-th element in the given list, counting from 0, as Emacs Lisp's
/// `nth` does.
///
/// ```rust
/// use tulisp::{TulispContext, lists};
///
/// let mut ctx = TulispContext::new();
/// let list = ctx.eval_string("'(a b c)").unwrap();
/// assert_eq!(lists::nth(1, &list).unwrap().to_string(), "b");
/// ```
pub fn nth(n: i64, list: &TulispObject) -> Result<TulispObject, Error> {
    nthcdr(n, list).and_then(|x| x.car())
}
