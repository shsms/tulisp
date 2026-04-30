use crate::{
    Error, TulispContext, TulispObject,
    eval::{DummyEval, funcall},
    list,
};

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

/// Makes an alist from the given arguments.
pub fn alist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input.into_iter() {
        builder.push(TulispObject::cons(key, value));
    }
    builder.build()
}

/// Makes a plist from the given arguments.
pub fn plist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input.into_iter() {
        builder.push(key);
        builder.push(value);
    }
    builder.build()
}

/// Returns the first association for key in alist, comparing key against the
/// alist elements using testfn if it is a function, and equal otherwise.
///
/// Read more about `alist`s
/// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html).
pub fn assoc(
    ctx: &mut TulispContext,
    key: &TulispObject,
    alist: &TulispObject,
    testfn: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    if !alist.listp() {
        return Err(Error::type_mismatch(format!(
            "expected alist. got: {}",
            alist
        )));
    }
    if let Some(testfn) = testfn {
        let pred = ctx.eval(&testfn)?;

        let testfn = |_1: &TulispObject, _2: &TulispObject| -> Result<bool, Error> {
            funcall::<DummyEval>(ctx, &pred, &list!(,_1.clone() ,_2.clone()).unwrap())
                .map(|x| x.is_truthy())
        };
        assoc_find(key, alist, testfn)
    } else {
        let testfn = |_1: &TulispObject, _2: &TulispObject| Ok(_1.equal(_2));
        assoc_find(key, alist, testfn)
    }
}

/// Finds the first association (key . value) by comparing key with alist
/// elements, and, if found, returns the value of that association.
///
/// Read more about `alist`s
/// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html).
pub fn alist_get(
    ctx: &mut TulispContext,
    key: &TulispObject,
    alist: &TulispObject,
    default_value: Option<TulispObject>,
    remove: Option<TulispObject>,
    testfn: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    // The REMOVE arg makes `(setf (alist-get …) nil)` delete an
    // entry; since `setf` isn't implemented yet, accepting a non-nil
    // REMOVE silently would miss the user's intent. Error explicitly
    // rather than ignoring.
    if let Some(remove) = remove
        && remove.is_truthy()
    {
        return Err(Error::not_implemented(
            "alist-get: REMOVE argument is not implemented (no `setf` support yet)".to_string(),
        ));
    }
    let x = assoc(ctx, key, alist, testfn)?;
    if x.is_truthy() {
        x.cdr()
    } else {
        Ok(default_value.unwrap_or_else(TulispObject::nil))
    }
}

fn assoc_find(
    key: &TulispObject,
    alist: &TulispObject,
    mut testfn: impl FnMut(&TulispObject, &TulispObject) -> Result<bool, Error>,
) -> Result<TulispObject, Error> {
    let mut cur = alist.clone();
    while cur.consp() {
        let entry = cur.car()?;
        // Match Emacs: silently skip non-cons elements rather than
        // erroring on `caar`. So `(assoc 'b '(1 (b . 2)))` finds
        // the pair instead of crashing on the leading `1`.
        if entry.consp() {
            let entry_key = entry.car()?;
            if testfn(&entry_key, key)? {
                return Ok(entry);
            }
        }
        cur = cur.cdr()?;
    }
    Ok(TulispObject::nil())
}

/// Returns the value of the property `property` stored in the property list
/// `plist`.
///
/// Read more about `plist`s
/// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html).
pub fn plist_get(plist: &TulispObject, property: &TulispObject) -> Result<TulispObject, Error> {
    let mut cur = plist.clone();
    while cur.consp() {
        if cur.car_and_then(|car| Ok(car.eq(property)))? {
            return cur.cadr();
        }
        cur = cur.cddr()?;
    }
    Ok(TulispObject::nil())
}

#[cfg(test)]
mod tests {
    use crate::lists::{Error, TulispContext, alist_from, alist_get, plist_from, plist_get};

    #[test]
    fn test_alist() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let a = ctx.intern("a");
        let b = ctx.intern("b");
        let c = ctx.intern("c");
        let d = ctx.intern("d");
        let list = alist_from([
            (a.clone(), 20.into()),
            (b.clone(), 30.into()),
            (c.clone(), 40.into()),
        ]);
        assert!(alist_get(&mut ctx, &b, &list, None, None, None)?.equal(&30.into()));
        assert!(alist_get(&mut ctx, &d, &list, None, None, None)?.null());
        Ok(())
    }

    #[test]
    fn test_plist() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let a = ctx.intern("a");
        let b = ctx.intern("b");
        let c = ctx.intern("c");
        let d = ctx.intern("d");
        let list = plist_from([
            (a.clone(), 20.into()),
            (b.clone(), 30.into()),
            (c.clone(), 40.into()),
        ]);
        assert!(plist_get(&list, &b)?.equal(&30.into()));
        assert!(plist_get(&list, &d)?.null());
        Ok(())
    }
}
