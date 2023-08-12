use crate::{
    eval::{eval, funcall, DummyEval},
    list, Error, ErrorKind, TulispContext, TulispObject,
};

/// Returns the number of elements in the given list.
pub fn length(list: &TulispObject) -> Result<i64, Error> {
    list.base_iter()
        .count()
        .try_into()
        .map_err(|e: _| Error::new(ErrorKind::OutOfRange, format!("{}", e)).with_span(list.span()))
}

/// Returns the last link in the given list.
pub fn last(list: &TulispObject, n: Option<i64>) -> Result<TulispObject, Error> {
    if list.null() {
        return Ok(list.clone());
    }
    if !list.consp() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("expected list, got: {}", list),
        )
        .with_span(list.span()));
    }

    // TODO: emacs' implementation uses the `safe-length` function for this, but
    // we don't have a `safe-length` function yet, because we don't have a way
    // to detect cycles in lists.
    let len = length(list)?;
    if let Some(n) = n {
        if n < 0 {
            return Err(Error::new(
                ErrorKind::OutOfRange,
                format!("n must be positive. got: {}", n),
            )
            .with_span(list.span()));
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
    let alist = TulispObject::nil();
    for (key, value) in input.into_iter() {
        let _ = alist.push(TulispObject::cons(key, value));
    }
    alist
}

/// Makes a plist from the given arguments.
pub fn plist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let plist = TulispObject::nil();
    for (key, value) in input.into_iter() {
        let _ = plist.push(key);
        let _ = plist.push(value);
    }
    plist
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
    if !alist.consp() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("expected alist. got: {}", alist),
        )
        .with_span(alist.span()));
    }
    if let Some(testfn) = testfn {
        let pred = eval(ctx, &testfn)?;

        let mut testfn = |_1: &TulispObject, _2: &TulispObject| -> Result<bool, Error> {
            funcall::<DummyEval>(ctx, &pred, &list!(,_1.clone() ,_2.clone()).unwrap())
                .map(|x| x.as_bool())
        };
        assoc_find(key, alist, &mut testfn)
    } else {
        let mut testfn = |_1: &TulispObject, _2: &TulispObject| Ok(_1.equal(_2));
        assoc_find(key, alist, &mut testfn)
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
    _remove: Option<TulispObject>, // TODO: implement after `setf`
    testfn: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    let x = assoc(ctx, key, alist, testfn)?;
    if x.as_bool() {
        x.cdr()
    } else {
        Ok(default_value.unwrap_or_else(TulispObject::nil))
    }
}

fn assoc_find(
    key: &TulispObject,
    alist: &TulispObject,
    testfn: &mut dyn FnMut(&TulispObject, &TulispObject) -> Result<bool, Error>,
) -> Result<TulispObject, Error> {
    for kvpair in alist.base_iter() {
        if !kvpair.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected cons inside alist".to_owned(),
            )
            .with_span(kvpair.span()));
        }
        if testfn(&kvpair.car()?, key)? {
            return Ok(kvpair);
        }
    }
    Ok(TulispObject::nil())
}

/// Returns the value of the property `property` stored in the property list
/// `plist`.
///
/// Read more about `plist`s
/// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html).
pub fn plist_get(plist: TulispObject, property: &TulispObject) -> Result<TulispObject, Error> {
    let mut next = plist;
    while let Some(cons) = next.as_list_cons() {
        let car = cons.car();
        let cdr = cons.cdr();
        if car.eq(property) {
            return cdr.car();
        }
        next = cdr.clone();
    }
    Ok(TulispObject::nil())
}

#[cfg(test)]
mod tests {
    use crate::{alist_from, alist_get, plist_from, plist_get, Error, TulispContext};

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
        assert_eq!(alist_get(&mut ctx, &b, &list, None, None, None)?, 30.into());
        assert_eq!(
            alist_get(&mut ctx, &d, &list, None, None, None)?.null(),
            true
        );
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
        assert_eq!(plist_get(list.clone(), &b)?, 30.into());
        assert_eq!(plist_get(list, &d)?.null(), true);
        Ok(())
    }
}
