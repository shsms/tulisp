use crate::{eval::eval, list, Error, ErrorKind, TulispContext, TulispObject};

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
        return Err(
            Error::new(ErrorKind::TypeMismatch, "expected alist".to_owned())
                .with_span(alist.span()),
        );
    }
    if let Some(testfn) = testfn {
        let pred = eval(ctx, &testfn)?;

        let mut testfn = |_1: &TulispObject, _2: &TulispObject| -> Result<bool, Error> {
            let vv = list!(,TulispObject::nil() ,_1.clone() ,_2.clone()).unwrap();
            vv.with_ctxobj(Some(pred.clone()));
            eval(ctx, &vv).map(|vv| vv.as_bool())
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
    let x = crate::lists::assoc(ctx, key, alist, testfn)?;
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
