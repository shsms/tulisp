use crate::{eval::eval, list, Error, ErrorKind, TulispContext, TulispValue};

pub fn assoc(
    ctx: &mut TulispContext,
    key: &TulispValue,
    alist: &TulispValue,
    _testfn: Option<TulispValue>,
) -> Result<TulispValue, Error> {
    if !alist.consp() {
        return Err(
            Error::new(ErrorKind::TypeMismatch, "expected alist".to_owned())
                .with_span(alist.span()),
        );
    }
    if let Some(testfn) = _testfn {
        let pred = eval(ctx, &testfn)?;

        let mut testfn = |_1: &TulispValue, _2: &TulispValue| -> Result<bool, Error> {
            let vv = list!(,TulispValue::nil() ,_1.clone() ,_2.clone()).unwrap();
            vv.with_ctxobj(Some(pred.clone()));
            eval(ctx, &vv).map(|vv| vv.as_bool())
        };
        assoc_find(key, alist, &mut testfn)
    } else {
        let mut testfn = |_1: &TulispValue, _2: &TulispValue| Ok(_1.eq(_2));
        assoc_find(key, alist, &mut testfn)
    }
}

pub fn alist_get(
    ctx: &mut TulispContext,
    key: &TulispValue,
    alist: &TulispValue,
    default_value: Option<TulispValue>,
    _remove: Option<TulispValue>, // TODO: implement after `setf`
    testfn: Option<TulispValue>,
) -> Result<TulispValue, Error> {
    let x = crate::lists::assoc(ctx, key, alist, testfn)?;
    if x.as_bool() {
        x.cdr()
    } else {
        Ok(default_value.unwrap_or_else(TulispValue::nil))
    }
}

fn assoc_find(
    key: &TulispValue,
    alist: &TulispValue,
    testfn: &mut dyn FnMut(&TulispValue, &TulispValue) -> Result<bool, Error>,
) -> Result<TulispValue, Error> {
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
    Ok(TulispValue::nil())
}

pub fn plist_get(plist: TulispValue, property: &TulispValue) -> Result<TulispValue, Error> {
    let mut next = plist;
    while let Some(cons) = next.as_list_cons() {
        let car = cons.car();
        let cdr = cons.cdr();
        if car.eq(property) {
            return cdr.car();
        }
        next = cdr.clone();
    }
    Ok(TulispValue::nil())
}
