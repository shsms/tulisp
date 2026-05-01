//! Association-list (alist) primitives.
//!
//! An alist is a list of cons pairs, e.g. `((name . "Alice") (age . 30))`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html

use crate::{
    Error, TulispContext, TulispObject,
    eval::{DummyEval, funcall},
    list,
};

/// Makes an alist from the given arguments.
pub fn alist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input.into_iter() {
        builder.push(TulispObject::cons(key, value));
    }
    builder.build()
}

/// Returns the first association for key in alist, comparing key against the
/// alist elements using testfn if it is a function, and equal otherwise.
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

#[cfg(test)]
mod tests {
    use super::{alist_from, alist_get};
    use crate::{Error, TulispContext};

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
}
