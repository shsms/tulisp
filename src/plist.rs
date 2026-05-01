//! Property-list (plist) primitives.
//!
//! A plist is a flat alternating-key/value list, e.g. `(:a 1 :b 2)`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html

use crate::{Error, TulispObject};

/// Makes a plist from the given arguments.
pub fn plist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input.into_iter() {
        builder.push(key);
        builder.push(value);
    }
    builder.build()
}

/// Returns the value of the property `property` stored in the property list
/// `plist`.
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
    use super::{plist_from, plist_get};
    use crate::{Error, TulispContext};

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
