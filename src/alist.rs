//! Association-list (alist) primitives.
//!
//! An alist is a list of cons pairs, e.g. `((name . "Alice") (age . 30))`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html

use crate::{
    Error, TulispContext, TulispObject,
    eval::{DummyEval, funcall, resolve_function},
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
        let pred = resolve_function(ctx, &testfn)?;

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
    let mut cycle = crate::cons::CycleCheck::new();
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
        cycle.step(&cur)?;
    }
    Ok(TulispObject::nil())
}

/// Conversion between a Rust struct and a Lisp alist.
///
/// The alist's values are already-evaluated lisp objects, and each one
/// becomes its field's value through
/// [`TulispConvertible`](crate::TulispConvertible) with no further
/// evaluation:
///
/// ```ignore
/// let cfg = MyType::from_alist(&mut ctx, &obj)?;
/// ```
///
/// A struct declared with [`AsList!`](macro@crate::AsList) implements
/// this trait and is a [`defun`](crate::TulispContext::defun) parameter
/// in its own right; [`from_alist`] is for an alist held in a free
/// variable.
///
/// [`from_alist`]: Self::from_alist
pub trait Alistable {
    /// Deserialize an already-evaluated lisp alist (a list of dotted
    /// pairs) into `Self`. Each value is passed through as-is, with no
    /// further evaluation.
    fn from_alist(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// Serialize `self` into a Lisp alist of dotted pairs.
    fn into_alist(self, ctx: &mut TulispContext) -> TulispObject;
}

#[cfg(test)]
mod tests {
    use super::{alist_from, alist_get};
    use crate::test_utils::eval_assert_equal;
    use crate::{Alistable, Error, TulispContext};

    #[test]
    fn assoc_does_not_evaluate_a_quoted_testfn_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(assoc 1 '((1 . a)) '(lambda (a b) (= a b)))",
            "'(1 . a)",
        );
        eval_assert_equal(
            ctx,
            "(setq zz 0)
             (condition-case nil (assoc 1 '((1 . a)) '(progn (setq zz 1) 'equal)) (error nil))
             zz",
            "0",
        );
    }

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

    crate::AsList! {
        #[lisp(alist)]
        #[derive(Default, Debug)]
        struct Person {
            /// The person's first name
            name<"first-name">: String,

            /// The person's age
            age: i64,

            /// The person's addresses
            addr: Vec<String>,

            /// The person's education (optional)
            education<"edu">: Option<String> {= None},

            /// The person's current place (optional, default is "Home")
            place: Option<String> {= Some("Home".to_string())},

            /// Optional, default 42.
            answer: i64 {= 42},
        }
    }

    #[test]
    fn test_alistable_from_lisp_value_no_eval() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"(setq x '((first-name . "Bob")
                         (age . 25)
                         (addr . ("Main St" "Oak Ave"))
                         (place . "Office")))"#,
        )?;
        let x = ctx.eval_string("x")?;

        let p = Person::from_alist(&mut ctx, &x)?;
        assert_eq!(p.name, "Bob");
        assert_eq!(p.age, 25);
        assert_eq!(p.addr, vec!["Main St".to_string(), "Oak Ave".to_string()]);
        assert_eq!(p.place.as_deref(), Some("Office"));
        // `edu` is omitted — its default kicks in.
        assert_eq!(p.education, None);
        Ok(())
    }

    #[test]
    fn test_alistable_round_trip() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let p = Person {
            name: "Carol".into(),
            age: 40,
            addr: vec!["Pine St".into()],
            education: None,
            place: Some("Home".into()),
            answer: 42,
        };
        let obj = p.into_alist(&mut ctx);
        let q = Person::from_alist(&mut ctx, &obj)?;
        assert_eq!(q.name, "Carol");
        assert_eq!(q.age, 40);
        assert_eq!(q.addr, vec!["Pine St".to_string()]);
        // None serializes as nil, and an optional field reads an
        // explicit nil as `None`.
        assert_eq!(q.education, None);
        assert_eq!(q.place.as_deref(), Some("Home"));
        assert_eq!(q.answer, 42);
        Ok(())
    }

    #[test]
    fn test_alistable_explicit_nil_is_none() -> Result<(), Error> {
        // Optional fields explicitly set to nil in the alist resolve
        // to None, regardless of what default the field declared.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"(setq x '((first-name . "Bob")
                          (age . 25)
                          (addr . nil)
                          (place . nil)))"#,
        )?;
        let x = ctx.eval_string("x")?;
        let p = Person::from_alist(&mut ctx, &x)?;
        assert_eq!(p.place, None);
        // `edu` is absent → its `None` default kicks in.
        assert_eq!(p.education, None);
        Ok(())
    }

    #[test]
    fn test_alistable_missing_required_field() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(r#"(setq x '((first-name . "Bob") (addr)))"#)
            .unwrap();
        let x = ctx.eval_string("x").unwrap();
        let err = Person::from_alist(&mut ctx, &x).unwrap_err();
        let msg = err.format(&ctx);
        assert!(msg.contains("Missing age field"), "got: {msg}");
    }

    #[test]
    fn test_alistable_unexpected_key() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(r#"(setq x '((first-name . "Bob") (age . 5) (addr) (other . 1)))"#)
            .unwrap();
        let x = ctx.eval_string("x").unwrap();
        let err = Person::from_alist(&mut ctx, &x).unwrap_err();
        let msg = err.format(&ctx);
        assert!(msg.contains("Unexpected key in alist"), "got: {msg}");
    }

    #[test]
    fn test_assoc_detects_cycle() {
        // Build a circular alist via setcdr: the cdr of the last cons
        // points back to the head, so a naive walk never terminates.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
            (setq x (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
            (setcdr (cdr (cdr x)) x)
            "#,
        )
        .unwrap();
        let x = ctx.eval_string("x").unwrap();
        let key = ctx.intern("missing");
        let err = super::assoc(&mut ctx, &key, &x, None).unwrap_err();
        let msg = err.format(&ctx);
        assert!(msg.contains("Circular list"), "got: {msg}");
    }

    #[test]
    fn assoc_and_alist_get_find_pairs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r##"
        (let ((vv '((name . "person") (age . 120))))
          (list (assoc 'age vv) (alist-get 'name vv) (alist-get 'names vv) (alist-get 'names vv "something") (alist-get 'name nil)))
        "##,
            r##"'((age . 120) "person" nil "something" nil)"##,
        );

        eval_assert_equal(
            ctx,
            r##"
        (let ((vv '((20 . "person") (30 . 120))))
          (list (assoc 30 vv 'eq)
                (assoc 30 vv 'equal)

                (alist-get 20 vv nil nil 'eq)
                (alist-get 20 vv nil nil 'equal)

                (alist-get 40 vv)
                (alist-get 40 vv nil nil 'equal)

                (alist-get 40 vv "something")
                (alist-get 40 vv "something" nil 'equal)))
        "##,
            r##"'((30 . 120) (30 . 120) "person" "person" nil nil "something" "something")"##,
        );
    }
}
