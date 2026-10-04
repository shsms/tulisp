//! Association-list (alist) primitives.
//!
//! An alist is a list of cons pairs, e.g. `((name . "Alice") (age . 30))`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html

use crate::{Error, TulispContext, TulispObject, eval::resolve_function};

/// Makes an alist from the given key and value pairs.
///
/// ```rust
/// use tulisp::{TulispObject, alist_from};
///
/// let pairs = vec![(TulispObject::from("a"), TulispObject::from(1))];
/// assert_eq!(alist_from(pairs).to_string(), r#"(("a" . 1))"#);
/// ```
pub fn alist_from(input: impl IntoIterator<Item = (TulispObject, TulispObject)>) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input {
        builder.push(TulispObject::cons(key, value));
    }
    builder.build()
}

/// Returns the first association for key in alist, comparing key against the
/// alist elements using testfn, and with `equal` when testfn is `None` or nil.
pub fn assoc(
    ctx: &mut TulispContext,
    key: &TulispObject,
    alist: &TulispObject,
    testfn: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    assoc_by(ctx, key, alist, testfn, |a, b| a.try_equal(b))
}

/// Finds the first association (key . value) by comparing key with alist
/// elements, and, if found, returns the value of that association. It compares
/// with testfn, and with `eq` when testfn is `None` or nil, as Emacs Lisp's
/// `alist-get` does.
pub fn alist_get(
    ctx: &mut TulispContext,
    key: &TulispObject,
    alist: &TulispObject,
    default_value: Option<TulispObject>,
    testfn: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    let x = assoc_by(ctx, key, alist, testfn, |a, b| Ok(a.eq(b)))?;
    if x.is_truthy() {
        x.cdr()
    } else {
        Ok(default_value.unwrap_or_else(TulispObject::nil))
    }
}

/// The first association for key in alist, compared with testfn, or with
/// `default` when testfn is `None` or nil.
fn assoc_by(
    ctx: &mut TulispContext,
    key: &TulispObject,
    alist: &TulispObject,
    testfn: Option<TulispObject>,
    default: impl FnMut(&TulispObject, &TulispObject) -> Result<bool, Error>,
) -> Result<TulispObject, Error> {
    if !alist.listp() {
        return Err(Error::type_mismatch(format!(
            "expected alist. got: {alist}"
        )));
    }
    match testfn.filter(|testfn| !testfn.null()) {
        Some(testfn) => {
            let pred = resolve_function(ctx, &testfn)?;
            assoc_find(key, alist, |a, b| {
                crate::bytecode::call_function(ctx, &pred, vec![a.clone(), b.clone()])
                    .map(|x| x.is_truthy())
            })
        }
        None => assoc_find(key, alist, default),
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

    // A test function given as a lambda or as a symbol.
    #[test]
    fn a_test_function_as_a_lambda_or_a_symbol() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(assoc 2 '((1 . a) (2 . b)) (lambda (a b) (= a b)))",
            "'(2 . b)",
        );
        eval_assert_equal(ctx, "(assoc 2 '((1 . a) (2 . b)) 'eql)", "'(2 . b)");
        eval_assert_equal(
            ctx,
            "(alist-get 2 '((1 . a) (2 . b)) nil nil (lambda (a b) (= a b)))",
            "'b",
        );
    }

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
        assert!(alist_get(&mut ctx, &b, &list, None, None)?.equal(&30.into()));
        assert!(alist_get(&mut ctx, &d, &list, None, None)?.null());
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
        let msg = err.to_string();
        assert!(msg.contains("Missing age field"), "got: {msg}");
    }

    #[test]
    fn test_alistable_unexpected_key() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(r#"(setq x '((first-name . "Bob") (age . 5) (addr) (other . 1)))"#)
            .unwrap();
        let x = ctx.eval_string("x").unwrap();
        let err = Person::from_alist(&mut ctx, &x).unwrap_err();
        let msg = err.to_string();
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
        let msg = err.to_string();
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

    // `assoc` and `alist-get` refuse a value that is no list before they look
    // at TESTFN.
    #[test]
    fn assoc_and_alist_get_reject_a_non_list_alist() {
        let ctx = &mut TulispContext::new();
        for form in [
            "(alist-get 'a 5)",
            "(alist-get 'a 5 nil nil 'equal)",
            "(alist-get 'a 5 nil nil 'no-such-fn)",
            "(assoc 'a 5)",
            "(assoc 'a 5 'no-such-fn)",
        ] {
            crate::test_utils::eval_assert_error_line(
                ctx,
                form,
                "ERR TypeMismatch: expected alist. got: 5",
            );
        }
    }

    // With no TESTFN, `alist-get` compares keys with `eq`, as in Emacs: a
    // string key matches only with `equal` as TESTFN.
    #[test]
    fn alist_get_compares_keys_with_eq_by_default() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(let ((vv (list (cons "a" 1) (cons 1000 2) (cons 'k 3))))
                 (list (alist-get "a" vv) (alist-get "a" vv nil nil 'equal)
                       (alist-get 1000 vv) (alist-get 'k vv)))"#,
            "'(nil 1 2 3)",
        );
    }

    // From Rust, a nil TESTFN means no TESTFN, as in Emacs.
    #[test]
    fn a_nil_testfn_is_no_testfn() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let alist = ctx.eval_string(r#"(list (cons "a" 1))"#)?;
        let key = crate::TulispObject::from("a");
        let nil = Some(crate::TulispObject::nil());
        assert!(alist_get(ctx, &key, &alist, None, nil.clone())?.null());
        assert_eq!(
            super::assoc(ctx, &key, &alist, nil)?.to_string(),
            r#"("a" . 1)"#
        );
        Ok(())
    }
}
