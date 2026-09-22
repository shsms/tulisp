//! Property-list (plist) primitives and the typed [`Plistable`] layer.
//!
//! A plist is a flat alternating-key/value list, e.g. `(:a 1 :b 2)`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html

use std::ops::Deref;

use crate::{Error, TulispContext, TulispObject};

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
    // Floyd's tortoise / hare: hare advances by `cddr` (two cells) per
    // iteration — the natural plist step — and tortoise by `cdr`. If
    // they ever meet, the plist is circular. Mirrors `lists::length`
    // and `alist::assoc`.
    let mut slow = plist.clone();
    let mut cur = plist.clone();
    loop {
        if !cur.consp() {
            return Ok(TulispObject::nil());
        }
        if cur.car_and_then(|car| Ok(car.eq(property)))? {
            return cur.cadr();
        }
        cur = cur.cddr()?;
        slow = slow.cdr()?;
        if slow.eq_ptr(&cur) {
            return Err(Error::out_of_range("Circular plist".to_string()));
        }
    }
}

/// A typed wrapper around a Lisp plist, for use as a [`defun`](crate::TulispContext::defun) argument.
///
/// When `Plist<T>` appears as a parameter type, the function receives the
/// caller's entire argument list as a plist and deserializes it into `T`.
///
/// `T` must implement [`Plistable`], which is most easily done via the
/// [`AsList!`](macro@crate::AsList) macro.
///
/// `Plist<T>` implements [`Deref<Target = T>`], so fields of the inner struct
/// can be accessed directly.
///
/// # Example
///
/// ```rust
/// use tulisp::{AsList, Plist, TulispContext};
///
/// AsList! {
///     struct Point { x: i64, y: i64 }
/// }
///
/// let mut ctx = TulispContext::new();
///
/// ctx.defun("distance", |p: Plist<Point>| -> f64 {
///     ((p.x * p.x + p.y * p.y) as f64).sqrt()
/// });
///
/// assert_eq!(
///     ctx.eval_string("(distance :x 3 :y 4)").unwrap().as_number().unwrap(),
///     5.0
/// );
/// ```
pub struct Plist<T: Plistable> {
    plist: T,
}

impl<T> Plist<T>
where
    T: Plistable,
{
    pub(crate) fn new(ctx: &mut TulispContext, args: &[TulispObject]) -> Result<Self, Error> {
        Ok(Self {
            plist: T::from_plist_as_slice(ctx, args)?,
        })
    }

    pub fn into_inner(self) -> T {
        self.plist
    }
}

impl<T> Deref for Plist<T>
where
    T: Plistable,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.plist
    }
}

/// Conversion between a Rust struct and a Lisp plist.
///
/// The plist's values are already-evaluated lisp objects, and each one
/// becomes its field's value through
/// [`TulispConvertible`](crate::TulispConvertible) with no further
/// evaluation — which is what you want for plists held in free
/// variables, literals, or any value already produced by the
/// interpreter:
///
/// ```ignore
/// let cfg = MyType::from_plist(&mut ctx, &obj)?;
/// ```
///
/// The [`AsList!`](macro@crate::AsList) macro implements this trait
/// for the struct it declares.
pub trait Plistable {
    /// Deserialize `Self` from a flat `[k0, v0, k1, v1, …]` slice of
    /// already-evaluated lisp values. The defun-arg path (`Plist<T>`)
    /// calls this directly with the evaluated arg slice.
    fn from_plist_as_slice(ctx: &mut TulispContext, kvs: &[TulispObject]) -> Result<Self, Error>
    where
        Self: Sized;

    /// Deserialize an already-evaluated lisp plist value into `Self`.
    fn from_plist(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// Serialize `self` into a Lisp plist.
    fn into_plist(self, ctx: &mut TulispContext) -> TulispObject;
}

#[cfg(test)]
mod tests {
    use super::{plist_from, plist_get};
    use crate::{
        Error, Plist, Plistable, TulispContext,
        test_utils::{eval_assert_equal, eval_assert_error},
    };

    #[test]
    fn test_plist_primitives() -> Result<(), Error> {
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

    #[test]
    fn plist_get_matches_a_nil_key() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(plist-get '(nil 1 a 2) nil)", "1");
        eval_assert_equal(&mut ctx, "(plist-get '(a 2) nil)", "nil");
    }

    #[test]
    fn test_plist_get_detects_cycle() {
        // Build a circular plist: cdr of the tail cell points back to
        // the head, so iteration via `cddr` never reaches a non-cons.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
            (setq x (list :a 1 :b 2))
            (setcdr (cdr (cdr (cdr x))) x)
            "#,
        )
        .unwrap();
        let x = ctx.eval_string("x").unwrap();
        let missing = ctx.intern(":missing");
        let err = plist_get(&x, &missing).unwrap_err();
        let msg = err.format(&ctx);
        assert!(msg.contains("Circular plist"), "got: {msg}");
    }

    crate::AsList! {
        #[derive(Default)]
        struct Person {
            /// The person's first name
            name<":first-name">: String,

            /// The person's age
            age: i64,

            /// The person's addresses
            addr: Vec<String>,

            /// The person's education (optional)
            education<":edu">: Option<String> {= None},

            /// The person's current place (optional, default is "Home")
            place: Option<String> {= Some("Home".to_string())},

            /// The person's answer to the ultimate question of life, the
            /// universe, and everything (optional, default is 42)
            answer: i64 {= 42},
        }
    }

    #[test]
    fn test_plistable() -> Result<(), Error> {
        let mut ctx = TulispContext::new();

        ctx.defun("get-name", |person: Plist<Person>| person.name.clone())
            .defun("get-age", |p: Plist<Person>| -> i64 { p.age })
            .defun("get-ans", |p: Plist<Person>| -> i64 { p.answer })
            .defun("get-place", |p: Plist<Person>| p.place.clone().unwrap())
            .defun("get-edu", |p: Plist<Person>| {
                p.education.clone().unwrap_or("Unknown".to_string())
            })
            .defun("get-addr", |p: Plist<Person>| {
                p.addr.last().unwrap().clone()
            });

        eval_assert_equal(
            &mut ctx,
            r#"(get-name :first-name "Alice" :age 30 :addr nil)"#,
            r#""Alice""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-age :first-name "Alice" :age 30 :addr nil)"#,
            r#"30"#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil)"#,
            r#""Unknown""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil :edu "School")"#,
            r#""School""#,
        );

        // An optional field reads an explicit nil as `None`.
        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil :edu nil)"#,
            r#""Unknown""#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-place :first-name "Alice" :age 30 :addr nil)"#,
            r#""Home""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-place :first-name "Alice" :age 30 :addr nil :place "Office")"#,
            r#""Office""#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-ans :first-name "Alice" :age 30 :addr nil)"#,
            r#"42"#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-ans :first-name "Alice" :age 30 :addr nil :place "Office" :answer 5)"#,
            r#"5"#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-addr :first-name "Alice" :age 30 :addr '("street" "other street"))"#,
            r#""other street""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-addr :first-name "Alice" :age 30 :addr '("street" "other street"))"#,
            r#""other street""#,
        );

        eval_assert_error(
            &mut ctx,
            r#"(get-age :first-name "Alice" :age 30 :other 10)"#,
            r#"ERR PlistError: Unexpected key in plist: :other
<eval_string>:1.1-1.47:  at (get-age :first-name "Alice" :age 30 :other 10)
"#,
        );

        eval_assert_error(
            &mut ctx,
            r#"(get-age :first-name "Alice")"#,
            r#"ERR PlistError: Missing :age field
<eval_string>:1.1-1.29:  at (get-age :first-name "Alice")
"#,
        );

        Ok(())
    }

    #[test]
    fn test_plistable_round_trip() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let p = Person {
            name: "Carol".into(),
            age: 40,
            addr: vec!["Pine St".into()],
            education: None,
            place: Some("Home".into()),
            answer: 42,
        };
        let obj = p.into_plist(&mut ctx);
        let q = Person::from_plist(&mut ctx, &obj)?;
        assert_eq!(q.name, "Carol");
        assert_eq!(q.age, 40);
        assert_eq!(q.addr, vec!["Pine St".to_string()]);
        // None serialises as nil, and an optional field reads an
        // explicit nil as `None`.
        assert_eq!(q.education, None);
        assert_eq!(q.place.as_deref(), Some("Home"));
        assert_eq!(q.answer, 42);
        Ok(())
    }

    #[test]
    fn test_plistable_from_lisp_value_no_eval() -> Result<(), Error> {
        // A plist held in a free variable contains already-evaluated
        // values, and `from_plist` converts each one as it is, so a
        // list field like `addr` reads cleanly.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"(setq x '(:first-name "Bob"
                          :age 25
                          :addr ("Main St" "Oak Ave")
                          :place "Office"))"#,
        )?;
        let x = ctx.eval_string("x")?;

        let p = Person::from_plist(&mut ctx, &x)?;
        assert_eq!(p.name, "Bob");
        assert_eq!(p.age, 25);
        assert_eq!(p.addr, vec!["Main St".to_string(), "Oak Ave".to_string()]);
        assert_eq!(p.place.as_deref(), Some("Office"));
        // `:edu` is omitted — its default kicks in.
        assert_eq!(p.education, None);

        Ok(())
    }
}
