//! Association-list (alist) primitives.
//!
//! An alist is a list of cons pairs, e.g. `((name . "Alice") (age . 30))`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html

use crate::{
    DummyEval, Error, Evaluator, TulispContext, TulispObject, eval::funcall, list,
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
            funcall::<crate::eval::DummyEval>(ctx, &pred, &list!(,_1.clone() ,_2.clone()).unwrap())
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

/// Conversion between a Rust struct and a Lisp alist.
///
/// The default entry point [`from_alist`](Self::from_alist) treats
/// the alist's values as already-evaluated lisp objects and passes
/// each through unchanged — which is what you want for alists held
/// in free variables, literals, or any value already produced by the
/// interpreter:
///
/// ```ignore
/// let cfg = MyType::from_alist(&mut ctx, &obj)?;
/// ```
///
/// To re-evaluate each value during conversion (e.g. when the alist
/// holds unevaluated forms), pick a different [`Evaluator`] strategy
/// via [`from_alist_with`](Self::from_alist_with):
///
/// ```ignore
/// let cfg = MyType::from_alist_with::<Eval>(&mut ctx, &obj)?;
/// ```
///
/// Unlike [`Plistable`](crate::Plistable), `Alistable` does not get an
/// auto-derived [`TulispConvertible`](crate::TulispConvertible) impl —
/// `into_alist` needs a context to intern symbol keys, and
/// `TulispConvertible::into_tulisp` does not take one. Construct alists
/// for return through [`into_alist`](Self::into_alist) directly, or
/// through [`alist_from`].
///
/// The [`AsAlist!`](macro@crate::AsAlist) macro generates the
/// `from_alist_with` and `into_alist` implementations from a struct
/// definition; the no-eval shortcut is provided as a default method on
/// the trait.
pub trait Alistable {
    /// Deserialize `obj` (a Lisp alist of dotted pairs) into `Self`
    /// using `E` to resolve each value. Implementation hook the
    /// [`AsAlist!`] macro fills in; most callers want
    /// [`from_alist`](Self::from_alist) instead.
    fn from_alist_with<E: Evaluator>(
        ctx: &mut TulispContext,
        obj: &TulispObject,
    ) -> Result<Self, Error>
    where
        Self: Sized;

    /// Deserialize an already-evaluated lisp alist into `Self`. Each
    /// value is passed through as-is, with no further evaluation.
    fn from_alist(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Self::from_alist_with::<DummyEval>(ctx, obj)
    }

    /// Serialize `self` into a Lisp alist of dotted pairs.
    fn into_alist(self, ctx: &mut TulispContext) -> TulispObject;
}

/// Derive [`Alistable`] for a struct.
///
/// # Syntax
///
/// ```text
/// AsAlist! {
///     [attributes]
///     [pub] struct Name {
///         [field_vis] field[<"key-name">]: Type [{= default}],
///         ...
///     }
/// }
/// ```
///
/// - Each field maps to a symbol key in the alist. By default the key
///   is `stringify!(field)`. Use `field<"custom-name">` to override.
/// - A field with `{= expr}` is optional; if absent from the alist the
///   default expression is used.
/// - A field without a default is required; a missing key is an error.
///
/// # Example
///
/// ```rust
/// use tulisp::{TulispContext, Alistable, AsAlist};
///
/// AsAlist! {
///     struct Person {
///         name: String,
///         age: i64 {= 0},
///     }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.eval_string(r#"(setq alice '((name . "Alice") (age . 30)))"#).unwrap();
/// let alice_obj = ctx.eval_string("alice").unwrap();
/// let alice = Person::from_alist(&mut ctx, &alice_obj).unwrap();
/// assert_eq!(alice.name, "Alice");
/// assert_eq!(alice.age, 30);
/// ```
#[macro_export]
macro_rules! AsAlist {
    (@key-name
        $field:ident<$field_key:literal>) => {
        $field_key
    };
    (@key-name $field:ident) => {
        stringify!($field)
    };

    (@missing-field $key_name:expr, $default:expr) => {
        Ok($default)
    };
    (@missing-field $key_name:expr,) => {
        Err($crate::Error::alist_error(concat!("Missing ", $key_name, " field")))
    };

    (@extract-field $ctx:ident, $value:ident, None) => {
        Some(<E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, Some($e: expr)) => {
        Some(<E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, $e: expr) => {
        <E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?
    };

    (@extract-field $ctx:ident, $value:ident) => {
        <E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?
    };

    (
        $( #[$meta:meta] )*
        $vis:vis struct $struct_name:ident {
            $(
                $( #[$($field_meta:tt)+] )*
                $field_vis:vis $field:ident$(<$field_key:literal>)? : $type:ty
                $({= $($default:tt)+ })?
            ),+ $(,)?
        }
    ) => {

        $( #[$meta] )*
        $vis struct $struct_name {

            $($( #[$($field_meta)+] )* $field_vis $field: $type),+
        }

        impl $crate::Alistable for $struct_name {
            fn from_alist_with<E: $crate::Evaluator>(
                ctx: &mut TulispContext, alist: &$crate::TulispObject
            ) -> Result<Self, $crate::Error> {
                #[derive(Default)]
                struct Builder {
                    $($field: Option<$type>),+
                }

                impl Builder {
                    fn build(self) -> Result<$struct_name, $crate::Error> {
                        Ok($struct_name {
                            $($field: if let Some(f) = self.$field { f } else {
                                $crate::AsAlist!(
                                    @missing-field
                                    $crate::AsAlist!(@key-name $field $(<$field_key>)?),
                                    $( $($default)+ )?
                                )?}),+
                        })
                    }
                }

                let symbols = $crate::intern!(ctx => {
                    $($field: $crate::AsAlist!(@key-name $field $(<$field_key>)?)),+
                });

                let mut builder = Builder::default();

                for entry in alist.base_iter() {
                    if !entry.consp() {
                        return Err($crate::Error::alist_error(format!(
                            "Alist entry is not a cons pair: {}",
                            entry
                        )));
                    }
                    let key = entry.car()?;
                    let value_owned = entry.cdr()?;
                    let value = &value_owned;
                    $(if key.eq(&symbols.$field) {
                        builder.$field = Some($crate::AsAlist!(@extract-field ctx, value $(, $($default)+)?));
                    } else)+ {
                        return Err($crate::Error::alist_error(format!(
                            "Unexpected key in alist: {}",
                            key
                        )));
                    }
                }

                builder.build()
            }

            fn into_alist(self, ctx: &mut TulispContext) -> $crate::TulispObject {
                let symbols = $crate::intern!(ctx => {
                    $($field: $crate::AsAlist!(@key-name $field $(<$field_key>)?)),+
                });

                $crate::alist::alist_from([
                    $((symbols.$field.clone(), self.$field.into())),+
                ])
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::{alist_from, alist_get};
    use crate::{Alistable, Error, TulispContext};

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

    AsAlist! {
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
    fn test_alistable_into_alist() -> Result<(), Error> {
        // into_alist emits every field including Optional ones (None
        // shows up as nil). That output isn't round-trippable through
        // from_alist when Option<T> is None — Option<T> has no
        // `TryFrom<TulispObject>` impl. Just verify the shape here.
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
        let formatted = format!("{}", obj);
        assert!(formatted.contains("(first-name . \"Carol\")"), "{formatted}");
        assert!(formatted.contains("(age . 40)"), "{formatted}");
        assert!(formatted.contains("(answer . 42)"), "{formatted}");
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
}
