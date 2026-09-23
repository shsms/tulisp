//! [`AsList!`](macro@crate::AsList): a Rust struct read from and
//! written to a keyed Lisp list, as a plist `(:key value ...)` or an
//! alist `((key . value) ...)`.

use std::borrow::Cow;

use crate::{Error, TulispObject};

/// How a keyed list spells its keys and names itself in errors.
#[doc(hidden)]
pub trait Shape {
    /// The shape's name in error messages.
    const NAME: &'static str;
    /// A declared key in this shape's spelling.
    fn key(name: &str) -> Cow<'_, str>;
    /// The error for a malformed list of this shape.
    fn error(message: String) -> Error;
}

/// The plist shape: keys carry a leading `:`.
#[doc(hidden)]
pub struct PlistShape;

impl Shape for PlistShape {
    const NAME: &'static str = "plist";
    fn key(name: &str) -> Cow<'_, str> {
        plist_key(name)
    }
    fn error(message: String) -> Error {
        Error::plist_error(message)
    }
}

/// The alist shape: keys are bare symbols.
#[doc(hidden)]
pub struct AlistShape;

impl Shape for AlistShape {
    const NAME: &'static str = "alist";
    fn key(name: &str) -> Cow<'_, str> {
        Cow::Borrowed(alist_key(name))
    }
    fn error(message: String) -> Error {
        Error::alist_error(message)
    }
}

/// The plist keyword for a declared key: the name with a leading `:`,
/// which is left alone when the name already carries one.
#[doc(hidden)]
pub fn plist_key(name: &str) -> Cow<'_, str> {
    if name.starts_with(':') {
        Cow::Borrowed(name)
    } else {
        Cow::Owned(format!(":{name}"))
    }
}

/// The alist symbol name for a declared key: the name without a
/// leading `:`.
#[doc(hidden)]
pub fn alist_key(name: &str) -> &str {
    name.strip_prefix(':').unwrap_or(name)
}

/// A field's own name as a key: a raw identifier drops its `r#`.
#[doc(hidden)]
pub const fn field_key(name: &'static str) -> &'static str {
    match name.as_bytes() {
        [b'r', b'#', ..] => name.split_at(2).1,
        _ => name,
    }
}

/// The plist keyword spelled from a field's own name: `:r#type`
/// reads `:type`. Drops what [`field_key`] drops.
#[doc(hidden)]
pub fn plist_field_key(key: &'static str) -> Cow<'static, str> {
    match key.strip_prefix(":r#") {
        Some(name) => Cow::Owned(format!(":{name}")),
        None => Cow::Borrowed(key),
    }
}

/// True when no two of `keys` spell the same key once a leading `:`
/// is dropped; evaluated at compile time for every `AsList!` struct.
#[doc(hidden)]
pub const fn distinct_keys(keys: &[&str]) -> bool {
    let mut i = 0;
    while i < keys.len() {
        let mut j = i + 1;
        while j < keys.len() {
            if bytes_eq(bare_key(keys[i]), bare_key(keys[j])) {
                return false;
            }
            j += 1;
        }
        i += 1;
    }
    true
}

const fn bare_key(key: &str) -> &[u8] {
    match key.as_bytes() {
        [b':', rest @ ..] => rest,
        bytes => bytes,
    }
}

pub(crate) const fn bytes_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut k = 0;
    while k < a.len() {
        if a[k] != b[k] {
            return false;
        }
        k += 1;
    }
    true
}

/// Calls `f` on each key and value of the plist slice `kvs`; an odd
/// item count is a plist error. A slice has no list to trace errors
/// to; the call form's span covers a keyword tail.
#[doc(hidden)]
pub fn plist_slice_pairs(
    kvs: &[TulispObject],
    mut f: impl FnMut(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    let (pairs, extra) = kvs.as_chunks::<2>();
    if !extra.is_empty() {
        return Err(Error::plist_error(
            "Expected an even number of items in the plist",
        ));
    }
    pairs.iter().try_for_each(|[key, value]| f(key, value))
}

/// Calls `f` on each key and value of the plist `obj`; a non-list or
/// an odd item count is a plist error, an improper list ends with the
/// list walk's error, and every error is traced to `obj`.
#[doc(hidden)]
pub fn plist_pairs(
    obj: &TulispObject,
    f: impl FnMut(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    plist_pairs_untraced(obj, f).map_err(|e| e.with_trace(obj.clone()))
}

fn plist_pairs_untraced(
    obj: &TulispObject,
    mut f: impl FnMut(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    if !obj.listp() {
        return Err(Error::plist_error(format!("Expected a plist, got: {obj}")));
    }
    let mut items = obj.base_iter();
    while let Some(key) = items.next() {
        let Some(value) = items.next() else {
            items.take_error()?;
            return Err(Error::plist_error(
                "Expected an even number of items in the plist",
            ));
        };
        f(&key, &value)?;
    }
    items.take_error()
}

/// Calls `f` on the key and value of each cons entry of the alist
/// `obj`, skipping other elements as `assq` does; a non-list or a list
/// with no cons entry is an alist error, an improper list ends with
/// the list walk's error, and every error is traced to `obj`.
#[doc(hidden)]
pub fn alist_pairs(
    obj: &TulispObject,
    f: impl FnMut(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    alist_pairs_untraced(obj, f).map_err(|e| e.with_trace(obj.clone()))
}

fn alist_pairs_untraced(
    obj: &TulispObject,
    mut f: impl FnMut(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    if !obj.listp() {
        return Err(Error::alist_error(format!("Expected an alist, got: {obj}")));
    }
    let mut entries = obj.base_iter();
    let mut saw_entry = false;
    for entry in entries.by_ref().filter(TulispObject::consp) {
        saw_entry = true;
        f(&entry.car()?, &entry.cdr()?)?;
    }
    entries.take_error()?;
    if obj.consp() && !saw_entry {
        return Err(Error::alist_error(format!("Expected an alist, got: {obj}")));
    }
    Ok(())
}

/// The element of the list `value` that decides its shape: the first
/// that is a cons (an alist) or a symbol other than nil or t (a
/// plist), if any. An improper list with neither is the list walk's
/// error.
#[doc(hidden)]
pub fn first_cons_or_symbol(value: &TulispObject) -> Result<Option<TulispObject>, Error> {
    let decides = |item: &TulispObject| item.consp() || item.is_symbol_variant();
    let car = value.car()?;
    if decides(&car) {
        return Ok(Some(car));
    }
    let mut items = value.base_iter();
    let first = items.by_ref().find(decides);
    items.take_error()?;
    Ok(first)
}

/// Declares a struct that converts to and from a keyed Lisp list.
///
/// ```text
/// AsList! {
///     [doc comment]
///     [#[lisp(plist | alist)]]      // `into_tulisp`'s shape; plist when omitted
///     [attributes]
///     [pub] struct Name {
///         [field_vis] field[<"key">]: Type [{= default}],
///         ...
///     }
/// }
/// ```
///
/// - `from_tulisp` reads either shape. The first element that is a
///   cons or a symbol decides it: a cons for an alist, whose other
///   elements are skipped as `assq` does, and a symbol for a plist
///   (`nil` and `t` are not symbols here), which is read in pairs from
///   its first element. nil has every field absent, and a list with
///   neither, or a non-list, is a type mismatch. `#[lisp(...)]` comes
///   after the doc comment and before every other attribute; any shape
///   but `plist` or `alist` is a compile error.
/// - Each field's key is `:field` in a plist and `field` in an alist,
///   without the `r#` of a raw identifier; `field<"key">` sets both (a
///   leading `:` is added for the plist key and dropped for the alist
///   key). Two fields with one key are a compile error.
/// - `{= expr}` supplies the value for an absent key. An `Option<T>`
///   field without a default is `None` when absent or nil. Any other
///   field without a default is required.
/// - Field values convert through
///   [`TulispConvertible`](crate::TulispConvertible), so a field may
///   be a primitive, a `Vec`, a raw `TulispObject`, another `AsList!`
///   struct, or an opaque host type.
/// - The first occurrence of a key wins, as with `plist-get`; later
///   ones are ignored.
///
/// The struct implements [`Plistable`](crate::Plistable),
/// [`Alistable`](crate::Alistable) and
/// [`TulispConvertible`](crate::TulispConvertible). The expansion
/// declares `AsListFields`, `AsListPlistKeys`, `AsListAlistKeys` and
/// `AsListShape` in a private block around the impls, where they
/// shadow anything of the same name, so neither the struct's name nor
/// a field's type may be one of those.
///
/// ```compile_fail
/// tulisp::AsList! { #[lisp(plst)] struct Typo { a: i64 } }
/// ```
///
/// ```compile_fail
/// tulisp::AsList! { struct Dup { alpha: i64, beta<":alpha">: i64 } }
/// ```
///
/// # Example
///
/// ```rust
/// use tulisp::{AsList, Plist, TulispContext};
///
/// AsList! {
///     struct Config {
///         host: String,
///         port<":port-number">: i64 {= 8080},
///         scheme: Option<String>,
///     }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("make-server", |cfg: Plist<Config>| -> String {
///     let scheme = cfg.scheme.clone().unwrap_or("http".into());
///     format!("{scheme}://{}:{}", cfg.host, cfg.port)
/// });
/// let url = |ctx: &mut TulispContext, call: &str| {
///     ctx.eval_string(call).unwrap().as_string().unwrap()
/// };
/// assert_eq!(
///     url(&mut ctx, r#"(make-server :host "localhost")"#),
///     "http://localhost:8080"
/// );
/// assert_eq!(
///     url(&mut ctx, r#"(make-server :host "h" :port-number 443 :scheme "https")"#),
///     "https://h:443"
/// );
/// ```
#[macro_export]
macro_rules! AsList {
    // The key a field declares, before the shape's spelling
    // rule adds or drops the leading `:`.
    (@key $field:ident<$key:literal>) => { $key };
    (@key $field:ident) => { $crate::as_list::field_key(stringify!($field)) };

    // A field's key in each shape, spelled at expansion time when
    // the field declares no key of its own.
    (@plist_key $field:ident<$key:literal>) => { &*$crate::as_list::plist_key($key) };
    (@plist_key $field:ident) => {
        &*$crate::as_list::plist_field_key(concat!(":", stringify!($field)))
    };
    (@alist_key $field:ident<$key:literal>) => { $crate::as_list::alist_key($key) };
    (@alist_key $field:ident) => { $crate::as_list::field_key(stringify!($field)) };

    // The value of a field whose key is absent from the list: its
    // default, `from_absent` for a field that may be absent, an error
    // otherwise.
    (@absent $ctx:ident, $shape:ty, $key:expr, $type:ty, $default:expr) => { Ok($default) };
    (@absent $ctx:ident, $shape:ty, $key:expr, $type:ty,) => {
        if <$type as $crate::TulispConvertible>::REQUIRED {
            Err(<$shape as $crate::as_list::Shape>::error(format!(
                "Missing {} field",
                <$shape as $crate::as_list::Shape>::key($key)
            )))
        } else {
            <$type as $crate::TulispConvertible>::from_absent($ctx)
                .map_err(|__e| {
                    <$shape as $crate::as_list::Shape>::error(format!(
                        "{} field: {__e}",
                        <$shape as $crate::as_list::Shape>::key($key)
                    ))
                })
        }
    };

    // The serializer and the shape marker for the declared shape.
    (@render alist) => { $crate::Alistable::into_alist };
    (@render plist) => { $crate::Plistable::into_plist };
    (@shape alist) => { $crate::as_list::AlistShape };
    (@shape plist) => { $crate::as_list::PlistShape };

    // Matches `key` against the interned keys, in field order, and
    // stores the converted value unless the key was already seen; an
    // unknown key is an error.
    (@match $shape:ty, $key:ident, $value:expr, $ctx:ident, $fields:ident, $keys:ident, ($($field:ident),+)) => {
        $(
            if $key.eq(&$keys.$field) {
                if $fields.$field.is_none() {
                    $fields.$field = Some(
                        $crate::TulispConvertible::from_tulisp($ctx, $value)
                            .map_err(|__e| __e.with_trace($value.clone()))?,
                    );
                }
            } else
        )+ {
            return Err(<$shape as $crate::as_list::Shape>::error(format!(
                "Unexpected key in {}: {}",
                <$shape as $crate::as_list::Shape>::NAME,
                $key
            )));
        }
    };

    (
        @emit $shape:ident,
        $( #[$meta:meta] )*
        $vis:vis struct $name:ident {
            $(
                $( #[$($field_meta:tt)+] )*
                $field_vis:vis $field:ident $(<$key:literal>)? : $type:ty
                $({= $($default:tt)+ })?
            ),+ $(,)?
        }
    ) => {
        $( #[$meta] )*
        $vis struct $name {
            $( $( #[$($field_meta)+] )* $field_vis $field: $type ),+
        }

        const _: () = {
            /// The fields read so far; `None` is a key the list did
            /// not carry.
            #[derive(Default)]
            struct AsListFields {
                $( $field: Option<$type> ),+
            }

            const _: () = assert!(
                $crate::as_list::distinct_keys(&[
                    $( $crate::AsList!(@key $field $(<$key>)?) ),+
                ]),
                "AsList! fields must map to distinct keys"
            );

            $crate::intern!(struct AsListPlistKeys {
                $( $field: $crate::AsList!(@plist_key $field $(<$key>)?) ),+
            });

            $crate::intern!(struct AsListAlistKeys {
                $( $field: $crate::AsList!(@alist_key $field $(<$key>)?) ),+
            });

            /// Fills every key the list did not carry, naming an
            /// absent one the way `shape` spells it.
            fn build<AsListShape: $crate::as_list::Shape>(
                __ctx: &mut $crate::TulispContext,
                __shape: AsListShape,
                __fields: AsListFields,
            ) -> Result<$name, $crate::Error> {
                Ok($name {
                    $( $field: match __fields.$field {
                        Some(__value) => __value,
                        None => $crate::AsList!(
                            @absent __ctx, AsListShape, $crate::AsList!(@key $field $(<$key>)?),
                            $type, $( $($default)+ )?
                        )?,
                    } ),+
                })
            }

            impl $crate::Plistable for $name {
                fn from_plist_as_slice(
                    __ctx: &mut $crate::TulispContext,
                    __kvs: &[$crate::TulispObject],
                ) -> Result<Self, $crate::Error> {
                    let mut __fields = AsListFields::default();
                    let __keys = AsListPlistKeys::new(__ctx);
                    $crate::as_list::plist_slice_pairs(__kvs, |__key, __value| {
                        $crate::AsList!(
                            @match $crate::as_list::PlistShape, __key, __value, __ctx, __fields, __keys,
                            ($( $field ),+)
                        );
                        Ok(())
                    })?;
                    build(__ctx, $crate::as_list::PlistShape, __fields)
                }

                fn from_plist(
                    __ctx: &mut $crate::TulispContext,
                    __obj: &$crate::TulispObject,
                ) -> Result<Self, $crate::Error> {
                    let mut __fields = AsListFields::default();
                    let __keys = AsListPlistKeys::new(__ctx);
                    $crate::as_list::plist_pairs(__obj, |__key, __value| {
                        $crate::AsList!(
                            @match $crate::as_list::PlistShape, __key, __value, __ctx, __fields, __keys,
                            ($( $field ),+)
                        );
                        Ok(())
                    })?;
                    build(__ctx, $crate::as_list::PlistShape, __fields)
                        .map_err(|__e| __e.with_trace(__obj.clone()))
                }

                fn into_plist(self, __ctx: &mut $crate::TulispContext) -> $crate::TulispObject {
                    let __keys = AsListPlistKeys::new(__ctx);
                    $crate::plist::plist_from([
                        $( (
                            __keys.$field,
                            $crate::TulispConvertible::into_tulisp(self.$field, __ctx),
                        ) ),+
                    ])
                }
            }

            impl $crate::Alistable for $name {
                fn from_alist(
                    __ctx: &mut $crate::TulispContext,
                    __alist: &$crate::TulispObject,
                ) -> Result<Self, $crate::Error> {
                    let mut __fields = AsListFields::default();
                    let __keys = AsListAlistKeys::new(__ctx);
                    $crate::as_list::alist_pairs(__alist, |__key, __value| {
                        $crate::AsList!(
                            @match $crate::as_list::AlistShape, __key, __value, __ctx, __fields, __keys,
                            ($( $field ),+)
                        );
                        Ok(())
                    })?;
                    build(__ctx, $crate::as_list::AlistShape, __fields)
                        .map_err(|__e| __e.with_trace(__alist.clone()))
                }

                fn into_alist(self, __ctx: &mut $crate::TulispContext) -> $crate::TulispObject {
                    let __keys = AsListAlistKeys::new(__ctx);
                    $crate::alist::alist_from([
                        $( (
                            __keys.$field,
                            $crate::TulispConvertible::into_tulisp(self.$field, __ctx),
                        ) ),+
                    ])
                }
            }

            impl $crate::TulispConvertible for $name {
                fn from_tulisp(
                    __ctx: &mut $crate::TulispContext,
                    __value: &$crate::TulispObject,
                ) -> Result<Self, $crate::Error> {
                    let __mismatch = || {
                        $crate::Error::type_mismatch(format!(
                            "Expected a plist or alist for {}, got: {__value}",
                            stringify!($name)
                        ))
                        .with_trace(__value.clone())
                    };
                    if __value.null() {
                        return build(__ctx, $crate::AsList!(@shape $shape), AsListFields::default())
                            .map_err(|__e| __e.with_trace(__value.clone()));
                    }
                    if !__value.consp() {
                        return Err(__mismatch());
                    }
                    match $crate::as_list::first_cons_or_symbol(__value).map_err(|_| __mismatch())? {
                        Some(__first) if __first.consp() => {
                            <Self as $crate::Alistable>::from_alist(__ctx, __value)
                        }
                        Some(_) => <Self as $crate::Plistable>::from_plist(__ctx, __value),
                        None => Err(__mismatch()),
                    }
                }

                fn into_tulisp(self, __ctx: &mut $crate::TulispContext) -> $crate::TulispObject {
                    $crate::AsList!(@render $shape)(self, __ctx)
                }
            }
        };
    };

    // The declared shape, defaulting to a plist.
    ($( #[doc = $doc:literal] )* #[lisp(plist)] $($rest:tt)*) => {
        $crate::AsList!(@emit plist, $( #[doc = $doc] )* $($rest)*);
    };
    ($( #[doc = $doc:literal] )* #[lisp(alist)] $($rest:tt)*) => {
        $crate::AsList!(@emit alist, $( #[doc = $doc] )* $($rest)*);
    };
    ($( #[doc = $doc:literal] )* #[lisp($shape:ident)] $($rest:tt)*) => {
        compile_error!(concat!(
            "unknown AsList! shape #[lisp(",
            stringify!($shape),
            ")]; the shapes are #[lisp(plist)] and #[lisp(alist)]"
        ));
    };
    ($( #[$meta:meta] )* $vis:vis struct $($rest:tt)*) => {
        $crate::AsList!(@emit plist, $( #[$meta] )* $vis struct $($rest)*);
    };
}

#[cfg(test)]
mod tests {
    use crate::test_utils::eval_assert_equal;
    use crate::{Alistable, Plist, Plistable, TulispContext, TulispConvertible, TulispObject};

    crate::AsList! {
        #[lisp(plist)]
        #[derive(Debug, PartialEq)]
        struct Inner {
            n: i64,
        }
    }

    // A one-letter struct name must not collide with anything the
    // expansion declares.
    crate::AsList! {
        #[derive(Debug, PartialEq)]
        struct S {
            a: i64 {= 1},
        }
    }

    crate::AsList! {
        // `TulispObject` has no `PartialEq`, so a struct holding a
        // raw field derives only `Debug`.
        #[derive(Debug)]
        struct Config {
            host: String,
            port<":port-number">: i64 {= 8080},
            scheme: Option<String>,
            extra: TulispObject {= TulispObject::nil()},
            inner: Option<Inner>,
        }
    }

    crate::AsList! {
        /// Written as an alist.
        #[lisp(alist)]
        #[derive(Debug, PartialEq)]
        struct Pair {
            a: i64,
            b: i64 {= 0},
        }
    }

    crate::AsList! {
        #[derive(Debug, PartialEq)]
        struct Defaults {
            a: i64 {= 1},
            b: String {= "b".to_string()},
        }
    }

    crate::AsList! {
        #[derive(Debug, PartialEq)]
        struct AllOptional {
            a: Option<i64>,
            b: Option<String>,
        }
    }

    fn config(ctx: &mut TulispContext, source: &str) -> Config {
        let value = ctx.eval_string(source).unwrap();
        Config::from_tulisp(ctx, &value).unwrap()
    }

    #[test]
    fn a_plist_reads_into_the_struct() {
        let mut ctx = TulispContext::new();
        let c = config(&mut ctx, r#"'(:host "h" :port-number 1 :scheme "s")"#);
        assert_eq!(c.host, "h");
        assert_eq!(c.port, 1);
        assert_eq!(c.scheme, Some("s".to_string()));
        assert!(c.extra.null());
        assert_eq!(c.inner, None);
    }

    #[test]
    fn an_alist_reads_into_the_same_struct() {
        let mut ctx = TulispContext::new();
        let c = config(
            &mut ctx,
            r#"'((host . "h") (port-number . 2) (inner . (:n 5)))"#,
        );
        assert_eq!(c.host, "h");
        assert_eq!(c.port, 2);
        assert_eq!(c.inner, Some(Inner { n: 5 }));
    }

    #[test]
    fn nil_reads_as_every_field_absent() {
        let mut ctx = TulispContext::new();
        let err = Pair::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap_err();
        assert!(
            err.to_string().contains("AlistError: Missing a field"),
            "{err}"
        );
        let value = ctx.eval_string("'(:a 1)").unwrap();
        assert_eq!(
            Pair::from_tulisp(&mut ctx, &value).unwrap(),
            Pair { a: 1, b: 0 }
        );
    }

    #[test]
    fn optional_fields_take_absent_default_and_explicit_nil() {
        let mut ctx = TulispContext::new();
        let c = config(&mut ctx, r#"'(:host "h")"#);
        assert_eq!(c.port, 8080);
        assert_eq!(c.scheme, None);
        let c = config(&mut ctx, r#"'(:host "h" :scheme nil)"#);
        assert_eq!(c.scheme, None);
    }

    #[test]
    fn a_raw_field_keeps_the_lisp_value() {
        let mut ctx = TulispContext::new();
        let c = config(&mut ctx, r#"'(:host "h" :extra (1 2))"#);
        assert_eq!(c.extra.to_string(), "(1 2)");
    }

    #[test]
    fn missing_required_unknown_and_odd_keys_are_errors() {
        let mut ctx = TulispContext::new();
        for (source, message) in [
            ("'(:port-number 1)", "Missing :host field"),
            (r#"'(:host "h" :bogus 1)"#, "Unexpected key"),
            (r#"'(:host "h" :port-number)"#, "even number"),
            (r#"'((bogus . 1))"#, "Unexpected key"),
            ("'(1 2)", "Expected a plist or alist"),
            ("7", "Expected a plist or alist"),
        ] {
            let value = ctx.eval_string(source).unwrap();
            let err = Config::from_tulisp(&mut ctx, &value).unwrap_err();
            assert!(err.to_string().contains(message), "{source}: {err}");
        }
    }

    #[test]
    fn to_lisp_renders_the_declared_shape() {
        let mut ctx = TulispContext::new();
        let c = Config {
            host: "h".into(),
            port: 1,
            scheme: None,
            extra: TulispObject::nil(),
            inner: Some(Inner { n: 2 }),
        };
        assert_eq!(
            c.into_tulisp(&mut ctx).to_string(),
            r#"(:host "h" :port-number 1 :scheme nil :extra nil :inner (:n 2))"#
        );
        assert_eq!(
            Pair { a: 1, b: 2 }.into_tulisp(&mut ctx).to_string(),
            "((a . 1) (b . 2))"
        );
        assert_eq!(
            Pair { a: 1, b: 2 }.into_plist(&mut ctx).to_string(),
            "(:a 1 :b 2)"
        );
        let value = ctx.eval_string("'((a . 3))").unwrap();
        assert_eq!(
            Pair::from_alist(&mut ctx, &value).unwrap(),
            Pair { a: 3, b: 0 }
        );
    }

    #[test]
    fn a_struct_whose_fields_all_default_reads_from_nil() {
        let mut ctx = TulispContext::new();
        assert_eq!(
            Defaults::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap(),
            Defaults {
                a: 1,
                b: "b".to_string()
            }
        );
    }

    #[test]
    fn a_defaulted_field_takes_the_key_the_list_carries() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string("'(:a 7)").unwrap();
        assert_eq!(
            Defaults::from_tulisp(&mut ctx, &value).unwrap(),
            Defaults {
                a: 7,
                b: "b".to_string()
            }
        );
    }

    #[test]
    fn a_struct_whose_fields_are_all_optional_reads_from_nil_as_none() {
        let mut ctx = TulispContext::new();
        assert_eq!(
            AllOptional::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap(),
            AllOptional { a: None, b: None }
        );
    }

    #[test]
    fn a_struct_is_a_defun_parameter_a_return_and_a_keyword_tail() {
        let mut ctx = TulispContext::new();
        ctx.defun("pair-sum", |p: Pair| -> i64 { p.a + p.b });
        ctx.defun("make-pair", |a: i64, b: i64| Pair { a, b });
        ctx.defun("kw-sum", |p: Plist<Pair>| -> i64 { p.a + p.b });
        eval_assert_equal(&mut ctx, "(pair-sum '(:a 1 :b 2))", "3");
        eval_assert_equal(&mut ctx, "(pair-sum (make-pair 2 3))", "5");
        eval_assert_equal(&mut ctx, "(kw-sum :a 1 :b 2)", "3");
        let err = ctx.eval_string("(kw-sum :a 1 :b)").unwrap_err();
        assert!(err.to_string().contains("even number of items"), "{err}");
    }

    crate::AsList! {
        #[derive(Debug)]
        struct Raw {
            r#type: i64,
        }
    }

    // Constants spelled like the generated bindings must not capture
    // them: an identifier pattern resolves to a constant in scope.
    #[allow(non_upper_case_globals, dead_code)]
    mod beside_constants {
        const ctx: &str = "";
        const fields: &str = "";
        const keys: &str = "";
        const key: &str = "";
        const value: &str = "";
        const obj: &str = "";
        const kvs: &str = "";
        const alist: &str = "";
        const mismatch: &str = "";
        const first: &str = "";
        const shape: &str = "";
        const e: &str = "";
        crate::AsList! {
            #[derive(Debug, PartialEq)]
            pub struct Shadowed {
                pub a: i64,
                pub b: Option<i64>,
            }
        }
    }

    #[test]
    fn an_as_list_beside_same_named_constants_compiles() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string("'(:a 1)").unwrap();
        let s = beside_constants::Shadowed::from_tulisp(&mut ctx, &value).unwrap();
        assert_eq!(s, beside_constants::Shadowed { a: 1, b: None });
    }

    #[test]
    fn a_raw_identifier_field_is_keyed_without_the_prefix() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string("'(:type 4)").unwrap();
        let raw = Raw::from_tulisp(&mut ctx, &value).unwrap();
        assert_eq!(raw.r#type, 4);
        assert_eq!(raw.into_tulisp(&mut ctx).to_string(), "(:type 4)");
        let value = ctx.eval_string("'((type . 5))").unwrap();
        assert_eq!(Raw::from_tulisp(&mut ctx, &value).unwrap().r#type, 5);
        let err = Raw::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap_err();
        assert!(err.to_string().contains("Missing :type field"), "{err}");
    }

    #[test]
    fn elements_that_are_not_conses_do_not_take_part_in_an_alist() {
        let mut ctx = TulispContext::new();
        for source in [
            r#"'(1 (host . "h") "x" (inner . ((n . 5))))"#,
            r#"'((host . "h") 1 (inner . ((n . 5))))"#,
        ] {
            let c = config(&mut ctx, source);
            assert_eq!(c.host, "h", "{source}");
            assert_eq!(c.inner.unwrap().n, 5, "{source}");
        }
        let value = ctx.eval_string(r#"'(:host "h")"#).unwrap();
        let err = Config::from_alist(&mut ctx, &value).unwrap_err();
        assert!(
            err.to_string()
                .contains(r#"Expected an alist, got: (:host "h")"#),
            "{err}"
        );
    }

    #[test]
    fn an_atom_before_the_first_symbol_is_a_plist_key() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string(r#"'(1 :host "h")"#).unwrap();
        let err = Config::from_tulisp(&mut ctx, &value).unwrap_err();
        assert!(
            err.to_string().contains("Unexpected key in plist: 1"),
            "{err}"
        );
    }

    #[test]
    fn an_error_points_at_its_list() {
        let mut ctx = TulispContext::new();
        for (source, formatted) in [
            (
                r#"'(:host "h" :bogus 1)"#,
                concat!(
                    "ERR PlistError: Unexpected key in plist: :bogus\n",
                    "<eval_string>:1.2-1.21:  at (:host \"h\" :bogus 1)\n"
                ),
            ),
            (
                r#"'(:host "h" :port-number)"#,
                concat!(
                    "ERR PlistError: Expected an even number of items in the plist\n",
                    "<eval_string>:1.2-1.25:  at (:host \"h\" :port-number)\n"
                ),
            ),
            (
                r#"'((bogus . 1))"#,
                concat!(
                    "ERR AlistError: Unexpected key in alist: bogus\n",
                    "<eval_string>:1.2-1.14:  at ((bogus . 1))\n"
                ),
            ),
            (
                r#"'(:inner (:m 1))"#,
                concat!(
                    "ERR PlistError: Unexpected key in plist: :m\n",
                    "<eval_string>:1.10-1.15:  at (:m 1)\n",
                    "<eval_string>:1.2-1.16:  at (:inner (:m 1))\n"
                ),
            ),
            (
                r#"'(:inner (:n 1))"#,
                concat!(
                    "ERR PlistError: Missing :host field\n",
                    "<eval_string>:1.2-1.16:  at (:inner (:n 1))\n"
                ),
            ),
            (
                "nil",
                concat!(
                    "ERR PlistError: Missing :host field\n",
                    "<eval_string>:1.1-1.3:  at nil\n"
                ),
            ),
            (
                r#"'((port-number . 1))"#,
                concat!(
                    "ERR AlistError: Missing host field\n",
                    "<eval_string>:1.2-1.20:  at ((port-number . 1))\n"
                ),
            ),
            (
                r#"'(1 2 . 3)"#,
                concat!(
                    "ERR TypeMismatch: Expected a plist or alist for Config, got: (1 2 . 3)\n",
                    "<eval_string>:1.2-1.10:  at (1 2 . 3)\n"
                ),
            ),
        ] {
            let value = ctx.eval_string(source).unwrap();
            let err = Config::from_tulisp(&mut ctx, &value).unwrap_err();
            assert_eq!(err.format(&ctx), formatted, "{source}");
        }
    }

    #[test]
    fn a_dotted_or_circular_list_and_an_atom_are_errors() {
        let mut ctx = TulispContext::new();
        for (source, message) in [
            (r#"'(:host "h" . 1)"#, "Expected list"),
            (r#"'(:host "h" :port-number . 1)"#, "Expected list"),
            (r#"'((host . "h") . 1)"#, "Expected list"),
            (
                r#"(let ((l (list :host "h"))) (setcdr (cdr l) l) l)"#,
                "Circular",
            ),
        ] {
            let value = ctx.eval_string(source).unwrap();
            let err = Config::from_tulisp(&mut ctx, &value).unwrap_err();
            assert!(err.to_string().contains(message), "{source}: {err}");
        }
        let seven = ctx.eval_string("7").unwrap();
        let err = Config::from_plist(&mut ctx, &seven).unwrap_err();
        assert!(
            err.to_string().contains("Expected a plist, got: 7"),
            "{err}"
        );
        let err = Config::from_alist(&mut ctx, &seven).unwrap_err();
        assert!(
            err.to_string().contains("Expected an alist, got: 7"),
            "{err}"
        );
    }

    #[test]
    fn a_failing_nested_field_carries_a_trace() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string(r#"'(:host "h" :inner (:n "x"))"#).unwrap();
        let err = Config::from_tulisp(&mut ctx, &value).unwrap_err();
        assert!(
            err.format(&ctx).contains("at (:n \"x\")"),
            "{}",
            err.format(&ctx)
        );
    }

    #[test]
    fn a_struct_named_s_expands() {
        let mut ctx = TulispContext::new();
        assert_eq!(
            S::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap(),
            S { a: 1 }
        );
    }

    #[test]
    fn the_first_occurrence_of_a_key_wins() {
        let mut ctx = TulispContext::new();
        let value = ctx.eval_string(r#"'(:host "a" :host "b")"#).unwrap();
        assert_eq!(Config::from_tulisp(&mut ctx, &value).unwrap().host, "a");
        let value = ctx.eval_string("'((a . 1) (a . 2))").unwrap();
        assert_eq!(
            Pair::from_tulisp(&mut ctx, &value).unwrap(),
            Pair { a: 1, b: 0 }
        );
    }
}
