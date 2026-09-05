use crate::{
    Error, Number, TulispContext, TulispConvertible, TulispObject, TulispValue,
    object::wrappers::generic::{Shared, SharedMut},
};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Key-comparison mode of a table, per Emacs `make-hash-table`'s
/// `:test` argument.
#[derive(Clone, Copy, PartialEq)]
enum HashTest {
    Eq,
    Eql,
    Equal,
}

/// A key together with its table's comparison mode, so `Hash` and
/// `Eq` agree with the test the table was created with.
#[derive(Clone)]
struct HashKey {
    obj: TulispObject,
    test: HashTest,
}

/// Hashes `obj` the same way `equal` compares it: strings by
/// contents, numbers by kind and value, cons cells and quote forms
/// by their contents, `nil` and `t` by fixed tags (each is a fresh
/// object on every read, so it has no fixed address), host values by
/// shared payload, everything else by object identity.
fn equal_hash<H: Hasher>(obj: &TulispObject, state: &mut H) {
    if let Ok(s) = obj.as_string() {
        state.write_u8(1);
        s.hash(state);
    } else if let Ok(i) = obj.as_int() {
        state.write_u8(2);
        i.hash(state);
    } else if let Ok(f) = obj.as_float() {
        state.write_u8(3);
        f.to_bits().hash(state);
    } else if obj.consp() {
        state.write_u8(4);
        if let (Ok(car), Ok(cdr)) = (obj.car(), obj.cdr()) {
            equal_hash(&car, state);
            equal_hash(&cdr, state);
        }
    } else {
        match &obj.inner_ref().0 {
            TulispValue::Nil => state.write_u8(5),
            TulispValue::T => state.write_u8(6),
            TulispValue::Quote { value } => {
                state.write_u8(7);
                equal_hash(value, state);
            }
            TulispValue::Sharpquote { value } => {
                state.write_u8(8);
                equal_hash(value, state);
            }
            TulispValue::Backquote { value } => {
                state.write_u8(9);
                equal_hash(value, state);
            }
            TulispValue::Unquote { value } => {
                state.write_u8(10);
                equal_hash(value, state);
            }
            TulispValue::Splice { value } => {
                state.write_u8(11);
                equal_hash(value, state);
            }
            TulispValue::Any(value) => {
                state.write_u8(13);
                state.write_usize(value.addr_as_usize());
            }
            // A lexical binding is `eq` to its symbol.
            TulispValue::LexicalBinding { binding } => {
                state.write_u8(12);
                state.write_usize(binding.symbol().addr_as_usize());
            }
            _ => {
                state.write_u8(12);
                state.write_usize(obj.addr_as_usize());
            }
        }
    }
}

/// What makes an object one `eq` key: `nil` and `t` are one key each
/// (every read of them is a fresh object), everything else is its
/// address.
#[derive(PartialEq, Eq, Hash)]
enum EqKey {
    Nil,
    T,
    Addr(usize),
}

/// The `eq` key of `obj`. A `nil` key must stay `nil` while it is in
/// a table: pushing onto it from Rust turns it into a list, which
/// changes its key. A lexical binding shares its symbol's key.
fn identity_key(obj: &TulispObject) -> EqKey {
    match &obj.inner_ref().0 {
        TulispValue::Nil => EqKey::Nil,
        TulispValue::T => EqKey::T,
        TulispValue::LexicalBinding { binding } => EqKey::Addr(binding.symbol().addr_as_usize()),
        _ => EqKey::Addr(obj.addr_as_usize()),
    }
}

impl Hash for HashKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.test {
            HashTest::Eq => identity_key(&self.obj).hash(state),
            HashTest::Eql => {
                // Copy the number out first: `identity_key` reads the
                // object again, so this borrow must be gone by then.
                let number = match &self.obj.inner_ref().0 {
                    TulispValue::Number { value, .. } => Some(*value),
                    _ => None,
                };
                match number {
                    Some(Number::Int(i)) => i.hash(state),
                    Some(Number::Float(f)) => f.to_bits().hash(state),
                    None => identity_key(&self.obj).hash(state),
                }
            }
            HashTest::Equal => equal_hash(&self.obj, state),
        }
    }
}

impl PartialEq for HashKey {
    fn eq(&self, other: &Self) -> bool {
        match self.test {
            HashTest::Eq => identity_key(&self.obj) == identity_key(&other.obj),
            HashTest::Eql => self.obj.eql(&other.obj),
            HashTest::Equal => self.obj.equal(&other.obj),
        }
    }
}
impl Eq for HashKey {}

#[derive(Clone)]
pub(crate) struct HashTable {
    inner: SharedMut<HashMap<HashKey, TulispObject>>,
    test: HashTest,
}

impl HashTable {
    fn key(&self, obj: TulispObject) -> HashKey {
        HashKey {
            obj,
            test: self.test,
        }
    }
}

impl std::fmt::Display for HashTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<hash-table>")
    }
}

impl TulispConvertible for HashTable {
    fn from_tulisp(value: &TulispObject) -> Result<HashTable, Error> {
        value
            .as_any()
            .ok()
            .and_then(|v| v.downcast_ref::<HashTable>().cloned())
            .ok_or_else(|| {
                Error::type_mismatch(format!("Expected hash-table, got: {value}"))
                    .with_trace(value.clone())
            })
    }
    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

/// Parses `make-hash-table`'s keyword arguments. `:test` selects the
/// comparison mode; `:size` is accepted as a hint and ignored.
fn parse_keyword_args(args: crate::Rest<TulispObject>) -> Result<HashTest, Error> {
    let mut test = HashTest::Eql;
    let mut iter = args.into_iter();
    while let Some(kw) = iter.next() {
        let name = kw.as_symbol().unwrap_or_else(|_| kw.to_string());
        let value = iter.next().ok_or_else(|| {
            Error::invalid_argument(format!("Missing keyword value: {name}")).with_trace(kw.clone())
        })?;
        match name.as_str() {
            ":test" => {
                test = match value.as_symbol().as_deref() {
                    Ok("eq") => HashTest::Eq,
                    Ok("eql") => HashTest::Eql,
                    Ok("equal") => HashTest::Equal,
                    _ => {
                        return Err(Error::invalid_argument(format!(
                            "Invalid hash table test: {value}"
                        ))
                        .with_trace(value));
                    }
                }
            }
            ":size" => {}
            _ => {
                return Err(
                    Error::invalid_argument(format!("Invalid argument list: {name}"))
                        .with_trace(kw),
                );
            }
        }
    }
    Ok(test)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "make-hash-table",
        |args: crate::Rest<TulispObject>| -> Result<HashTable, Error> {
            Ok(HashTable {
                inner: SharedMut::new(HashMap::new()),
                test: parse_keyword_args(args)?,
            })
        },
    );

    ctx.defun(
        "gethash",
        |key: TulispObject, table: HashTable, default: Option<TulispObject>| -> TulispObject {
            // Match Emacs `(gethash KEY TABLE &optional DEFAULT)` —
            // returns DEFAULT (nil if omitted) when KEY isn't present.
            let key = table.key(key);
            table
                .inner
                .borrow()
                .get(&key)
                .cloned()
                .unwrap_or_else(|| default.unwrap_or_else(TulispObject::nil))
        },
    );

    ctx.defun(
        "puthash",
        |key: TulispObject, value: TulispObject, table: HashTable| {
            let key = table.key(key);
            table.inner.borrow_mut().insert(key, value);
        },
    );
}

#[cfg(test)]
mod tests {
    use super::{HashKey, HashTest};
    use crate::test_utils::{eval_assert_equal, eval_assert_error};
    use crate::{Error, Shared, TulispContext, TulispObject};

    // A lexical binding is `eq` to its symbol, so it must be `eql` to
    // it and hash like it under every table test.
    #[test]
    fn a_lexical_binding_keys_like_its_symbol() {
        use std::hash::{Hash, Hasher};
        fn hash_of(key: &HashKey) -> u64 {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            key.hash(&mut hasher);
            hasher.finish()
        }
        let ctx = &mut TulispContext::new();
        let symbol = ctx.intern("x");
        let binding = TulispObject::lexical_binding(ctx.lex_allocator.clone(), symbol.clone());
        assert!(symbol.eq(&binding) && binding.eq(&symbol));
        assert!(symbol.eql(&binding) && binding.eql(&symbol));
        for test in [HashTest::Eq, HashTest::Eql, HashTest::Equal] {
            let symbol_key = HashKey {
                obj: symbol.clone(),
                test,
            };
            let binding_key = HashKey {
                obj: binding.clone(),
                test,
            };
            assert!(symbol_key == binding_key);
            assert_eq!(hash_of(&symbol_key), hash_of(&binding_key));
        }
    }

    #[test]
    fn test_hash_table_tests() {
        let mut ctx = TulispContext::new();
        // `:test 'equal` compares keys structurally, so distinct
        // string and list objects with equal contents hit the same
        // entry.
        eval_assert_equal(
            &mut ctx,
            r#"(let ((h (make-hash-table :test 'equal))) (puthash "k" 1 h) (gethash "k" h))"#,
            "1",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash '(1 2) 5 h) (gethash '(1 2) h))",
            "5",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'eq))) (puthash 'a 1 h) (gethash 'a h))",
            "1",
        );
        // Under `eq`, a re-read string is a different object and
        // misses — this is what separates `eq` from `equal`.
        eval_assert_equal(
            &mut ctx,
            r#"(let ((h (make-hash-table :test 'eq))) (puthash "k" 1 h) (gethash "k" h 'missing))"#,
            "'missing",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'eql))) (puthash 1.5 1 h) (gethash 1.5 h))",
            "1",
        );
        // The default test remains `eql`: numbers match by value, but
        // a re-read string is a different object and misses.
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table))) (puthash 10 'x h) (gethash 10 h))",
            "'x",
        );
        eval_assert_equal(
            &mut ctx,
            r#"(let ((h (make-hash-table))) (puthash "k" 1 h) (gethash "k" h 'missing))"#,
            "'missing",
        );
        // `:size` is accepted as a hint and ignored.
        eval_assert_equal(
            &mut ctx,
            r#"(let ((h (make-hash-table :size 10 :test 'equal))) (puthash "a" 2 h) (gethash "a" h))"#,
            "2",
        );
        eval_assert_error(
            &mut ctx,
            "(make-hash-table :best 'equal)",
            r#"ERR InvalidArgument: Invalid argument list: :best
<eval_string>:1.1-1.30:  at (make-hash-table :best 'equal)
"#,
        );
        eval_assert_error(
            &mut ctx,
            "(make-hash-table :test 'foo)",
            r#"ERR InvalidArgument: Invalid hash table test: foo
<eval_string>:1.1-1.28:  at (make-hash-table :test 'foo)
"#,
        );
        eval_assert_error(
            &mut ctx,
            "(make-hash-table :test)",
            r#"ERR InvalidArgument: Missing keyword value: :test
<eval_string>:1.1-1.23:  at (make-hash-table :test)
"#,
        );
    }

    #[test]
    fn hash_key_eq_follows_table_test() -> Result<(), Error> {
        // Pin `HashKey`'s `Eq` directly; the hasher alone can hide a
        // wrong `Eq` by keeping keys in different buckets.
        let mut ctx = TulispContext::new();
        let key = |obj: &TulispObject, test| HashKey {
            obj: obj.clone(),
            test,
        };
        let int: TulispObject = 5.into();
        let float: TulispObject = 5.0.into();
        let neg_zero: TulispObject = (-0.0).into();
        let zero: TulispObject = 0.0.into();
        let f1 = ctx.eval_string("(lambda (x) x)")?;
        let f2 = ctx.eval_string("(lambda (x) x)")?;
        let nil_a = TulispObject::nil();
        let nil_b = TulispObject::nil();
        let t_a = TulispObject::t();
        let t_b = TulispObject::t();
        for test in [HashTest::Eq, HashTest::Eql] {
            assert!(key(&nil_a, test) == key(&nil_b, test));
            assert!(key(&t_a, test) == key(&t_b, test));
            assert!(key(&nil_a, test) != key(&t_a, test));
        }
        for test in [HashTest::Eql, HashTest::Equal] {
            assert!(key(&int, test) == key(&int, test));
            assert!(key(&int, test) != key(&float, test));
            assert!(key(&zero, test) != key(&neg_zero, test));
            assert!(key(&f1, test) == key(&f1, test));
            assert!(key(&f1, test) != key(&f2, test));
        }
        Ok(())
    }

    #[test]
    fn eql_table_distinguishes_int_and_float_keys() {
        // `(eql 5 5.0)` is nil, so 5 and 5.0 are different keys.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table))) (puthash 5 'a h) (gethash 5.0 h 'missing))",
            "'missing",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table))) (puthash 5.0 'a h) (gethash 5 h 'missing))",
            "'missing",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table))) (puthash 5 'a h) (puthash 5.0 'b h) (list (gethash 5 h) (gethash 5.0 h)))",
            "'(a b)",
        );
    }

    #[test]
    fn eql_table_float_keys_are_bit_exact() {
        // 0.0 and -0.0 are two keys; a NaN key finds itself.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table))) (puthash 0.0 'a h) (puthash -0.0 'b h) (list (gethash 0.0 h) (gethash -0.0 h)))",
            "'(a b)",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table)) (n (/ 0.0 0.0))) (puthash n 'a h) (gethash n h 'missing))",
            "'a",
        );
    }

    #[test]
    fn equal_table_is_type_strict_on_numbers() {
        // `(equal 1 1.0)` is nil, so 1 and 1.0 are different keys.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash 1 'i h) (gethash 1.0 h 'missing))",
            "'missing",
        );
    }

    #[test]
    fn equal_table_finds_structural_keys() {
        // nil, t and quoted forms are fresh objects on each read, so
        // they must hash by structure.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash nil 1 h) (gethash nil h 'missing))",
            "1",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash t 1 h) (gethash t h 'missing))",
            "1",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash ''a 1 h) (gethash ''a h 'missing))",
            "1",
        );
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal))) (puthash '(1 (2 \"s\")) 1 h) (gethash '(1 (2 \"s\")) h 'missing))",
            "1",
        );
    }

    #[test]
    fn equal_table_compares_exotic_keys_by_identity() {
        // Only the same lambda object finds the entry.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((h (make-hash-table :test 'equal)) (f (lambda (x) x))) (puthash f 1 h) (list (gethash f h) (gethash (lambda (x) x) h 'missing)))",
            "'(1 missing)",
        );
    }

    #[test]
    fn equal_table_finds_host_value_through_another_wrapper() {
        // One host value handed to Lisp twice is one `equal` key.
        struct Host;
        impl std::fmt::Display for Host {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("host")
            }
        }
        let shared = Shared::new(Host);
        let a: TulispObject = shared.clone().into();
        let b: TulispObject = shared.into();
        let c: TulispObject = Shared::new(Host).into();
        let key = |obj: &TulispObject| HashKey {
            obj: obj.clone(),
            test: HashTest::Equal,
        };
        assert!(key(&a) == key(&b));
        assert!(key(&a) != key(&c));
        let mut hasher_a = std::hash::DefaultHasher::new();
        let mut hasher_b = std::hash::DefaultHasher::new();
        std::hash::Hash::hash(&key(&a), &mut hasher_a);
        std::hash::Hash::hash(&key(&b), &mut hasher_b);
        assert_eq!(
            std::hash::Hasher::finish(&hasher_a),
            std::hash::Hasher::finish(&hasher_b)
        );
    }

    #[test]
    fn every_table_test_finds_nil_and_t_keys() {
        // `nil` and `t` are fresh objects on each read but one value
        // each, so they must be found under `eq`, `eql` and `equal`.
        let mut ctx = TulispContext::new();
        for test in ["'eq", "'eql", "'equal"] {
            eval_assert_equal(
                &mut ctx,
                &format!(
                    "(let ((h (make-hash-table :test {test}))) (puthash nil 1 h) (puthash t 2 h) (list (gethash nil h 'missing) (gethash t h 'missing)))"
                ),
                "'(1 2)",
            );
        }
    }
}
