use crate::{
    Error, TulispContext, TulispObject,
    eval::{DummyEval, funcall},
    list, lists,
};
use std::cmp::Ordering;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("length", |list: TulispObject| {
        lists::length(&list).map(TulispObject::from)
    });

    ctx.defun("reverse", |list: TulispObject| {
        Ok(list
            .base_iter()
            .fold(TulispObject::nil(), |acc, item| TulispObject::cons(item, acc)))
    });

    ctx.defun(
        "seq-map",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject| ctx.map(&func, &seq),
    );

    ctx.defun(
        "seq-reduce",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject, initial: TulispObject| {
            ctx.reduce(&func, &seq, &initial)
        },
    );

    ctx.defun(
        "seq-filter",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject| ctx.filter(&func, &seq),
    );

    ctx.defun(
        "seq-find",
        |ctx: &mut TulispContext,
         func: TulispObject,
         seq: TulispObject,
         default: Option<TulispObject>| {
            let func = ctx.eval(&func)?;
            for item in seq.base_iter() {
                if funcall::<DummyEval>(ctx, &func, &list!(item.clone())?)?.is_truthy() {
                    return Ok(item);
                }
            }
            Ok(default.unwrap_or_else(TulispObject::nil))
        },
    );

    ctx.defun(
        "mapconcat",
        |ctx: &mut TulispContext,
         func: TulispObject,
         seq: TulispObject,
         sep: Option<String>| {
            let mapped = ctx.map(&func, &seq)?;
            let sep = sep.unwrap_or_default();
            let mut out = String::new();
            let mut first = true;
            for item in mapped.base_iter() {
                if !first {
                    out.push_str(&sep);
                }
                first = false;
                out.push_str(&item.as_string()?);
            }
            Ok(TulispObject::from(out))
        },
    );

    ctx.defun(
        "string-join",
        |strings: TulispObject, sep: Option<String>| {
            let sep = sep.unwrap_or_default();
            let mut out = String::new();
            let mut first = true;
            for item in strings.base_iter() {
                if !first {
                    out.push_str(&sep);
                }
                first = false;
                out.push_str(&item.as_string()?);
            }
            Ok(TulispObject::from(out))
        },
    );

    ctx.defun("seq-take", |seq: TulispObject, n: i64| {
        let ret = TulispObject::nil();
        if n <= 0 {
            return Ok(ret);
        }
        for item in seq.base_iter().take(n as usize) {
            ret.push(item)?;
        }
        Ok(ret)
    });

    ctx.defun("seq-drop", |seq: TulispObject, n: i64| {
        let ret = TulispObject::nil();
        let mut skipped = 0i64;
        for item in seq.base_iter() {
            if skipped < n {
                skipped += 1;
                continue;
            }
            ret.push(item)?;
        }
        Ok(ret)
    });

    ctx.defun("make-string", |n: i64, ch: i64| {
        if n < 0 {
            return Err(Error::out_of_range(format!(
                "make-string: negative length {}",
                n
            )));
        }
        let Some(c) = u32::try_from(ch).ok().and_then(char::from_u32) else {
            return Err(Error::out_of_range(format!(
                "make-string: invalid character code {}",
                ch
            )));
        };
        let mut out = String::with_capacity(n as usize);
        for _ in 0..n {
            out.push(c);
        }
        Ok(TulispObject::from(out))
    });

    ctx.defun("memq", |elt: TulispObject, list: TulispObject| {
        let mut cur = list;
        while cur.consp() {
            if cur.car_and_then(|car| Ok(car.eq(&elt)))? {
                return Ok(cur);
            }
            cur = cur.cdr()?;
        }
        Ok(TulispObject::nil())
    });

    ctx.defun("memql", |elt: TulispObject, list: TulispObject| {
        let mut cur = list;
        while cur.consp() {
            if cur.car_and_then(|car| Ok(car.eql(&elt)))? {
                return Ok(cur);
            }
            cur = cur.cdr()?;
        }
        Ok(TulispObject::nil())
    });

    ctx.defun("member", |elt: TulispObject, list: TulispObject| {
        let mut cur = list;
        while cur.consp() {
            if cur.car_and_then(|car| Ok(car.equal(&elt)))? {
                return Ok(cur);
            }
            cur = cur.cdr()?;
        }
        Ok(TulispObject::nil())
    });

    ctx.defun(
        "sort",
        |ctx: &mut TulispContext, seq: TulispObject, pred: TulispObject| {
            let pred = ctx.eval(&pred)?;
            let mut vec: Vec<_> = seq.base_iter().collect();
            let mut err = None;
            vec.sort_by(|v1, v2| {
                if funcall::<DummyEval>(ctx, &pred, &list!(v1.clone(), v2.clone()).unwrap())
                    .map(|v| v.null())
                    .unwrap_or_else(|x| {
                        err = Some(x);
                        false
                    })
                {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            });
            if let Some(err) = err {
                return Err(err);
            }
            let ret = vec.iter().fold(TulispObject::nil(), |v1, v2| {
                TulispObject::cons(v2.clone(), v1)
            });
            Ok(ret)
        },
    );
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, test_utils::eval_assert_equal};

    #[test]
    fn test_mapconcat() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(mapconcat (lambda (s) (concat "<" s ">")) '("a" "b" "c") "-")"#,
            r#""<a>-<b>-<c>""#,
        );
        eval_assert_equal(ctx, r#"(mapconcat (lambda (s) s) '("x") ",")"#, r#""x""#);
        eval_assert_equal(ctx, r#"(mapconcat (lambda (s) s) '() ",")"#, r#""""#);
    }

    #[test]
    fn test_string_join() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(string-join '("a" "b" "c") "-")"#, r#""a-b-c""#);
        eval_assert_equal(ctx, r#"(string-join '("a" "b"))"#, r#""ab""#);
        eval_assert_equal(ctx, r#"(string-join '())"#, r#""""#);
    }

    #[test]
    fn test_seq_take() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(seq-take '(1 2 3 4 5) 3)", "'(1 2 3)");
        eval_assert_equal(ctx, "(seq-take '(1 2) 5)", "'(1 2)");
        eval_assert_equal(ctx, "(seq-take '(1 2 3) 0)", "nil");
        eval_assert_equal(ctx, "(seq-take '() 3)", "nil");
    }

    #[test]
    fn test_seq_drop() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(seq-drop '(1 2 3 4 5) 2)", "'(3 4 5)");
        eval_assert_equal(ctx, "(seq-drop '(1 2) 5)", "nil");
        eval_assert_equal(ctx, "(seq-drop '(1 2 3) 0)", "'(1 2 3)");
    }

    #[test]
    fn test_make_string() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(make-string 5 65)", r#""AAAAA""#);
        eval_assert_equal(ctx, "(make-string 0 65)", r#""""#);
        eval_assert_equal(ctx, "(make-string 3 32)", r#""   ""#);
    }

    #[test]
    fn test_length_string() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(length "abc")"#, "3");
        eval_assert_equal(ctx, r#"(length "")"#, "0");
        eval_assert_equal(ctx, "(length '(1 2 3 4))", "4");
    }

    #[test]
    fn test_memql() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(memql 3 '(1 2 3 4 5))", "'(3 4 5)");
        eval_assert_equal(ctx, "(memql 9 '(1 2 3))", "nil");
        eval_assert_equal(ctx, "(memql 1 '())", "nil");
        eval_assert_equal(ctx, "(memql 'a '(a b c))", "'(a b c)");
    }

    #[test]
    fn test_memq() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(memq 'b '(a b c))", "'(b c)");
        eval_assert_equal(ctx, "(memq 'z '(a b c))", "nil");
    }

    #[test]
    fn test_member() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(member "b" '("a" "b" "c"))"#,
            r#"'("b" "c")"#,
        );
        eval_assert_equal(ctx, r#"(member "z" '("a" "b"))"#, "nil");
    }

    #[test]
    fn test_reverse() {
        let ctx = &mut TulispContext::new();

        eval_assert_equal(ctx, "(reverse '())", "nil");
        eval_assert_equal(ctx, "(reverse '(1))", "'(1)");
        eval_assert_equal(ctx, "(reverse '(1 2 3))", "'(3 2 1)");
        eval_assert_equal(ctx, r#"(reverse '("a" "b" "c"))"#, r#"'("c" "b" "a")"#);
    }
}
