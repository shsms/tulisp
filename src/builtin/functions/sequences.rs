use crate::{Error, TulispContext, TulispObject, TulispValue, lists};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("length", |list: TulispObject| {
        lists::length(&list).map(TulispObject::from)
    });

    ctx.defun("reverse", |list: TulispObject| {
        let mut iter = list.base_iter();
        let result = iter.by_ref().fold(TulispObject::nil(), |acc, item| {
            TulispObject::cons(item, acc)
        });
        iter.take_error()?;
        Ok(result)
    });

    ctx.defun(
        "string-join",
        |strings: TulispObject, sep: Option<String>| {
            let sep = sep.unwrap_or_default();
            let mut out = String::new();
            let mut first = true;
            let mut iter = strings.base_iter();
            for item in iter.by_ref() {
                if !first {
                    out.push_str(&sep);
                }
                first = false;
                out.push_str(&item.as_string()?);
            }
            iter.take_error()?;
            Ok(TulispObject::from(out))
        },
    );

    ctx.defun("seq-take", |seq: TulispObject, n: i64| {
        let ret = TulispObject::nil();
        if n <= 0 {
            return Ok(ret);
        }
        let mut iter = seq.base_iter();
        for item in iter.by_ref().take(n as usize) {
            ret.push(item)?;
        }
        iter.take_error()?;
        Ok(ret)
    });

    ctx.defun("seq-drop", |seq: TulispObject, n: i64| {
        let ret = TulispObject::nil();
        let mut skipped = 0i64;
        let mut iter = seq.base_iter();
        for item in iter.by_ref() {
            if skipped < n {
                skipped += 1;
                continue;
            }
            ret.push(item)?;
        }
        iter.take_error()?;
        Ok(ret)
    });

    // `(aset STRING INDEX CHAR)` mutates the character at INDEX in
    // STRING and returns CHAR. Tulisp doesn't have vectors yet, so
    // this is string-only — Emacs additionally supports vectors and
    // bool-vectors. CHAR is an integer code point (Tulisp has no
    // character literals); UTF-8 string layout is handled by
    // collecting to `Vec<char>` and reassembling.
    ctx.defun(
        "aset",
        |s: TulispObject, idx: i64, ch: i64| -> Result<TulispObject, Error> {
            let s_str = s.as_string()?;
            let new_char = u32::try_from(ch)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| {
                    Error::out_of_range(format!("aset: invalid character code: {}", ch))
                })?;
            let idx_usize = usize::try_from(idx)
                .map_err(|_| Error::out_of_range(format!("aset: negative index: {}", idx)))?;
            let mut chars: Vec<char> = s_str.chars().collect();
            if idx_usize >= chars.len() {
                return Err(Error::out_of_range(format!(
                    "aset: index {} out of range for string of length {}",
                    idx,
                    chars.len()
                )));
            }
            chars[idx_usize] = new_char;
            let new_string: String = chars.into_iter().collect();
            s.assign(TulispValue::String { value: new_string });
            Ok(TulispObject::from(ch))
        },
    );

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
        // `String::with_capacity(n)` (the previous form) would OOM
        // and abort the process for huge `n`. Compute the actual
        // byte cost (chars can be multi-byte UTF-8) and use
        // `try_reserve_exact` so a request the system can't satisfy
        // comes back as a Lisp `OutOfRange` error.
        let n_usize = n as usize;
        let bytes_needed = n_usize.checked_mul(c.len_utf8()).ok_or_else(|| {
            Error::out_of_range(format!("make-string: length {} overflows usize", n))
        })?;
        let mut out = String::new();
        out.try_reserve_exact(bytes_needed).map_err(|_| {
            Error::out_of_range(format!(
                "make-string: cannot allocate {n} chars ({bytes_needed} bytes)"
            ))
        })?;
        for _ in 0..n {
            out.push(c);
        }
        Ok(TulispObject::from(out))
    });

    fn member_with(
        list: TulispObject,
        elt: &TulispObject,
        eq: impl Fn(&TulispObject, &TulispObject) -> bool,
    ) -> Result<TulispObject, Error> {
        let mut cur = list;
        while cur.consp() {
            if cur.car_and_then(|car| Ok(eq(car, elt)))? {
                return Ok(cur);
            }
            cur = cur.cdr()?;
        }
        // `cur` is non-cons: either nil (clean end) or an
        // improper-list tail. Reject the latter the way Emacs does.
        if !cur.null() {
            return Err(Error::type_mismatch(format!("expected list, got: {cur}")));
        }
        Ok(TulispObject::nil())
    }

    ctx.defun("memq", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| a.eq(b))
    });

    ctx.defun("memql", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| a.eql(b))
    });

    ctx.defun("member", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| a.equal(b))
    });
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
        eval_assert_equal(ctx, r#"(member "b" '("a" "b" "c"))"#, r#"'("b" "c")"#);
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
