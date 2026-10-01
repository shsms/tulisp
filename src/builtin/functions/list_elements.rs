use crate::{Error, TulispContext, TulispObject, lists};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "nth",
        |n: i64, list: TulispObject| -> Result<TulispObject, Error> { lists::nth(n, list) },
    );

    ctx.defun(
        "nthcdr",
        |n: i64, list: TulispObject| -> Result<TulispObject, Error> { lists::nthcdr(n, list) },
    );

    ctx.defun(
        "last",
        |list: TulispObject, n: Option<i64>| -> Result<TulispObject, Error> {
            lists::last(&list, n)
        },
    );

    ctx.defun(
        "setcar",
        |cell: TulispObject, val: TulispObject| -> Result<TulispObject, Error> {
            cell.set_car(val.clone())?;
            Ok(val)
        },
    );

    ctx.defun(
        "setcdr",
        |cell: TulispObject, val: TulispObject| -> Result<TulispObject, Error> {
            cell.set_cdr(val.clone())?;
            Ok(val)
        },
    );

    macro_rules! impl_all_cxr {
        ($name:ident) => {
            ctx.defun(
                stringify!($name),
                |obj: TulispObject| -> Result<TulispObject, Error> { obj.$name() },
            );
        };
        ($name:ident, $($rest:ident),*) => {
            impl_all_cxr!($name);
            impl_all_cxr!($($rest),*);
        };
    }

    impl_all_cxr!(
        car, cdr, caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr,
        caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar, cdaadr, cdadar,
        cdaddr, cddaar, cddadr, cdddar, cddddr
    );
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::eval_assert_equal;

    #[test]
    fn nth_and_nthcdr_index_a_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((items '(4 20 3 22 55)))
               (list (nth 0 items) (nth 2 items) (nth 4 items) (nth 5 items)))",
            "'(4 3 55 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((items '(4 20 3 22 55)))
               (list (nthcdr 0 items) (nthcdr 2 items) (nthcdr 4 items) (nthcdr 5 items)))",
            "'((4 20 3 22 55) (3 22 55) (55) nil)",
        );
    }
}
