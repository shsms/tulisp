//! Printing objects as `prin1` does.
//!
//! The printer keeps the lists it is inside on a list of its own, instead of
//! one stack frame per level, so a list of any depth prints.

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Write};

use crate::{TulispObject, TulispValue, cons::CycleCheck};

std::thread_local! {
    /// The lists being printed on this thread, so a list that contains itself
    /// prints the repeat instead of recursing forever.
    static PRINTING: RefCell<Printing> = const {
        RefCell::new(Printing {
            lists: Vec::new(),
            deep: None,
        })
    };
}

/// The addresses of the lists being printed, outermost first.
struct Printing {
    lists: Vec<usize>,
    /// The depths of the lists past the first `SCANNED`, by address, so looking
    /// a list up stays quick however deep the printing goes.
    deep: Option<HashMap<usize, usize>>,
}

impl Printing {
    /// How many of the outermost lists `depth_of` looks through in order before
    /// it looks in `deep`, as a scan is quicker than a lookup for a few lists.
    const SCANNED: usize = 32;

    /// The depth of the list at ADDR among the lists being printed.
    fn depth_of(&self, addr: usize) -> Option<usize> {
        self.lists
            .iter()
            .take(Self::SCANNED)
            .position(|a| *a == addr)
            .or_else(|| self.deep.as_ref()?.get(&addr).copied())
    }

    fn push(&mut self, addr: usize) {
        if self.lists.len() >= Self::SCANNED {
            self.deep
                .get_or_insert_default()
                .insert(addr, self.lists.len());
        }
        self.lists.push(addr);
    }

    fn pop(&mut self) {
        if let Some(addr) = self.lists.pop()
            && self.lists.len() >= Self::SCANNED
            && let Some(deep) = &mut self.deep
        {
            deep.remove(&addr);
        }
    }
}

/// A list the printer has printed the opening parenthesis of.
struct OpenList {
    /// The cell to print the car of next, or the tail.
    rest: TulispObject,
    cycle: CycleCheck,
    /// Whether an element has been printed.
    started: bool,
    /// Whether the cdrs have looped back; the list then ends in ` ...`.
    circular: bool,
    /// Whether all but the closing parenthesis has been printed.
    closing: bool,
    /// Whether the list printed as `#'X`, so it closes without a parenthesis.
    function_form: bool,
}

/// The lists being printed by one `print`, innermost last. Each is on
/// `PRINTING` too, but for an outermost one that is not counted, and leaves it
/// when this does, on every path.
struct OpenLists {
    lists: Vec<OpenList>,
    /// Whether the outermost list counts among the lists being printed.
    outer_counted: bool,
}

impl OpenLists {
    /// Whether a list opened now, or the one just closed, is on `PRINTING`:
    /// all but an outermost one that is not counted are.
    fn top_counted(&self) -> bool {
        self.outer_counted || !self.lists.is_empty()
    }
}

impl Drop for OpenLists {
    fn drop(&mut self) {
        let counted = self
            .lists
            .len()
            .saturating_sub(usize::from(!self.outer_counted));
        PRINTING.with(|printing| {
            let mut printing = printing.borrow_mut();
            for _ in 0..counted {
                printing.pop();
            }
        });
    }
}

/// Prints OBJ to F as `prin1` does. A list met again inside its own printing
/// prints as `#N`, N its depth among the lists being printed, as Emacs does. A
/// list whose cdrs loop back ends in ` ...` after a few rounds of the loop,
/// where the walk notices it.
pub(super) fn print(obj: &TulispObject, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    print_lists(obj, f, true)
}

/// Prints OBJ, a list copied out of a value only to print it, as `print`
/// does, but leaves OBJ off the lists being printed, as no list inside it can
/// be the copy.
pub(crate) fn print_copy(obj: &TulispObject, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    print_lists(obj, f, false)
}

/// `print`, which counts OBJ among the lists being printed when OUTER_COUNTED.
fn print_lists(obj: &TulispObject, f: &mut fmt::Formatter<'_>, outer_counted: bool) -> fmt::Result {
    let mut open = OpenLists {
        lists: Vec::new(),
        outer_counted,
    };
    let mut next = Some(obj.clone());
    loop {
        if let Some(obj) = next.take() {
            next = print_one(&obj, f, &mut open)?;
            continue;
        }
        let Some(list) = open.lists.last_mut() else {
            return Ok(());
        };
        if list.closing {
            if !list.function_form {
                f.write_char(')')?;
            }
            open.lists.pop();
            if open.top_counted() {
                PRINTING.with(|printing| printing.borrow_mut().pop());
            }
            continue;
        }
        let cell = {
            let inner = list.rest.inner_ref();
            match &inner.0 {
                TulispValue::List { cons } => Some((cons.car().clone(), cons.cdr().clone())),
                _ => None,
            }
        };
        let Some((car, cdr)) = cell else {
            list.closing = true;
            if list.circular {
                f.write_str(" ...")?;
            } else if !list.rest.null() {
                f.write_str(" . ")?;
                next = Some(list.rest.clone());
            }
            continue;
        };
        if list.started {
            f.write_char(' ')?;
        }
        list.started = true;
        if list.cycle.step(&cdr).is_err() {
            list.circular = true;
            list.rest = TulispObject::nil();
        } else {
            list.rest = cdr;
        }
        next = Some(car);
    }
}

/// X when OBJ is the two-element list `(function X)`.
pub(crate) fn function_form_arg(obj: &TulispObject) -> Option<TulispObject> {
    let head = obj.car().ok()?;
    if head.inner_ref().0.symbol_name() != Some("function") {
        return None;
    }
    let rest = obj.cdr().ok()?;
    if !rest.consp() || !rest.cdr().ok()?.null() {
        return None;
    }
    rest.car().ok()
}

/// Prints the start of OBJ: all of an atom, the prefix of a quote form, whose
/// quoted form it returns to print next, and the opening of a list, which it
/// adds to OPEN; for a `(function X)` list the opening is `#'`, and it returns
/// X to print next.
fn print_one(
    obj: &TulispObject,
    f: &mut fmt::Formatter<'_>,
    open: &mut OpenLists,
) -> Result<Option<TulispObject>, fmt::Error> {
    if obj.consp() {
        let addr = obj.addr_as_usize();
        if let Some(depth) = PRINTING.with(|printing| printing.borrow().depth_of(addr)) {
            write!(f, "#{depth}")?;
            return Ok(None);
        }
        // A tail call `mark_tail_calls` marked, `(Bounce f args...)`, prints as
        // the call, as it was written.
        let mut list = obj.clone();
        while let Ok(head) = list.car()
            && head.is_bounce()
            && let Ok(call) = list.cdr()
        {
            list = call;
        }
        if open.top_counted() {
            PRINTING.with(|printing| printing.borrow_mut().push(addr));
        }
        // `(function X)` prints as `#'X`, as in Emacs, but stays among the
        // lists being printed until X is printed, in case X leads back to it.
        let arg = function_form_arg(obj);
        let function_form = arg.is_some();
        f.write_str(if function_form { "#'" } else { "(" })?;
        open.lists.push(OpenList {
            rest: list,
            cycle: CycleCheck::new(),
            started: false,
            circular: false,
            closing: function_form,
            function_form,
        });
        return Ok(arg);
    }
    // A quote form prints its value after letting go of the lock, as a list
    // does, in case the value leads back here.
    let inner = obj.inner_ref();
    let (prefix, value) = match &inner.0 {
        TulispValue::Quote { value, .. } => ("'", value.clone()),
        TulispValue::Backquote { value, .. } => ("`", value.clone()),
        TulispValue::Unquote { value, .. } => (",", value.clone()),
        TulispValue::Splice { value, .. } => (",@", value.clone()),
        other => {
            write!(f, "{other}")?;
            return Ok(None);
        }
    };
    drop(inner);
    f.write_str(prefix)?;
    Ok(Some(value))
}

#[cfg(test)]
mod tests {
    use crate::test_utils::eval_assert_equal;
    use crate::{Error, TulispContext, TulispObject};

    /// Deeper than any thread's stack could print one level per frame.
    const DEPTH: usize = 1_000_000;

    #[test]
    fn a_list_nested_a_million_deep_prints() {
        let mut list = TulispObject::from(1);
        for _ in 0..DEPTH {
            list = TulispObject::cons(list, TulispObject::nil());
        }
        let expected = "(".repeat(DEPTH) + "1" + &")".repeat(DEPTH);
        assert!(list.to_string() == expected);
    }

    #[test]
    fn a_list_nested_a_million_deep_in_cars_and_cdrs_prints() {
        let mut list = TulispObject::nil();
        for _ in 0..DEPTH {
            let tail = TulispObject::cons(2.into(), TulispObject::nil());
            list = TulispObject::cons(list, tail);
        }
        let expected = "(".repeat(DEPTH) + "nil" + &" 2)".repeat(DEPTH);
        assert!(list.to_string() == expected);
    }

    #[test]
    fn nested_lists_print_their_elements_tails_and_quotes() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(format "%S" '(1 (2 "s" (3 . 4)) 'a `(b ,c ,@d) #'e (f . 'g)))"#,
            r#""(1 (2 \"s\" (3 . 4)) 'a `(b ,c ,@d) 'e (f . 'g))""#,
        );
        Ok(())
    }

    #[test]
    fn a_list_inside_itself_prints_as_its_depth() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(setq a (list 1 (list 2 nil))) (setcar (cdr (cadr a)) a)
             (setq b (list 1 2)) (setcar (cdr b) (list 3 b))",
        )?;
        eval_assert_equal(ctx, r#"(format "%S" a)"#, r#""(1 (2 #0))""#);
        eval_assert_equal(ctx, r#"(format "%S" b)"#, r#""(1 (3 #0))""#);
        eval_assert_equal(ctx, r#"(format "%S" (list b))"#, r#""((1 (3 #1)))""#);
        Ok(())
    }

    // A `#'` form that leads back to itself prints the repeat, as a list does.
    #[test]
    fn a_function_form_inside_itself_prints_as_its_depth() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(setq a (list 'function nil)) (setcar (cdr a) a)
             (setq b (list 'function nil)) (setcar (cdr b) (list 'function b))
             (setq c (list 1 nil)) (setcar (cdr c) (list 'function (list 2 c)))",
        )?;
        eval_assert_equal(ctx, r#"(format "%S" a)"#, r##""#'#0""##);
        eval_assert_equal(ctx, r#"(format "%S" b)"#, r##""#'#'#0""##);
        eval_assert_equal(ctx, r#"(format "%S" c)"#, r#""(1 #'(2 #0))""#);
        eval_assert_equal(
            ctx,
            r#"(format "%S" '(1 (function (function f)) (function (a b))))"#,
            r#""(1 #'#'f #'(a b))""#,
        );
        // Only a list of `function` and one argument prints as `#'`.
        eval_assert_equal(
            ctx,
            r#"(format "%S" '((function a b) (function)))"#,
            r#""((function a b) (function))""#,
        );
        Ok(())
    }

    #[test]
    fn a_list_a_million_deep_inside_itself_prints_as_its_depth() {
        let inner = TulispObject::cons(1.into(), TulispObject::nil());
        let mut list = inner.clone();
        for _ in 0..DEPTH {
            list = TulispObject::cons(list, TulispObject::nil());
        }
        inner
            .set_cdr(TulispObject::cons(list.clone(), TulispObject::nil()))
            .unwrap();
        let printed = list.to_string();
        assert!(
            printed.contains("(1 #0)"),
            "{}",
            &printed[DEPTH - 10..DEPTH + 20]
        );
    }

    #[test]
    fn a_value_prints_the_lists_inside_it_as_the_object_does() {
        // A value prints through a copy of its list, which counts for no
        // depth.
        let a = TulispObject::cons(1.into(), TulispObject::nil());
        a.set_cdr(TulispObject::cons(a.clone(), TulispObject::nil()))
            .unwrap();
        assert_eq!(a.to_string(), "(1 #0)");
        assert_eq!(a.clone_inner().to_string(), "(1 (1 #0))");
    }

    #[test]
    fn a_list_deeper_than_scanned_prints_again_after_it_closes() {
        let mut list = TulispObject::from(1);
        for _ in 0..40 {
            list = TulispObject::cons(list, TulispObject::nil());
        }
        let once = list.to_string();
        let twice = TulispObject::cons(list.clone(), TulispObject::cons(list, TulispObject::nil()));
        assert_eq!(twice.to_string(), format!("({once} {once})"));
    }
}
