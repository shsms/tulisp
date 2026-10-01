//! Freeing values nested deeper than the stack would allow.
//!
//! Dropping a value drops the values it holds, each one level further down the
//! stack. A list a million cars deep, or a million closures that each captured
//! the one before, would overflow it. The drops that would recurse hand the
//! inner value to [`release`] instead, which queues it while a drop further up
//! this thread's stack frees the queue one value at a time.

use std::cell::{Cell, RefCell};
use std::mem::ManuallyDrop;

use crate::{TulispObject, TulispValue};

std::thread_local! {
    /// How many `free`s running on this thread drop their values directly,
    /// each inside the one before.
    static DEPTH: Cell<u32> = const { Cell::new(0) };

    /// The values waiting to be freed, while a `free` further up this thread's
    /// stack frees them; `None` when none is running. It has no destructor, so
    /// it is there for the thread-locals torn down as the thread ends, which
    /// may hold values to free; it is empty whenever no `free` is running.
    static PENDING: RefCell<ManuallyDrop<Option<Vec<TulispValue>>>> =
        const { RefCell::new(ManuallyDrop::new(None)) };
}

/// The `free`s that drop their values directly, nested in each other, before
/// the next one queues its value instead.
const DIRECT_DEPTH: u32 = 64;

/// Frees the value of OBJ through `free`, when OBJ is its last reference and
/// the value can hold other values. Otherwise dropping OBJ frees nothing that
/// could recurse.
#[inline]
pub(crate) fn release(obj: &mut TulispObject) {
    let Some(inner) = obj.rc.get_mut() else {
        return;
    };
    if matches!(
        inner.0,
        TulispValue::Nil | TulispValue::T | TulispValue::Number { .. } | TulispValue::String { .. }
    ) {
        return;
    }
    free(inner.0.take());
}

/// Frees VALUE: directly while fewer than `DIRECT_DEPTH` `free`s are nested,
/// and otherwise through the queue, which a `free` further up empties, or this
/// one when none is.
#[inline(never)]
fn free(value: TulispValue) {
    let depth = DEPTH.get();
    if depth < DIRECT_DEPTH {
        DEPTH.set(depth + 1);
        let _depth = DepthGuard(depth);
        drop(value);
        return;
    }
    let started = PENDING.try_with(|pending| {
        let mut pending = pending.borrow_mut();
        match pending.as_mut() {
            Some(queue) => {
                queue.push(value);
                None
            }
            None => {
                **pending = Some(Vec::new());
                Some(value)
            }
        }
    });
    // `Ok(None)`: VALUE is queued. `PENDING` has no destructor, so `try_with`
    // always succeeds; if it failed, VALUE would drop with the closure, as it
    // would without `release`.
    let Ok(Some(value)) = started else {
        return;
    };
    let _pending = PendingGuard;
    drop(value);
    while let Some(next) = PENDING.with(|pending| pending.borrow_mut().as_mut().and_then(Vec::pop))
    {
        drop(next);
    }
}

/// Puts `DEPTH` back to what it was when its `free` returns or unwinds.
struct DepthGuard(u32);

impl Drop for DepthGuard {
    fn drop(&mut self) {
        DEPTH.set(self.0);
    }
}

/// Empties `PENDING` when the `free` that started the queue returns or
/// unwinds; a drop that panicked leaves values in the queue, and they drop
/// here.
struct PendingGuard;

impl Drop for PendingGuard {
    fn drop(&mut self) {
        let left = PENDING.try_with(|pending| pending.borrow_mut().take());
        drop(left);
    }
}

#[cfg(test)]
mod tests {
    use crate::{Error, TulispContext, TulispObject};

    /// Deeper than any thread's stack could free one level per frame.
    const DEPTH: usize = 1_000_000;

    #[test]
    fn a_list_nested_in_its_cars_frees() {
        let mut list = TulispObject::nil();
        for _ in 0..DEPTH {
            list = TulispObject::cons(list, TulispObject::nil());
        }
        drop(list);
    }

    #[test]
    fn a_drop_that_panics_leaves_the_depth_as_it_was() {
        struct Panics;
        impl Drop for Panics {
            fn drop(&mut self) {
                panic!("a host value that panics when dropped");
            }
        }
        impl std::fmt::Display for Panics {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("panics")
            }
        }
        impl crate::TulispAny for Panics {}
        let host: TulispObject = crate::Shared::new(Panics).into();
        let inner = TulispObject::cons(host, TulispObject::nil());
        let list = TulispObject::cons(inner, TulispObject::nil());
        let dropped = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| drop(list)));
        assert!(dropped.is_err());
        assert_eq!(super::DEPTH.get(), 0);
    }

    #[test]
    fn a_list_nested_in_cars_and_cdrs_frees() {
        let mut list = TulispObject::nil();
        for i in 0..DEPTH as i64 {
            let tail = TulispObject::cons(i.into(), TulispObject::nil());
            list = TulispObject::cons(list, tail);
        }
        drop(list);
    }

    #[test]
    fn a_list_nested_in_its_cars_frees_as_its_thread_ends() {
        // A thread-local that a host keeps its values in, set before the queue
        // is first used, is torn down after the queue.
        std::thread_local! {
            static KEPT: std::cell::RefCell<Option<TulispObject>> =
                const { std::cell::RefCell::new(None) };
        }
        std::thread::spawn(|| {
            let mut list = TulispObject::nil();
            for _ in 0..DEPTH {
                list = TulispObject::cons(list, TulispObject::nil());
            }
            KEPT.with(|kept| *kept.borrow_mut() = Some(list));
            drop(TulispObject::cons(
                TulispObject::cons(1.into(), TulispObject::nil()),
                TulispObject::nil(),
            ));
        })
        .join()
        .expect("the thread ends");
    }

    #[test]
    fn a_chain_of_closures_frees() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(let ((f nil))
               (dotimes (i 100000)
                 (let ((g f)) (setq f (lambda () g)))))",
        )?;
        Ok(())
    }

    #[test]
    fn closures_in_lists_in_closures_free() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(let ((f nil))
               (dotimes (i 100000)
                 (let ((g (list f))) (setq f (lambda () g)))))",
        )?;
        Ok(())
    }

    #[test]
    fn a_list_whose_car_is_its_cdr_frees() {
        let mut list = TulispObject::nil();
        for _ in 0..DEPTH {
            list = TulispObject::cons(list.clone(), list);
        }
        drop(list);
    }

    #[test]
    fn a_chain_of_hash_tables_frees() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(let ((h nil))
               (dotimes (i 100000)
                 (let ((n (make-hash-table))) (puthash 1 h n) (puthash h 1 n) (setq h n))))",
        )?;
        Ok(())
    }

    #[test]
    fn a_chain_of_uninterned_symbols_frees() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(let ((s nil))
               (dotimes (i 100000)
                 (let ((n (make-symbol \"x\"))) (set n s) (setq s n))))",
        )?;
        Ok(())
    }
}
