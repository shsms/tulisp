use crate::TulispContext;

// These macros are used by multiple submodules, so they are defined here.

macro_rules! max_min_ops {
    ($oper:tt) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<TulispObject, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok(std::cmp::$oper(s, o).into())
            }
        }
    }};
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<TulispObject, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            }
        }
    }};
}

macro_rules! intern_set_func {
    ($ctx:ident, $func: ident, $name: expr) => {
        #[cfg(not(feature = "sync_send"))]
        $ctx.intern($name)
            .set_global(TulispValue::Func(std::rc::Rc::new($func)).into_ref(None))
            .unwrap();

        #[cfg(feature = "sync_send")]
        $ctx.intern($name)
            .set_global(TulispValue::Func(std::sync::Arc::new($func)).into_ref(None))
            .unwrap();
    };
    ($ctx:ident, $func: ident) => {
        intern_set_func!($ctx, $func, stringify!($func));
    };
}

pub(crate) mod common;
mod comparison_of_strings;
mod conditionals;
mod equality_predicates;
mod functions;
mod hash_table;
mod list_elements;
mod numbers;
mod sequences;
mod time_operations;

pub(crate) fn add(ctx: &mut TulispContext) {
    conditionals::add(ctx);
    equality_predicates::add(ctx);
    functions::add(ctx);
    hash_table::add(ctx);
    list_elements::add(ctx);
    sequences::add(ctx);
    numbers::add(ctx);
    comparison_of_strings::add(ctx);
    time_operations::add(ctx);
}
