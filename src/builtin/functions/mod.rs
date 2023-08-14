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
    ($ctx:ident, $func: ident, $name: literal) => {
        $ctx.intern($name)
            .set_scope(TulispValue::Func(Rc::new($func)).into())
            .unwrap();
    };
}

mod arithmetic_operations;
mod conditionals;
mod functions;
mod list_elements;
mod sequences;

pub(crate) fn add(ctx: &mut TulispContext) {
    arithmetic_operations::add(ctx);
    conditionals::add(ctx);
    functions::add(ctx);
    sequences::add(ctx);
    list_elements::add(ctx);
}
