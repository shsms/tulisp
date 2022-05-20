#[macro_use]
extern crate pest_derive;

mod builtin;
mod cons;
pub use cons::{car, cdr, cons, BaseIter, Iter};
pub mod context;
pub mod error;
pub mod eval;
mod parser;
mod value;

pub mod macros;
mod value_ref;

pub use builtin::new_context;
pub use proc_macros::{tulisp_fn, tulisp_fn_no_eval};
pub use value_ref::TulispValueRef;
pub use value::TulispValue::Nil;
