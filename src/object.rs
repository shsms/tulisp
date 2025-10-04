#[cfg(not(feature = "sync_send"))]
mod rc_refcell;

#[cfg(not(feature = "sync_send"))]
pub use rc_refcell::{Span, TulispObject};

#[cfg(feature = "sync_send")]
mod arc_rwlock;

#[cfg(feature = "sync_send")]
pub use arc_rwlock::{Span, TulispObject};
