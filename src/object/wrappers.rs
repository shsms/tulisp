use crate::{Error, TulispContext, TulispObject};

#[cfg(not(feature = "sync_send"))]
pub(crate) mod not_sync_send {
    pub(crate) type Shared<T> = std::rc::Rc<T>;
    pub(crate) trait SyncSend {}
}

#[cfg(feature = "sync_send")]
pub(crate) mod sync_send {
    pub(crate) type Shared<T> = std::sync::Arc<T>;
    pub(crate) trait SyncSend: Send + Sync {}
}
