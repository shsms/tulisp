//! Raising error symbols from Rust.

use crate::{Error, TulispContext, TulispObject};

impl TulispContext {
    /// The error `(signal NAME DATA)` raises. A `condition-case` handler for
    /// NAME, or for an error NAME is defined under, catches it and sees `(NAME
    /// . DATA)`. Its description is what Emacs's `error-message-string` gives
    /// for that.
    pub fn signal(&mut self, name: &str, data: TulispObject) -> Error {
        let desc = self.error_table.message_string(name, &data);
        Error::new_signal(self.intern(name), data, desc)
    }

    /// Like `signal`, for the symbol SYMBOL; fails when SYMBOL is not a symbol.
    pub(crate) fn signal_symbol(
        &self,
        symbol: TulispObject,
        data: TulispObject,
    ) -> Result<Error, Error> {
        let name = symbol.as_symbol()?;
        let desc = self.error_table.message_string(&name, &data);
        Ok(Error::new_signal(symbol, data, desc))
    }
}
