//! Raising and defining error symbols from Rust.

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

    /// Adds the error symbol NAME with MESSAGE under PARENTS, as `define-error`
    /// does with a list of parents; no PARENTS means `error`. A handler for
    /// NAME or for any of its ancestors catches it. Each parent must be an
    /// error symbol already, so define a parent before its children.
    /// Redefining an error defined this way replaces it; the error symbols
    /// every context starts with, such as `error` and `quit`, cannot be
    /// redefined.
    pub fn define_error(
        &mut self,
        name: &str,
        message: &str,
        parents: &[&str],
    ) -> Result<(), Error> {
        self.check_error_parents(parents)?;
        self.define_error_any_parent(name, Some(message), parents)
    }

    /// Refuses a parent that is not an error symbol, as `define-error` does for
    /// a list of parents.
    pub(crate) fn check_error_parents(&self, parents: &[&str]) -> Result<(), Error> {
        match parents.iter().find(|p| !self.error_table.is_defined(p)) {
            Some(unknown) => Err(Error::lisp_error(format!(
                "Unknown signal \u{2018}{unknown}\u{2019}"
            ))),
            None => Ok(()),
        }
    }

    /// Like `define_error`, but a parent that is not an error symbol counts as
    /// just itself, as a lone PARENT of Lisp's `define-error` does, and the
    /// MESSAGE may be missing, as a nil one is there.
    pub(crate) fn define_error_any_parent(
        &mut self,
        name: &str,
        message: Option<&str>,
        parents: &[&str],
    ) -> Result<(), Error> {
        if crate::error::ErrorTable::is_built_in(name) {
            return Err(Error::lisp_error(format!(
                "Can't redefine a built-in error: {name}"
            )));
        }
        let parents = if parents.is_empty() {
            &["error"][..]
        } else {
            parents
        };
        self.error_table.define(name, message, parents);
        Ok(())
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
