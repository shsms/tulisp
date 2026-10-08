//! Copying lists and strings.

use crate::{Error, TulispObject, cons::ListBuilder, lists};

impl TulispObject {
    /// A copy of `self`, a list or a string, as Emacs's `copy-sequence` makes
    /// it: a new list holding the same elements, or a new string with the same
    /// text. A value that is neither, a list that ends in a non-list, and one
    /// whose cdrs loop back are errors.
    ///
    /// ```rust
    /// # use tulisp::TulispContext;
    /// let mut ctx = TulispContext::new();
    /// let list = ctx.eval_string("(list 1 (list 2))").unwrap();
    /// let copy = list.copy_sequence().unwrap();
    /// assert!(copy.equal(&list) && !copy.eq(&list));
    /// // The elements are shared.
    /// assert!(copy.cadr().unwrap().eq(&list.cadr().unwrap()));
    /// ```
    pub fn copy_sequence(&self) -> Result<TulispObject, Error> {
        if self.stringp() {
            return self.with_str(|text| TulispObject::from(text));
        }
        if !self.listp() {
            return Err(lists::not_a_sequence(self));
        }
        let mut copy = ListBuilder::new();
        copy.push_all(self)?;
        Ok(copy.build())
    }
}
