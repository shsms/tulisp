//! Copying lists and strings.

use std::collections::HashSet;

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

    /// A copy of `self` with every cons new, in the cars as in the cdrs, as
    /// Emacs's `copy-tree` makes it. Any other value is shared, and a value
    /// that is no cons is returned as it is. A cons met again inside its own
    /// copy, through its cars or its cdrs, is an error: Emacs runs out of
    /// nesting or loops forever there.
    ///
    /// ```rust
    /// # use tulisp::TulispContext;
    /// let mut ctx = TulispContext::new();
    /// let tree = ctx.eval_string("(list 1 (list 2) \"a\")").unwrap();
    /// let copy = tree.copy_tree().unwrap();
    /// assert!(copy.equal(&tree));
    /// assert!(!copy.cadr().unwrap().eq(&tree.cadr().unwrap()));
    /// // The string is shared.
    /// assert!(copy.caddr().unwrap().eq(&tree.caddr().unwrap()));
    /// ```
    pub fn copy_tree(&self) -> Result<TulispObject, Error> {
        if !self.consp() {
            return Ok(self.clone());
        }
        let root = TulispObject::cons(TulispObject::nil(), TulispObject::nil());
        let mut copying = Copying::default();
        copying.enter(self, &root)?;
        while let Some(list) = copying.lists.last_mut() {
            if !list.car_done {
                list.car_done = true;
                let (car, new) = (list.cell.car()?, list.new.clone());
                if car.consp() {
                    let new_car = TulispObject::cons(TulispObject::nil(), TulispObject::nil());
                    new.set_car(new_car.clone())?;
                    copying.enter(&car, &new_car)?;
                } else {
                    new.set_car(car)?;
                }
                continue;
            }
            let cdr = list.cell.cdr()?;
            if !cdr.consp() {
                list.new.set_cdr(cdr)?;
                copying.leave();
                continue;
            }
            let new_cdr = TulispObject::cons(TulispObject::nil(), TulispObject::nil());
            list.new.set_cdr(new_cdr.clone())?;
            copying.step(&cdr, new_cdr)?;
        }
        Ok(root)
    }
}

/// The lists `copy_tree` is inside, innermost last.
#[derive(Default)]
struct Copying {
    lists: Vec<CopyingList>,
    /// The addresses of the conses on the path to the one being copied, in the
    /// order they were met.
    path: Vec<usize>,
    /// `path`, for looking a cons up.
    on_path: HashSet<usize>,
}

/// A list `copy_tree` is copying.
struct CopyingList {
    /// The cons being copied.
    cell: TulispObject,
    /// Its copy.
    new: TulispObject,
    /// Whether the car has been copied.
    car_done: bool,
    /// How many of the conses on `path` belong to this list.
    cells: usize,
}

impl Copying {
    /// Starts copying CELL, a cons, into NEW, a list inside the one before.
    fn enter(&mut self, cell: &TulispObject, new: &TulispObject) -> Result<(), Error> {
        self.add_to_path(cell)?;
        self.lists.push(CopyingList {
            cell: cell.clone(),
            new: new.clone(),
            car_done: false,
            cells: 1,
        });
        Ok(())
    }

    /// Moves the innermost list on to CELL, its next cons, copied into NEW.
    fn step(&mut self, cell: &TulispObject, new: TulispObject) -> Result<(), Error> {
        self.add_to_path(cell)?;
        if let Some(list) = self.lists.last_mut() {
            list.cell = cell.clone();
            list.new = new;
            list.car_done = false;
            list.cells += 1;
        }
        Ok(())
    }

    /// Ends the innermost list, and takes its conses off the path.
    fn leave(&mut self) {
        let Some(list) = self.lists.pop() else {
            return;
        };
        for addr in self.path.drain(self.path.len() - list.cells..) {
            self.on_path.remove(&addr);
        }
    }

    fn add_to_path(&mut self, cell: &TulispObject) -> Result<(), Error> {
        let addr = cell.addr_as_usize();
        if !self.on_path.insert(addr) {
            return Err(Error::circular_list());
        }
        self.path.push(addr);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, TulispObject};

    // A tree nested deeper than any stack would hold, in its cars or in its
    // cdrs, copies.
    #[test]
    fn a_deep_tree_copies() {
        let mut tree = TulispObject::nil();
        for i in 0..200_000 {
            tree = TulispObject::cons(
                TulispObject::cons(i.into(), tree.clone()),
                TulispObject::cons(i.into(), TulispObject::nil()),
            );
        }
        let copy = tree.copy_tree().unwrap();
        assert!(copy.caar().unwrap().eq(&tree.caar().unwrap()));
        assert!(!copy.car().unwrap().eq(&tree.car().unwrap()));
        drop(copy);
        drop(tree);
    }

    // Shared conses that form no loop are copied once for each place they
    // appear.
    #[test]
    fn a_shared_cons_is_copied_where_it_appears() {
        let ctx = &mut TulispContext::new();
        let tree = ctx
            .eval_string("(let ((x (list 1 2))) (list x x (cons x x)))")
            .unwrap();
        let copy = tree.copy_tree().unwrap();
        assert!(copy.equal(&tree));
        assert!(!copy.car().unwrap().eq(&copy.cadr().unwrap()));
    }
}
