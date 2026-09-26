//! The error symbols a context knows: each one's message, and the conditions a
//! `condition-case` handler can catch it by.

use std::collections::HashMap;

/// An error symbol's message and conditions.
struct ErrorDef {
    #[expect(dead_code, reason = "read by message_string, added next")]
    message: String,
    /// The symbol, then each of its ancestors, each once.
    conditions: Vec<String>,
}

/// The error symbols of a context, by name.
pub(crate) struct ErrorTable {
    defs: HashMap<String, ErrorDef>,
}

/// The error symbols every context starts with: each name, its message and its
/// parents. The built-in error kinds report `error` and the symbols from
/// `wrong-type-argument` on (see `error_symbol`).
const BUILT_IN_ERRORS: &[(&str, &str, &[&str])] = &[
    ("error", "error", &[]),
    ("quit", "Quit", &[]),
    ("user-error", "", &["error"]),
    ("wrong-type-argument", "Wrong type argument", &["error"]),
    ("args-out-of-range", "Args out of range", &["error"]),
    ("arith-error", "Arithmetic error", &["error"]),
    (
        "wrong-number-of-arguments",
        "Wrong number of arguments",
        &["error"],
    ),
    (
        "void-function",
        "Symbol's function definition is void",
        &["error"],
    ),
    (
        "void-variable",
        "Symbol's value as variable is void",
        &["error"],
    ),
    ("invalid-read-syntax", "Invalid read syntax", &["error"]),
    ("not-implemented", "Not implemented", &["error"]),
    ("file-error", "File error", &["error"]),
];

impl ErrorTable {
    pub(crate) fn new() -> Self {
        let mut table = Self {
            defs: HashMap::new(),
        };
        for (name, message, parents) in BUILT_IN_ERRORS {
            table.define(name, message, parents);
        }
        table
    }

    /// Adds NAME, or replaces it. Its conditions are NAME followed by each
    /// parent's conditions, each condition once; a parent not in the table
    /// counts as just itself. Errors already defined under NAME keep the
    /// conditions they got then, as in Emacs.
    pub(crate) fn define(&mut self, name: &str, message: &str, parents: &[&str]) {
        let mut conditions = vec![name.to_string()];
        for parent in parents {
            for condition in self.conditions(parent) {
                if !conditions.iter().any(|c| c == condition) {
                    conditions.push(condition.to_string());
                }
            }
        }
        self.defs.insert(
            name.to_string(),
            ErrorDef {
                message: message.to_string(),
                conditions,
            },
        );
    }

    /// NAME's conditions, or just NAME when it is not in the table.
    fn conditions<'a>(&'a self, name: &'a str) -> Vec<&'a str> {
        match self.defs.get(name) {
            Some(def) => def.conditions.iter().map(String::as_str).collect(),
            None => vec![name],
        }
    }

    /// Whether a handler for CONDITION catches an error whose symbol is NAME.
    pub(crate) fn matches(&self, name: &str, condition: &str) -> bool {
        match self.defs.get(name) {
            Some(def) => def.conditions.iter().any(|c| c == condition),
            None => name == condition,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BUILT_IN_ERRORS, ErrorTable};

    #[test]
    fn every_built_in_error_but_quit_is_an_error() {
        let table = ErrorTable::new();
        for (name, _, _) in BUILT_IN_ERRORS {
            assert!(table.matches(name, name), "{name}");
            assert_eq!(table.matches(name, "error"), *name != "quit", "{name}");
        }
    }

    #[test]
    fn a_child_matches_its_ancestors() {
        let mut table = ErrorTable::new();
        table.define("my-error", "My error", &["arith-error"]);
        table.define("my-child", "Child", &["my-error", "file-error"]);
        for condition in ["my-child", "my-error", "arith-error", "file-error", "error"] {
            assert!(table.matches("my-child", condition), "{condition}");
        }
        assert!(!table.matches("my-child", "quit"));
        assert!(!table.matches("my-error", "my-child"));
        assert_eq!(
            table.conditions("my-child"),
            ["my-child", "my-error", "arith-error", "error", "file-error"]
        );
    }

    #[test]
    fn an_unknown_symbol_matches_only_itself() {
        let mut table = ErrorTable::new();
        assert!(table.matches("no-such-error", "no-such-error"));
        assert!(!table.matches("no-such-error", "error"));
        table.define("lone", "Lone", &["no-such-parent"]);
        assert!(table.matches("lone", "no-such-parent"));
        assert!(!table.matches("lone", "error"));
        table.define("selfish", "Selfish", &["selfish"]);
        assert_eq!(table.conditions("selfish"), ["selfish"]);
    }

    #[test]
    fn a_child_keeps_the_conditions_its_parent_had() {
        let mut table = ErrorTable::new();
        table.define("my-error", "My error", &["arith-error"]);
        table.define("my-child", "Child", &["my-error"]);
        table.define("my-error", "My error", &["file-error"]);
        assert!(table.matches("my-child", "arith-error"));
        assert!(!table.matches("my-child", "file-error"));
    }
}
