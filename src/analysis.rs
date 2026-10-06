//! Answers for an editor about Lisp source and a context: completion, argument
//! hints, hover and diagnostics. Every function reads a [`SyntaxTree`] and a
//! [`TulispContext`]; none evaluates code or interns a name, and none changes
//! the context.

mod scope;

use std::collections::BTreeMap;
use std::ops::Range;

use crate::TulispContext;
use crate::symbols::{Signature, SymbolInfo, SymbolKind};
use crate::syntax::{AtomKind, NodeKind, Prefix, SyntaxTree};

/// A name that can complete what is being typed.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Completion {
    pub name: String,
    pub kind: SymbolKind,
    pub signature: Option<Signature>,
    /// The first line of its docstring.
    pub summary: Option<String>,
}

/// The names that can complete what is typed at an offset, by name, and the
/// text a completion replaces.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Completions {
    pub range: Range<usize>,
    pub items: Vec<Completion>,
}

/// Which names fit where the cursor is.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Place {
    /// The head of a call: functions, macros and special forms.
    Call,
    /// After `#'`: functions.
    Function,
    /// An argument: variables.
    Value,
    /// Quoted data: any name.
    Any,
}

impl Place {
    fn fits(self, kind: SymbolKind) -> bool {
        match self {
            Place::Call => kind != SymbolKind::Variable,
            Place::Function => kind == SymbolKind::Function,
            Place::Value => kind == SymbolKind::Variable,
            Place::Any => true,
        }
    }
}

fn completion(name: &str, info: SymbolInfo) -> Completion {
    Completion {
        name: name.to_string(),
        kind: info.kind,
        summary: info
            .doc
            .as_deref()
            .and_then(|doc| doc.lines().next())
            .map(str::to_string),
        signature: info.signature,
    }
}

/// What can complete the symbol being typed at OFFSET, or start one there.
/// Right after `(` they are functions, macros and special forms; after `#'`,
/// functions; in quoted data, any name; elsewhere, variables, the local ones
/// around OFFSET included. The file's own definitions are offered too, and win
/// over the context's. Inside a string, a comment or a number there are none.
pub fn completions(ctx: &TulispContext, tree: &SyntaxTree, offset: usize) -> Completions {
    let offset = tree.clamp(offset);
    let path = tree.path_at(offset);
    let typed = match path.last() {
        Some(&id) => match tree.node(id).kind() {
            NodeKind::Atom(AtomKind::Symbol) => Some(id),
            NodeKind::List { .. } | NodeKind::Prefix { .. } => None,
            NodeKind::Atom(
                AtomKind::Integer | AtomKind::Float | AtomKind::String | AtomKind::Character,
            )
            | NodeKind::Dot
            | NodeKind::Comment
            | NodeKind::Error => {
                return Completions {
                    range: offset..offset,
                    items: Vec::new(),
                };
            }
        },
        None => None,
    };
    let (range, prefix) = match typed {
        Some(id) => {
            let range = tree.node(id).range();
            let prefix = &tree.source()[range.start..offset];
            (range, prefix)
        }
        None => (offset..offset, ""),
    };
    let container = match typed {
        Some(id) => tree.node(id).parent(),
        None => path.last().copied(),
    };
    let place = match container {
        Some(id)
            if matches!(
                tree.node(id).kind(),
                NodeKind::Prefix {
                    prefix: Prefix::Function,
                    ..
                }
            ) =>
        {
            Place::Function
        }
        Some(id) if scope::quoted(tree, id) => Place::Any,
        Some(list) if matches!(tree.node(list).kind(), NodeKind::List { .. }) => {
            // At the head when no form ends before the offset.
            let before = tree
                .forms(list)
                .iter()
                .take_while(|&&form| Some(form) != typed && tree.node(form).range().end < offset)
                .count();
            if before == 0 {
                Place::Call
            } else {
                Place::Value
            }
        }
        _ => Place::Value,
    };

    let mut items = BTreeMap::new();
    for (name, info) in ctx.symbols() {
        if name.starts_with(prefix) && place.fits(info.kind) {
            items.insert(name.to_string(), completion(name, info));
        }
    }
    for definition in scope::definitions(tree) {
        if definition.name.starts_with(prefix) && place.fits(definition.info.kind) {
            let item = completion(&definition.name, definition.info);
            items.insert(definition.name, item);
        }
    }
    if matches!(place, Place::Value | Place::Any) {
        for local in scope::locals_at(tree, offset) {
            if local.name.starts_with(prefix) {
                let info = SymbolInfo::new(SymbolKind::Variable, None, None);
                let item = completion(&local.name, info);
                items.insert(local.name, item);
            }
        }
    }
    Completions {
        range,
        items: items.into_values().collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::read;

    /// SOURCE without its `|`, and the offset the `|` was at.
    fn at_cursor(source: &str) -> (String, usize) {
        let offset = source.find('|').expect("a | in the source");
        (source.replacen('|', "", 1), offset)
    }

    fn complete(ctx: &TulispContext, source: &str) -> Vec<String> {
        let (text, offset) = at_cursor(source);
        let tree = read(&text);
        completions(ctx, &tree, offset)
            .items
            .into_iter()
            .map(|item| item.name)
            .collect()
    }

    fn has(names: &[String], name: &str) -> bool {
        names.iter().any(|n| n == name)
    }

    fn context() -> TulispContext {
        let mut ctx = TulispContext::new();
        ctx.defvar("cat-count", 1).unwrap();
        ctx
    }

    #[test]
    fn the_head_of_a_call_completes_functions() {
        let names = complete(&context(), "(ca|");
        assert!(has(&names, "car"));
        assert!(!has(&names, "cat-count"));
    }

    #[test]
    fn right_after_a_paren_every_function_is_offered() {
        let names = complete(&context(), "(|");
        assert!(has(&names, "car"));
        assert!(has(&names, "if"));
        assert!(!has(&names, "cat-count"));
    }

    #[test]
    fn an_argument_completes_variables() {
        let names = complete(&context(), "(car ca|");
        assert!(has(&names, "cat-count"));
        assert!(!has(&names, "car"));
    }

    #[test]
    fn sharp_quote_completes_functions_only() {
        let names = complete(&context(), "(mapcar #'ca|");
        assert!(has(&names, "car"));
        assert!(!has(&names, "cat-count"));
    }

    #[test]
    fn a_quote_completes_any_name() {
        let names = complete(&context(), "'ca|");
        assert!(has(&names, "car"));
        assert!(has(&names, "cat-count"));
    }

    #[test]
    fn nothing_completes_in_a_string_a_comment_or_a_number() {
        let ctx = context();
        assert!(complete(&ctx, "(f \"ca|\")").is_empty());
        assert!(complete(&ctx, "; ca|").is_empty());
        assert!(complete(&ctx, "(f 12|)").is_empty());
    }

    #[test]
    fn nothing_completes_in_an_unfinished_string() {
        assert!(complete(&context(), "(f \"ca|").is_empty());
    }

    #[test]
    fn the_typed_symbol_is_what_a_completion_replaces() {
        let ctx = context();
        let (text, offset) = at_cursor("(cad|r x)");
        let tree = read(&text);
        assert_eq!(completions(&ctx, &tree, offset).range, 1..5);
        let (text, offset) = at_cursor("(|");
        let tree = read(&text);
        assert_eq!(completions(&ctx, &tree, offset).range, 1..1);
    }

    #[test]
    fn let_variables_are_seen_in_the_body_only() {
        let ctx = context();
        assert!(has(&complete(&ctx, "(let ((abc 1)) ab|)"), "abc"));
        assert!(!has(
            &complete(&ctx, "(let ((abc 1) (abd ab|)) nil)"),
            "abc"
        ));
        let names = complete(&ctx, "(let* ((abc 1) (abd ab|)) nil)");
        assert!(has(&names, "abc"));
        assert!(!has(&names, "abd"));
        assert!(!has(&complete(&ctx, "(let ((abc 1)) nil) ab|"), "abc"));
    }

    #[test]
    fn parameters_are_variables_in_the_body() {
        let ctx = context();
        let names = complete(&ctx, "(defun f (aaa &optional aab &rest aac) aa|)");
        assert!(has(&names, "aaa") && has(&names, "aab") && has(&names, "aac"));
        assert!(!has(
            &complete(&ctx, "(defun f (aaa &optional b) &|)"),
            "&optional"
        ));
        assert!(has(&complete(&ctx, "(lambda (aaa) aa|)"), "aaa"));
        assert!(!has(&complete(&ctx, "(defun f (aaa) nil) aa|"), "aaa"));
    }

    #[test]
    fn loop_and_conditional_bindings() {
        let ctx = context();
        assert!(has(&complete(&ctx, "(dolist (item '(1)) ite|)"), "item"));
        assert!(has(&complete(&ctx, "(dotimes (idx 3) id|)"), "idx"));
        assert!(has(&complete(&ctx, "(when-let ((val 1)) va|)"), "val"));
        assert!(has(&complete(&ctx, "(if-let (val 1) va|)"), "val"));
        assert!(has(
            &complete(&ctx, "(condition-case err (f) (error er|))"),
            "err"
        ));
        assert!(!has(
            &complete(&ctx, "(condition-case err (er|) (error nil))"),
            "err"
        ));
    }

    #[test]
    fn the_file_wins_over_the_context() {
        let mut ctx = context();
        ctx.defun("my-fn", |a: i64| a);
        let (text, offset) = at_cursor("(defun my-fn (x y) x) (my-|");
        let tree = read(&text);
        let found = completions(&ctx, &tree, offset);
        let item = found
            .items
            .iter()
            .find(|item| item.name == "my-fn")
            .expect("my-fn");
        let signature = item.signature.as_ref().expect("a signature");
        assert_eq!(signature.render("my-fn"), "(my-fn X Y)");
    }

    #[test]
    fn a_definition_further_down_counts() {
        assert!(has(
            &complete(&context(), "(lat|) (defun later-fn () 1)"),
            "later-fn"
        ));
    }

    #[test]
    fn a_quoted_definition_defines_nothing() {
        let ctx = context();
        assert!(!has(
            &complete(&ctx, "'(defun quoted-fn () 1) (quoted-|"),
            "quoted-fn"
        ));
        assert!(!has(&complete(&ctx, "`(defun bq-fn () 1) (bq-|"), "bq-fn"));
    }

    #[test]
    fn completion_does_not_intern() {
        let ctx = context();
        let before = ctx.obarray_len();
        complete(&ctx, "(defun new-thing (zzz) zz|)");
        complete(&ctx, "(unknown-name|");
        assert_eq!(ctx.obarray_len(), before);
    }

    #[test]
    fn completion_works_at_the_end_of_an_unclosed_file() {
        let names = complete(&context(), "(car (cdr ca|");
        assert!(has(&names, "cat-count"));
    }

    #[test]
    fn completion_in_an_empty_file() {
        let ctx = context();
        let tree = read("");
        let found = completions(&ctx, &tree, 0);
        assert_eq!(found.range, 0..0);
    }

    #[test]
    fn an_offset_past_the_end_is_the_end() {
        let ctx = context();
        let tree = read("(ca");
        let names: Vec<String> = completions(&ctx, &tree, 100)
            .items
            .into_iter()
            .map(|item| item.name)
            .collect();
        assert!(has(&names, "car"));
    }

    #[test]
    fn an_offset_inside_a_character_does_not_panic() {
        let ctx = context();
        // `é` is bytes 1..3; offset 2 is inside it.
        let tree = read("(é");
        completions(&ctx, &tree, 2);
    }
}
