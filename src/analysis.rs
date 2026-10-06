//! Answers for an editor about Lisp source and a context: completion, argument
//! hints, hover and diagnostics. Every function reads a [`SyntaxTree`] and a
//! [`TulispContext`]; none evaluates code or interns a name, and none changes
//! the context.

mod scope;

use std::collections::BTreeMap;
use std::ops::Range;

use crate::TulispContext;
use crate::symbols::{ParamPosition, Signature, SymbolInfo, SymbolKind};
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
            // A cursor at a symbol's start is not typing it.
            NodeKind::Atom(AtomKind::Symbol) if tree.node(id).range().start < offset => Some(id),
            NodeKind::Atom(AtomKind::Symbol) => None,
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
        None => match path.last() {
            Some(&id) if matches!(tree.node(id).kind(), NodeKind::Atom(AtomKind::Symbol)) => {
                tree.node(id).parent()
            }
            last => last.copied(),
        },
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
                .take_while(|&form| Some(form) != typed && tree.node(form).range().end < offset)
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

/// The call around an offset: what is called, its parameters, and which of them
/// the argument at the offset fills.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct SignatureHelp {
    pub name: String,
    pub signature: Signature,
    pub doc: Option<String>,
    /// The index in `signature.params` of the parameter the argument at the
    /// offset fills. `None` on the function's name, or past the last parameter.
    pub active: Option<usize>,
}

/// The parameter that argument ARG, counted from 0, fills: a `&rest` or keyword
/// parameter takes every argument from its own on.
fn active_param(signature: &Signature, arg: usize) -> Option<usize> {
    let mut position = 0;
    for (index, param) in signature.params.iter().enumerate() {
        match param.position {
            ParamPosition::Rest | ParamPosition::Keywords => return Some(index),
            ParamPosition::Required | ParamPosition::Optional if position == arg => {
                return Some(index);
            }
            ParamPosition::Required | ParamPosition::Optional => position += 1,
        }
    }
    None
}

/// The signature of the innermost call around OFFSET, the file's own definition
/// first, and which parameter the offset is at. `None` when the call's head is
/// not a name with a known signature, in quoted data, or in a string or a
/// comment.
pub fn signature_help(
    ctx: &TulispContext,
    tree: &SyntaxTree,
    offset: usize,
) -> Option<SignatureHelp> {
    let offset = tree.clamp(offset);
    let call = tree.call_at(offset)?;
    if scope::quoted(tree, call.list) {
        return None;
    }
    let head = tree.forms(call.list).next()?;
    if !matches!(tree.node(head).kind(), NodeKind::Atom(AtomKind::Symbol)) {
        return None;
    }
    let name = tree.text(head);
    let info = scope::lookup(ctx, tree, name)?;
    if info.kind == SymbolKind::Variable {
        return None;
    }
    let signature = info.signature?;
    let active = call
        .position
        .checked_sub(1)
        .and_then(|arg| active_param(&signature, arg));
    Some(SignatureHelp {
        name: name.to_string(),
        signature,
        doc: info.doc,
        active,
    })
}

/// What a name under the cursor is.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Hover {
    /// The name's text.
    pub range: Range<usize>,
    pub name: String,
    pub info: SymbolInfo,
    /// For a local variable, the name where it is bound.
    pub binding: Option<Range<usize>>,
}

/// What the symbol at OFFSET is: a local variable in scope there, else the
/// file's definition, else the context's. `None` off a symbol, or for a name
/// nothing defines.
pub fn hover(ctx: &TulispContext, tree: &SyntaxTree, offset: usize) -> Option<Hover> {
    let offset = tree.clamp(offset);
    let id = *tree.path_at(offset).last()?;
    if !matches!(tree.node(id).kind(), NodeKind::Atom(AtomKind::Symbol)) {
        return None;
    }
    let name = tree.text(id);
    let range = tree.node(id).range();
    if scope::is_binding_site(tree, id) {
        return Some(Hover {
            range: range.clone(),
            name: name.to_string(),
            info: SymbolInfo::new(SymbolKind::Variable, None, None),
            binding: Some(range),
        });
    }
    let is_call_head = tree
        .node(id)
        .parent()
        .is_some_and(|list| tree.forms(list).next() == Some(id));
    // A local never hides a function: a call head runs the global one.
    if !is_call_head
        && !scope::quoted(tree, id)
        && let Some(local) = scope::locals_at(tree, offset)
            .into_iter()
            .rev()
            .find(|local| local.name == name)
    {
        return Some(Hover {
            range,
            name: name.to_string(),
            info: SymbolInfo::new(SymbolKind::Variable, None, None),
            binding: Some(local.range),
        });
    }
    let info = scope::lookup(ctx, tree, name)?;
    Some(Hover {
        range,
        name: name.to_string(),
        info,
        binding: None,
    })
}

/// A problem in the source.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Diagnostic {
    pub range: Range<usize>,
    pub message: String,
}

/// The problems in the source: for now, where it cannot be read.
pub fn diagnostics(tree: &SyntaxTree) -> Vec<Diagnostic> {
    tree.errors()
        .iter()
        .map(|error| Diagnostic {
            range: error.range.clone(),
            message: error.message.clone(),
        })
        .collect()
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

    #[test]
    fn bindings_are_not_in_scope_in_an_unclosed_form() {
        let ctx = context();
        assert!(!has(&complete(&ctx, "(let ((abc 1) (abd ab|"), "abc"));
        assert!(!has(&complete(&ctx, "(condition-case err (f er|"), "err"));
        assert!(!has(&complete(&ctx, "(dolist (xx (f x|"), "xx"));
        assert!(!has(&complete(&ctx, "(defun f (aa ab|"), "ab"));
        assert!(!has(&complete(&ctx, "(let* ((abc 1) (abd (f ab|"), "abd"));
    }

    #[test]
    fn a_cursor_at_a_symbols_start_types_nothing() {
        let ctx = context();
        let (text, offset) = at_cursor("(f |car)");
        let tree = read(&text);
        assert_eq!(completions(&ctx, &tree, offset).range, 3..3);
    }

    #[test]
    fn a_file_defvar_is_a_variable() {
        let names = complete(&context(), "(defvar my-var 1) (car my-|");
        assert!(has(&names, "my-var"));
    }

    #[test]
    fn the_last_of_two_definitions_is_offered() {
        let ctx = context();
        let (text, offset) = at_cursor("(defun twice (a) a) (defun twice (b c) b) (twi|");
        let tree = read(&text);
        let found = completions(&ctx, &tree, offset);
        let item = found
            .items
            .iter()
            .find(|i| i.name == "twice")
            .expect("twice");
        let signature = item.signature.as_ref().expect("a signature");
        assert_eq!(signature.render("twice"), "(twice B C)");
    }

    #[test]
    fn a_comma_ends_quoting() {
        let ctx = context();
        let names = complete(&ctx, "`(a ,(ca|");
        assert!(has(&names, "car") && !has(&names, "cat-count"));
        let names = complete(&ctx, "`(a ,ca|");
        assert!(has(&names, "cat-count") && !has(&names, "car"));
    }

    #[test]
    fn a_dolist_variable_is_not_seen_in_its_own_spec() {
        assert!(!has(
            &complete(&context(), "(dolist (item (f ite|)) nil)"),
            "item"
        ));
    }

    crate::AsList! {
        struct Opts {
            a: Option<i64>,
        }
    }

    fn help(ctx: &TulispContext, source: &str) -> Option<SignatureHelp> {
        let (text, offset) = at_cursor(source);
        let tree = read(&text);
        signature_help(ctx, &tree, offset)
    }

    fn hover_at(ctx: &TulispContext, source: &str) -> Option<Hover> {
        let (text, offset) = at_cursor(source);
        let tree = read(&text);
        hover(ctx, &tree, offset)
    }

    fn hint_context() -> TulispContext {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun three (a &optional b &rest c) a)")
            .unwrap();
        ctx.defun("fixed", |a: i64| a);
        ctx.defun("kw", |a: i64, o: crate::Plist<Opts>| a + o.a.unwrap_or(0));
        ctx
    }

    #[test]
    fn the_active_parameter_follows_the_cursor() {
        let ctx = hint_context();
        let active = |source| help(&ctx, source).and_then(|h| h.active);
        assert_eq!(active("(three |"), Some(0));
        assert_eq!(active("(three 1 |"), Some(1));
        assert_eq!(active("(three 1 2 3 |"), Some(2));
        assert_eq!(active("(fixed 1 2 |"), None);
        assert_eq!(active("(kw 1 :a |"), Some(1));
        let on_name = help(&ctx, "(thr|ee 1)").expect("help on the name");
        assert_eq!(on_name.name, "three");
        assert_eq!(on_name.active, None);
    }

    #[test]
    fn the_innermost_call_wins() {
        let ctx = hint_context();
        assert_eq!(
            help(&ctx, "(three (fixed |").map(|h| h.name).as_deref(),
            Some("fixed")
        );
    }

    #[test]
    fn no_signature_in_strings_comments_or_quoted_lists() {
        let ctx = hint_context();
        assert!(help(&ctx, "(three \"|\")").is_none());
        assert!(help(&ctx, "(three ; |\n)").is_none());
        assert!(help(&ctx, "'(three |)").is_none());
    }

    #[test]
    fn signature_help_at_the_end_of_an_unclosed_file() {
        let ctx = hint_context();
        let found = help(&ctx, "(three 1 (fixed 2) |").expect("help");
        assert_eq!(found.name, "three");
        assert_eq!(found.active, Some(2));
    }

    #[test]
    fn signature_help_uses_the_file_definition() {
        let ctx = hint_context();
        let found = help(&ctx, "(defun three (x) x) (three |").expect("help");
        assert_eq!(found.signature.render("three"), "(three X)");
    }

    #[test]
    fn hover_shows_a_context_function() {
        let mut ctx = hint_context();
        ctx.set_doc("fixed", "Doc of fixed.").unwrap();
        let found = hover_at(&ctx, "(fix|ed 1)").expect("hover");
        assert_eq!(found.name, "fixed");
        assert_eq!(found.range, 1..6);
        assert_eq!(found.info.doc.as_deref(), Some("Doc of fixed."));
        assert_eq!(found.binding, None);
    }

    #[test]
    fn hover_shows_where_a_local_was_bound() {
        let ctx = hint_context();
        let source = "(let ((xyz 1)) (+ x|yz 1))";
        let found = hover_at(&ctx, source).expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert_eq!(found.binding, Some(7..10));
    }

    #[test]
    fn a_local_does_not_hide_a_function_in_call_position() {
        let ctx = hint_context();
        let found = hover_at(&ctx, "(let ((fixed 1)) (fix|ed))").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Function);
        assert_eq!(found.binding, None);
    }

    #[test]
    fn a_local_used_as_an_argument_still_hovers_as_the_local() {
        let ctx = hint_context();
        let found = hover_at(&ctx, "(let ((fixed 1)) (+ fix|ed 1))").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert!(found.binding.is_some());
    }

    #[test]
    fn hover_shows_a_file_definition() {
        let ctx = hint_context();
        let found = hover_at(&ctx, "(defun ff (a) \"Doc of ff.\" a) (f|f 1)").expect("hover");
        assert_eq!(found.info.doc.as_deref(), Some("Doc of ff."));
    }

    #[test]
    fn no_hover_off_a_symbol() {
        let ctx = hint_context();
        assert!(hover_at(&ctx, "(f \"a|b\")").is_none());
        assert!(hover_at(&ctx, "(f 1|2)").is_none());
        assert!(hover_at(&ctx, "(f (|))").is_none());
    }

    #[test]
    fn hover_past_the_end_is_none() {
        let ctx = hint_context();
        let tree = read("(fixed 1)");
        assert!(hover(&ctx, &tree, 100).is_none());
    }

    #[test]
    fn diagnostics_are_the_tree_errors() {
        let found = diagnostics(&read("(a"));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].message, "Unclosed list");
        assert_eq!(found[0].range, 0..1);
        assert!(diagnostics(&read("(a)")).is_empty());
    }

    #[test]
    fn hover_on_a_binding_shows_the_variable() {
        let ctx = hint_context();
        for source in [
            "(let ((xy|z 1)) xyz)",
            "(let* (ab|c) abc)",
            "(lambda (a|) a)",
            "(defun f (a|) a)",
            "(dolist (it|em l) item)",
            "(dotimes (i|dx 3) idx)",
            "(condition-case er|r nil (error err))",
            "(if-let (v|al 1) val)",
            "(when-let ((v|al 1)) val)",
        ] {
            let found = hover_at(&ctx, source).unwrap_or_else(|| panic!("hover on {source}"));
            assert_eq!(found.info.kind, SymbolKind::Variable, "{source}");
            assert_eq!(found.binding, Some(found.range.clone()), "{source}");
        }
        let found = hover_at(&ctx, "(defun f (&opt|ional a) a)");
        assert!(found.is_none_or(|h| h.binding.is_none()));
    }

    #[test]
    fn signature_help_ignores_a_local_of_the_same_name() {
        let ctx = hint_context();
        let found = help(&ctx, "(let ((fixed 1)) (fixed |))").expect("help");
        assert_eq!(found.name, "fixed");
    }
}
