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
/// Right after `(` they are functions, macros and special forms, except in a
/// list where a binding form names what it binds; after `#'`, functions; in
/// quoted data, any name; elsewhere, variables, the local ones around OFFSET
/// included. Where a key of a call's keyword parameter goes, the keys that
/// parameter takes are offered too, without the ones the call has given before
/// the cursor. The file's own definitions are offered as well, and win over the
/// context's. Inside a string, a comment or a number there are none.
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
    // The list or prefix the cursor is in.
    let container = match path.last() {
        Some(&id) if scope::is_symbol(tree, id) => tree.node(id).parent(),
        last => last.copied(),
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
        // A list here is the innermost one around the offset. The offset is at
        // its head when no form of it ends before the offset.
        Some(list) if scope::is_list(tree, list) => {
            let at_head = tree
                .forms(list)
                .next()
                .is_none_or(|first| tree.node(first).range().end >= offset);
            if at_head && scope::is_call(tree, list) {
                Place::Call
            } else {
                Place::Value
            }
        }
        _ => Place::Value,
    };

    let mut items = BTreeMap::new();
    for (name, kind) in ctx.symbol_kinds() {
        if name.starts_with(prefix)
            && place.fits(kind)
            && let Some(info) = ctx.describe(name)
        {
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
        for local in scope::locals_at(tree, &path, offset) {
            if local.name.starts_with(prefix) {
                let info = SymbolInfo::new(SymbolKind::Variable, None, None);
                let item = completion(&local.name, info);
                items.insert(local.name, item);
            }
        }
    }
    // Every key starts with `:`.
    if place == Place::Value && (prefix.is_empty() || prefix.starts_with(':')) {
        let keys = key_completions(ctx, tree, prefix, offset).unwrap_or_default();
        for item in keys {
            items.insert(item.name.clone(), item);
        }
    }
    Completions {
        range,
        items: items.into_values().collect(),
    }
}

/// The keys to offer at OFFSET, in the innermost call around it: the keys of
/// the parameter that the argument at OFFSET fills, that start with PREFIX,
/// without the ones the call gives before OFFSET. `None` where no key goes:
/// outside a call with a known signature, at no parameter of it, at a parameter
/// that declares no keys, or where a key's value goes.
fn key_completions(
    ctx: &TulispContext,
    tree: &SyntaxTree,
    prefix: &str,
    offset: usize,
) -> Option<Vec<Completion>> {
    let help = signature_help(ctx, tree, offset)?;
    let active = help.active?;
    let keys = help.signature.params.into_iter().nth(active)?.keys;
    if keys.is_empty() {
        return None;
    }
    let call = tree.call_at(offset)?;
    // From the first of the plist's arguments, every second form is a key. The
    // head and each parameter before the plist take one form, so its arguments
    // start at ACTIVE + 1.
    if !call.position.checked_sub(active + 1)?.is_multiple_of(2) {
        return None;
    }
    // The keys the call has given before the cursor.
    let given: Vec<&str> = tree
        .forms(call.list)
        .take(call.position)
        .skip(active + 1)
        .step_by(2)
        .filter(|form| scope::is_symbol(tree, *form))
        .map(|form| tree.text(form))
        .collect();
    let items = keys
        .iter()
        .filter(|key| key.starts_with(prefix) && !given.contains(&key.as_ref()))
        .map(|key| completion(key, SymbolInfo::new(SymbolKind::Keyword, None, None)))
        .collect();
    Some(items)
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
/// not a name with a known signature, in quoted data, in a list where a binding
/// form names what it binds, or in a string or a comment.
pub fn signature_help(
    ctx: &TulispContext,
    tree: &SyntaxTree,
    offset: usize,
) -> Option<SignatureHelp> {
    let offset = tree.clamp(offset);
    let call = tree.call_at(offset)?;
    if !scope::is_call(tree, call.list) {
        return None;
    }
    let name = scope::head(tree, call.list)?;
    let info = scope::lookup(ctx, tree, name)?;
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
    let path = tree.path_at(offset);
    let id = *path.last()?;
    if !scope::is_symbol(tree, id) {
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
    // A local never hides a function: a call head runs the global one.
    if !scope::is_call_head(tree, id)
        && !scope::quoted(tree, id)
        && let Some(local) = scope::locals_at(tree, &path, offset)
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

/// The places where the source cannot be read, and why, in order.
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
        let names = complete(&context(), "(defconst my-const 1) (car my-|");
        assert!(has(&names, "my-const"));
    }

    #[test]
    fn a_file_defvar_has_no_signature() {
        let ctx = context();
        let (text, offset) = at_cursor("(defvar my-var 1 \"Doc.\n\n(fn A)\") (car my-|");
        let tree = read(&text);
        let found = completions(&ctx, &tree, offset);
        let item = found
            .items
            .iter()
            .find(|i| i.name == "my-var")
            .expect("my-var");
        assert_eq!(item.signature, None);
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
            bee<":bee">: i64 {= 0},
        }
    }

    #[test]
    fn a_call_offers_the_keys_its_plist_parameter_takes() {
        let ctx = hint_context();
        let names = complete(&ctx, "(kw 1 |");
        assert!(has(&names, ":a") && has(&names, ":bee"));
        // Only the keys that match what is typed.
        assert!(has(&complete(&ctx, "(kw 1 :|"), ":a"));
        assert!(!has(&complete(&ctx, "(kw 1 :a|"), ":bee"));
        // A positional argument takes no keys, nor does a call without a plist
        // parameter.
        assert!(!has(&complete(&ctx, "(kw |"), ":a"));
        assert!(!has(&complete(&ctx, "(fixed 1 |"), ":a"));
        // Not after `#'` or `'`, where a key is not being typed.
        assert!(!has(&complete(&ctx, "(kw 1 #'|"), ":a"));
        assert!(!has(&complete(&ctx, "(kw 1 '|"), ":a"));
        // The keys come as keywords.
        let (text, offset) = at_cursor("(kw 1 |");
        let tree = read(&text);
        let key = completions(&ctx, &tree, offset)
            .items
            .into_iter()
            .find(|item| item.name == ":a")
            .unwrap();
        assert_eq!(key.kind, SymbolKind::Keyword);
    }

    #[test]
    fn a_key_the_call_already_gives_is_not_offered() {
        let ctx = hint_context();
        assert!(!has(&complete(&ctx, "(kw 1 :a 2 |"), ":a"));
        assert!(has(&complete(&ctx, "(kw 1 :a 2 |"), ":bee"));
        // The key being typed is still offered: it may be a prefix of another
        // one, and a cursor at its end has not given it yet.
        assert!(has(&complete(&ctx, "(kw 1 :|bee 2"), ":bee"));
        assert!(has(&complete(&ctx, "(kw 1 :a|"), ":a"));
        // A key's value is not a given key: only every second form from where
        // the plist's arguments start is a key.
        assert!(has(&complete(&ctx, "(kw 1 :a :bee |"), ":bee"));
    }

    #[test]
    fn no_keys_where_a_value_goes() {
        let ctx = hint_context();
        assert!(!has(&complete(&ctx, "(kw 1 :a |"), ":bee"));
        assert!(!has(&complete(&ctx, "(kw 1 :a :|"), ":bee"));
        assert!(!has(&complete(&ctx, "(kw 1 :a ; c\n |"), ":bee"));
        // A comment is not a form, so the value after it fills the slot.
        assert!(has(&complete(&ctx, "(kw 1 :a ; c\n 2 |"), ":bee"));
    }

    #[test]
    fn no_keys_in_quoted_data_or_a_binding_list() {
        let ctx = hint_context();
        assert!(!has(&complete(&ctx, "'(kw 1 :|"), ":a"));
        assert!(!has(&complete(&ctx, "(let ((kw 1 :|"), ":a"));
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
    fn a_file_defun_usage_line_gives_the_signature() {
        let ctx = hint_context();
        let found = help(&ctx, "(defun f (x) \"Doc.\n\n(fn A)\" x) (f |").expect("help");
        assert_eq!(found.signature.render("f"), "(f A)");
        assert_eq!(found.doc.as_deref(), Some("Doc."));
    }

    #[test]
    fn a_variable_has_no_signature_help() {
        let ctx = hint_context();
        assert!(help(&ctx, "(defvar v 1 \"Doc.\n\n(fn A)\") (v |").is_none());
    }

    #[test]
    fn hover_shows_a_context_function() {
        let mut ctx = hint_context();
        ctx.defun(("fixed", "Doc of fixed."), |a: i64| a);
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

    // Under `,` or `,@` a symbol is an argument of the backquoted list, not a
    // call head; under `#'` it names a function.
    #[test]
    fn a_symbol_under_a_comma_is_not_a_call_head() {
        let ctx = hint_context();
        for source in [
            "(let ((fixed 1)) `(a ,fix|ed))",
            "(let ((fixed 1)) `(a ,@fix|ed))",
        ] {
            let found = hover_at(&ctx, source).unwrap_or_else(|| panic!("hover on {source}"));
            assert_eq!(found.info.kind, SymbolKind::Variable, "{source}");
            assert_eq!(found.binding, Some(7..12), "{source}");
        }
        let found = hover_at(&ctx, "(let ((xyz 1)) `(a ,x|yz))").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert_eq!(found.binding, Some(7..10));
        let found = hover_at(&ctx, "(let ((fixed 1)) (mapcar #'fix|ed nil))").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Function);
        assert_eq!(found.binding, None);
    }

    // As in `let*`, each binding in an `if-let` family form's list of bindings
    // sees the ones before it. A single `(x VALUE)` spec does not see its own
    // name.
    #[test]
    fn if_let_bindings_see_the_ones_before_them() {
        let ctx = context();
        assert!(has(
            &complete(&ctx, "(when-let ((abc 1) (abd (+ ab|))) nil)"),
            "abc"
        ));
        assert!(has(
            &complete(&ctx, "(if-let* ((abc 1) (abd (+ ab|))) nil)"),
            "abc"
        ));
        assert!(!has(&complete(&ctx, "(when-let (abc (+ ab|)) nil)"), "abc"));
        let found = hover_at(&ctx, "(when-let ((abc 1) (abd (+ ab|c 1))) abd)").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert_eq!(found.binding, Some(12..15));
    }

    // Where a binding form names what it binds, the names are variables, and
    // the list there is not a call.
    #[test]
    fn binding_lists_are_not_calls() {
        let ctx = context();
        for source in ["(let ((ca|", "(let (ca|", "(defun f (ca|", "(dolist (ca|"] {
            let names = complete(&ctx, source);
            assert!(has(&names, "cat-count"), "{source}: {names:?}");
            assert!(!has(&names, "car"), "{source}: {names:?}");
        }
        assert_eq!(help(&ctx, "(let ((car |"), None);
        assert_eq!(
            help(&ctx, "(let ((x (car |").map(|h| h.name).as_deref(),
            Some("car")
        );
    }

    #[test]
    fn more_binding_forms_bind_their_names() {
        let ctx = context();
        assert!(has(&complete(&ctx, "(defmacro m (aaa) aa|)"), "aaa"));
        assert!(has(&complete(&ctx, "(if-let* ((val 1)) va|)"), "val"));
        assert!(has(&complete(&ctx, "(while-let ((val 1)) va|)"), "val"));
        let found = hover_at(&ctx, "(condition-case ni|l (f) (error 1))");
        assert!(found.is_none_or(|h| h.binding.is_none()));
    }

    // `if-let*` always reads its spec as a list of bindings; only `if-let`,
    // `when-let` and `while-let` take a single `(x VALUE)` binding, and a first
    // form that is `nil` makes the spec a list of bindings.
    #[test]
    fn only_if_let_when_let_and_while_let_take_a_single_binding() {
        let ctx = context();
        assert_eq!(help(&ctx, "(if-let* (abc (car| y)) abc)"), None);
        let names = complete(&ctx, "(if-let* (abc (ca| y)) abc)");
        assert!(has(&names, "cat-count") && !has(&names, "car"), "{names:?}");
        let found = hover_at(&ctx, "(if-let* (abc (car y)) ca|r)").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert_eq!(found.binding, Some(15..18));
        assert_eq!(help(&ctx, "(if-let (nil (car |"), None);
        assert!(!has(&complete(&ctx, "(if-let (abc) ab|)"), "abc"));
    }

    // In a list of bindings, a bare symbol binds itself.
    #[test]
    fn a_bare_symbol_in_a_list_of_bindings_is_bound() {
        let ctx = context();
        let names = complete(&ctx, "(if-let* ((abd 1) abc) ab|)");
        assert!(has(&names, "abc") && has(&names, "abd"), "{names:?}");
        let names = complete(&ctx, "(when-let (abc (abd 1) abe) ab|)");
        assert!(has(&names, "abc") && has(&names, "abe"), "{names:?}");
        let found = hover_at(&ctx, "(if-let* ((abd 1) abc) a|bc)").expect("hover");
        assert_eq!(found.binding, Some(18..21));
    }

    // A `dotimes` variable is seen in the spec's result forms, after the count;
    // a `dolist` variable is not seen in its spec at all.
    #[test]
    fn a_dotimes_variable_is_seen_in_its_result_forms() {
        let ctx = context();
        assert!(has(&complete(&ctx, "(dotimes (ixy 3 ix|) nil)"), "ixy"));
        let found = hover_at(&ctx, "(dotimes (ixy 3 ix|y) nil)").expect("hover");
        assert_eq!(found.binding, Some(10..13));
        assert!(!has(
            &complete(&ctx, "(dotimes (ixy (length ix|)) nil)"),
            "ixy"
        ));
        assert!(!has(&complete(&ctx, "(dotimes (ixy (length ix| 1"), "ixy"));
        assert!(!has(&complete(&ctx, "(dotimes (ixy ix|) nil)"), "ixy"));
        assert!(!has(&complete(&ctx, "(dolist (ixy '(1) ix|) nil)"), "ixy"));
    }

    // A binding whose name is that of a binding form is not read as that form.
    #[test]
    fn a_binding_named_like_a_binding_form_is_not_one() {
        let ctx = hint_context();
        let source = "(let ((lambda (ca|r y))) lambda)";
        let found = hover_at(&ctx, source).expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Function);
        assert_eq!(found.binding, None);
        assert_eq!(help(&ctx, source).map(|h| h.name).as_deref(), Some("car"));
    }

    // A `dolist` spec holds no bindings, so a list in it is a call; a list in
    // an `if-let` family list of bindings is a binding, so its first form is
    // not a call head.
    #[test]
    fn lists_in_specs_are_calls_only_where_they_bind_nothing() {
        let ctx = context();
        assert_eq!(
            help(&ctx, "(dolist (x (car |").map(|h| h.name).as_deref(),
            Some("car")
        );
        let found = hover_at(&ctx, "(let ((xq 1)) (when-let ((y 2) (x|q)) y))").expect("hover");
        assert_eq!(found.info.kind, SymbolKind::Variable);
        assert_eq!(found.binding, Some(7..9));
    }

    #[test]
    fn signature_help_ignores_a_local_of_the_same_name() {
        let ctx = hint_context();
        let found = help(&ctx, "(let ((fixed 1)) (fixed |))").expect("help");
        assert_eq!(found.name, "fixed");
    }
}
