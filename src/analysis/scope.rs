//! What a file defines, and the local variables around an offset.

use std::ops::Range;

use crate::TulispContext;
use crate::symbols::{Signature, SymbolInfo, SymbolKind};
use crate::syntax::{AtomKind, NodeId, NodeKind, Prefix, SyntaxTree, string_value};

/// Whether ID is data: inside a `'` or `` ` ``, with no `,` or `,@` nearer to
/// it.
pub(super) fn quoted(tree: &SyntaxTree, id: NodeId) -> bool {
    let mut at = Some(id);
    while let Some(current) = at {
        if let NodeKind::Prefix { prefix, .. } = tree.node(current).kind() {
            match prefix {
                Prefix::Quote | Prefix::Backquote => return true,
                Prefix::Comma | Prefix::Splice => return false,
                Prefix::Function => {}
            }
        }
        at = tree.node(current).parent();
    }
    false
}

fn is_symbol(tree: &SyntaxTree, id: NodeId) -> bool {
    matches!(tree.node(id).kind(), NodeKind::Atom(AtomKind::Symbol))
}

fn is_list(tree: &SyntaxTree, id: NodeId) -> bool {
    matches!(tree.node(id).kind(), NodeKind::List { .. })
}

/// The name a list starts with.
pub(super) fn head<'a>(tree: &SyntaxTree<'a>, list: NodeId) -> Option<&'a str> {
    let first = *tree.forms(list).first()?;
    is_symbol(tree, first).then(|| tree.text(first))
}

fn string_of(tree: &SyntaxTree, id: NodeId) -> Option<String> {
    matches!(tree.node(id).kind(), NodeKind::Atom(AtomKind::String))
        .then(|| string_value(tree.text(id)))
        .flatten()
}

/// The signature of PARAMS, a parameter list in the tree.
fn lambda_list(tree: &SyntaxTree, params: NodeId) -> Signature {
    if !is_list(tree, params) {
        return Signature::default();
    }
    let names: Vec<&str> = tree
        .forms(params)
        .into_iter()
        .filter(|&p| is_symbol(tree, p))
        .map(|p| tree.text(p))
        .collect();
    Signature::from_lambda_list(names)
}

/// The docstring of BODY, by the compiler's rule: a leading string with more
/// forms after it, not counting a `declare` right after the string.
fn body_docstring(tree: &SyntaxTree, body: &[NodeId]) -> Option<String> {
    let (&first, rest) = body.split_first()?;
    let doc = string_of(tree, first)?;
    let rest = match rest.split_first() {
        Some((&next, after)) if is_list(tree, next) && head(tree, next) == Some("declare") => after,
        _ => rest,
    };
    (!rest.is_empty()).then_some(doc)
}

/// A name the file defines with `defun`, `defmacro` or `defvar`.
pub(super) struct Definition {
    pub(super) name: String,
    pub(super) info: SymbolInfo,
}

/// Every definition in the file, at any depth, except those in quoted data.
/// Definitions take effect as the file compiles, so one further down counts as
/// well.
pub(super) fn definitions(tree: &SyntaxTree) -> Vec<Definition> {
    let mut found = Vec::new();
    for id in tree.ids() {
        if !is_list(tree, id) || quoted(tree, id) {
            continue;
        }
        let kind = match head(tree, id) {
            Some("defun") => SymbolKind::Function,
            Some("defmacro") => SymbolKind::Macro,
            Some("defvar") => SymbolKind::Variable,
            _ => continue,
        };
        let forms = tree.forms(id);
        let Some(&name) = forms.get(1) else {
            continue;
        };
        if !is_symbol(tree, name) {
            continue;
        }
        let (mut signature, mut doc) = (None, None);
        if kind == SymbolKind::Variable {
            doc = forms.get(3).and_then(|&d| string_of(tree, d));
        } else if let Some(&params) = forms.get(2) {
            signature = Some(lambda_list(tree, params));
            doc = body_docstring(tree, &forms[3..]);
        }
        found.push(Definition {
            name: tree.text(name).to_string(),
            info: SymbolInfo::new(kind, signature, doc),
        });
    }
    found
}

/// What NAME is: the file's own (last) definition, else the context's.
#[cfg_attr(
    not(test),
    expect(dead_code, reason = "hover and argument hints use it in later changes")
)]
pub(super) fn lookup(ctx: &TulispContext, tree: &SyntaxTree, name: &str) -> Option<SymbolInfo> {
    // The last definition wins, as it does when the file compiles.
    definitions(tree)
        .into_iter()
        .rev()
        .find(|definition| definition.name == name)
        .map(|definition| definition.info)
        .or_else(|| ctx.describe(name))
}

/// Whether ID ends at or before OFFSET. An unclosed list runs to the end of the
/// input, so it has not ended even when OFFSET is there.
fn passed(tree: &SyntaxTree, id: NodeId, offset: usize) -> bool {
    !matches!(tree.node(id).kind(), NodeKind::List { closed: false, .. })
        && tree.node(id).range().end <= offset
}

/// A local variable, and where its name is bound.
pub(super) struct Local {
    pub(super) name: String,
    #[expect(dead_code, reason = "diagnostics and hover use it in later changes")]
    pub(super) range: Range<usize>,
}

fn push(tree: &SyntaxTree, locals: &mut Vec<Local>, id: Option<NodeId>) {
    if let Some(id) = id
        && is_symbol(tree, id)
        && tree.text(id) != "nil"
    {
        locals.push(Local {
            name: tree.text(id).to_string(),
            range: tree.node(id).range(),
        });
    }
}

/// The name a binding binds: `x` or `(x VALUE)`.
fn binding_name(tree: &SyntaxTree, binding: NodeId) -> Option<NodeId> {
    if is_symbol(tree, binding) {
        return Some(binding);
    }
    if !is_list(tree, binding) {
        return None;
    }
    tree.forms(binding).first().copied()
}

fn params(tree: &SyntaxTree, list: NodeId, locals: &mut Vec<Local>) {
    if !is_list(tree, list) {
        return;
    }
    for param in tree.forms(list) {
        if !tree.text(param).starts_with('&') {
            push(tree, locals, Some(param));
        }
    }
}

/// The names an `if-let` family SPEC binds: `(x VALUE)`, or a list of such
/// bindings.
fn if_let_names(tree: &SyntaxTree, spec: NodeId, locals: &mut Vec<Local>) {
    if !is_list(tree, spec) {
        return;
    }
    let forms = tree.forms(spec);
    if forms.len() == 2 && is_symbol(tree, forms[0]) {
        push(tree, locals, Some(forms[0]));
        return;
    }
    for binding in forms {
        if is_list(tree, binding) && tree.forms(binding).len() == 2 {
            push(tree, locals, binding_name(tree, binding));
        }
    }
}

/// The local variables in scope at OFFSET, outermost binding first. The
/// forms that bind them are the built-in ones: `let`, `let*`, `lambda`,
/// `defun`, `defmacro`, `dolist`, `dotimes`, the `if-let` family and
/// `condition-case`.
pub(super) fn locals_at(tree: &SyntaxTree, offset: usize) -> Vec<Local> {
    let mut locals = Vec::new();
    for list in tree.path_at(offset) {
        if !is_list(tree, list) || quoted(tree, list) {
            continue;
        }
        let forms = tree.forms(list);
        let past = |i: usize| forms.get(i).is_some_and(|&form| passed(tree, form, offset));
        match head(tree, list) {
            Some("let" | "let*") if past(1) => {
                for binding in tree.forms(forms[1]) {
                    push(tree, &mut locals, binding_name(tree, binding));
                }
            }
            // In `let*`, each binding's value sees the bindings before it.
            Some("let*") if forms.len() > 1 && tree.node(forms[1]).range().start < offset => {
                for binding in tree.forms(forms[1]) {
                    if passed(tree, binding, offset) {
                        push(tree, &mut locals, binding_name(tree, binding));
                    }
                }
            }
            Some("lambda") if past(1) => params(tree, forms[1], &mut locals),
            Some("defun" | "defmacro") if past(2) => params(tree, forms[2], &mut locals),
            Some("dolist" | "dotimes") if past(1) => {
                push(tree, &mut locals, binding_name(tree, forms[1]))
            }
            Some("if-let" | "if-let*" | "when-let" | "while-let") if past(1) => {
                if_let_names(tree, forms[1], &mut locals)
            }
            // The variable is bound in the handlers, after the body form.
            Some("condition-case") if past(2) => push(tree, &mut locals, Some(forms[1])),
            _ => {}
        }
    }
    locals
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::read;

    #[test]
    fn the_last_definition_of_a_name_wins() {
        let ctx = TulispContext::new();
        let tree = read("(defun twice (a) a) (defun twice (b c) b)");
        let info = lookup(&ctx, &tree, "twice").expect("twice");
        let signature = info.signature.expect("a signature");
        assert_eq!(signature.render("twice"), "(twice B C)");
    }
}
