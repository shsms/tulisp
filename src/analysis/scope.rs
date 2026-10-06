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

pub(super) fn is_symbol(tree: &SyntaxTree, id: NodeId) -> bool {
    matches!(tree.node(id).kind(), NodeKind::Atom(AtomKind::Symbol))
}

pub(super) fn is_list(tree: &SyntaxTree, id: NodeId) -> bool {
    matches!(tree.node(id).kind(), NodeKind::List { .. })
}

/// The name a list starts with.
pub(super) fn head<'a>(tree: &SyntaxTree<'a>, list: NodeId) -> Option<&'a str> {
    let first = tree.forms(list).next()?;
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
        if !is_list(tree, id) {
            continue;
        }
        let kind = match head(tree, id) {
            Some("defun") => SymbolKind::Function,
            Some("defmacro") => SymbolKind::Macro,
            Some("defvar") => SymbolKind::Variable,
            _ => continue,
        };
        if quoted(tree, id) {
            continue;
        }
        let forms: Vec<NodeId> = tree.forms(id).collect();
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
    pub(super) range: Range<usize>,
}

impl Local {
    fn new(tree: &SyntaxTree, id: NodeId) -> Self {
        Local {
            name: tree.text(id).to_string(),
            range: tree.node(id).range(),
        }
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
    tree.forms(binding).next()
}

/// The parameters in LIST, a parameter list, without `&optional` and `&rest`.
fn params(tree: &SyntaxTree, list: NodeId, names: &mut Vec<NodeId>) {
    names.extend(
        tree.forms(list)
            .filter(|&param| !tree.text(param).starts_with('&')),
    );
}

/// Whether SPEC, an `if-let` family spec, is a single `(x VALUE)` binding
/// instead of a list of bindings.
fn is_single_binding(tree: &SyntaxTree, spec: NodeId) -> bool {
    let mut forms = tree.forms(spec);
    forms.next().is_some_and(|name| is_symbol(tree, name))
        && forms.next().is_some()
        && forms.next().is_none()
}

/// The names an `if-let` family SPEC, a list, binds: `(x VALUE)`, or a list of
/// such bindings.
fn if_let_names(tree: &SyntaxTree, spec: NodeId, names: &mut Vec<NodeId>) {
    if is_single_binding(tree, spec) {
        names.extend(tree.forms(spec).next());
        return;
    }
    for binding in tree.forms(spec) {
        if is_list(tree, binding) && tree.forms(binding).count() == 2 {
            names.extend(binding_name(tree, binding));
        }
    }
}

/// What a binding form binds.
struct Binders {
    /// The names it binds, in order: symbols, and none of them `nil`.
    names: Vec<NodeId>,
    /// Which of the list's forms the names are in scope after, counting from 0
    /// for the head.
    scope_after: usize,
    /// The list that holds the names: a list of bindings, a parameter list, a
    /// `dolist` or `dotimes` spec, or an `if-let` family `(x VALUE)`.
    spec: Option<NodeId>,
    /// Whether SPEC holds `(name VALUE)` bindings: a list of bindings.
    nested: bool,
    /// Whether each binding's value in SPEC sees the bindings before it: that
    /// of a `let*`, or an `if-let` family list of bindings.
    sequential: bool,
}

/// What LIST binds, when it is one of the built-in binding forms: `let`,
/// `let*`, `lambda`, `defun`, `defmacro`, `dolist`, `dotimes`, the `if-let`
/// family and `condition-case`. `None` for any other node.
fn binders(tree: &SyntaxTree, list: NodeId) -> Option<Binders> {
    if !is_list(tree, list) {
        return None;
    }
    let forms: Vec<NodeId> = tree.forms(list).collect();
    let head = forms.first().filter(|&&head| is_symbol(tree, head))?;
    let form = |index: usize| forms.get(index).copied();
    let list_at = |index: usize| form(index).filter(|&form| is_list(tree, form));
    let mut names = Vec::new();
    let (mut nested, mut sequential) = (false, false);
    let (scope_after, spec) = match tree.text(*head) {
        head @ ("let" | "let*") => {
            let spec = list_at(1);
            if let Some(bindings) = spec {
                names.extend(
                    tree.forms(bindings)
                        .filter_map(|binding| binding_name(tree, binding)),
                );
            }
            nested = true;
            sequential = head == "let*";
            (1, spec)
        }
        "lambda" => {
            let spec = list_at(1);
            if let Some(list) = spec {
                params(tree, list, &mut names);
            }
            (1, spec)
        }
        "defun" | "defmacro" => {
            let spec = list_at(2);
            if let Some(list) = spec {
                params(tree, list, &mut names);
            }
            (2, spec)
        }
        "dolist" | "dotimes" => {
            names.extend(form(1).and_then(|spec| binding_name(tree, spec)));
            (1, list_at(1))
        }
        "if-let" | "if-let*" | "when-let" | "while-let" => {
            let spec = list_at(1);
            if let Some(spec) = spec {
                if_let_names(tree, spec, &mut names);
                nested = !is_single_binding(tree, spec);
                sequential = nested;
            }
            (1, spec)
        }
        // The variable is bound in the handlers, after the body form.
        "condition-case" => {
            names.extend(form(1));
            (2, None)
        }
        _ => return None,
    };
    names.retain(|&name| is_symbol(tree, name) && tree.text(name) != "nil");
    Some(Binders {
        names,
        scope_after,
        spec,
        nested,
        sequential,
    })
}

/// Whether LIST is where a binding form names what it binds: the spec
/// [`binders`] gives for its parent, or a binding in the list of bindings
/// [`binders`] gives for the parent of that. Such a list is not a call.
pub(super) fn is_binding_list(tree: &SyntaxTree, list: NodeId) -> bool {
    if !is_list(tree, list) {
        return false;
    }
    let Some(parent) = tree.node(list).parent() else {
        return false;
    };
    if binders(tree, parent).is_some_and(|binders| binders.spec == Some(list)) {
        return true;
    }
    tree.node(parent).parent().is_some_and(|grandparent| {
        binders(tree, grandparent)
            .is_some_and(|binders| binders.nested && binders.spec == Some(parent))
    })
}

/// Whether LIST is a call: a list that is neither quoted data nor where a
/// binding form names what it binds.
pub(super) fn calls(tree: &SyntaxTree, list: NodeId) -> bool {
    is_list(tree, list) && !quoted(tree, list) && !is_binding_list(tree, list)
}

/// Whether ID names what is called: the first form of a list that
/// [`calls`], or the name after `#'`.
pub(super) fn is_call_head(tree: &SyntaxTree, id: NodeId) -> bool {
    let Some(parent) = tree.node(id).parent() else {
        return false;
    };
    match tree.node(parent).kind() {
        NodeKind::List { .. } => tree.forms(parent).next() == Some(id) && calls(tree, parent),
        NodeKind::Prefix {
            prefix: Prefix::Function,
        } => true,
        NodeKind::Prefix {
            prefix: Prefix::Quote | Prefix::Backquote | Prefix::Comma | Prefix::Splice,
        } => false,
        NodeKind::Atom(_) | NodeKind::Dot | NodeKind::Comment | NodeKind::Error => false,
    }
}

/// Whether ID is a name a binding form binds, as `locals_at` reads them: the
/// same forms and positions, wherever the cursor is. `&optional` and `&rest`,
/// `nil` and anything quoted are not.
pub(super) fn is_binding_site(tree: &SyntaxTree, id: NodeId) -> bool {
    if !is_symbol(tree, id) || quoted(tree, id) {
        return false;
    }
    let mut at = tree.node(id).parent();
    while let Some(list) = at {
        if binders(tree, list).is_some_and(|binders| binders.names.contains(&id)) {
            return true;
        }
        at = tree.node(list).parent();
    }
    false
}

/// The local variables in scope at OFFSET, outermost binding first, given PATH,
/// the nodes that hold OFFSET as [`SyntaxTree::path_at`] finds them. The forms
/// that bind them are those [`binders`] knows.
pub(super) fn locals_at(tree: &SyntaxTree, path: &[NodeId], offset: usize) -> Vec<Local> {
    let mut locals = Vec::new();
    for &list in path {
        if quoted(tree, list) {
            continue;
        }
        let Some(binders) = binders(tree, list) else {
            continue;
        };
        let in_scope = tree
            .forms(list)
            .nth(binders.scope_after)
            .is_some_and(|form| passed(tree, form, offset));
        if in_scope {
            locals.extend(binders.names.iter().map(|&name| Local::new(tree, name)));
            continue;
        }
        if binders.sequential
            && let Some(bindings) = binders.spec
            && tree.node(bindings).range().start < offset
        {
            let seen = tree
                .forms(bindings)
                .filter(|&binding| passed(tree, binding, offset))
                .filter_map(|binding| binding_name(tree, binding))
                .filter(|name| binders.names.contains(name));
            locals.extend(seen.map(|name| Local::new(tree, name)));
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
