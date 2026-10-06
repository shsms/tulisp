//! What a context knows about each name, for editor tools.

use std::borrow::Cow;

use crate::symbols::{ParamPosition, Signature, SignatureParam, SymbolInfo, SymbolKind};
use crate::{TulispContext, TulispObject, TulispValue};

/// The kind of name a value makes.
pub(crate) fn kind_of(value: &TulispValue) -> SymbolKind {
    match value {
        TulispValue::Defun { .. } | TulispValue::CompiledDefun { .. } => SymbolKind::Function,
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => SymbolKind::Macro,
        TulispValue::Special { .. } | TulispValue::SpecialForm => SymbolKind::SpecialForm,
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce => SymbolKind::Variable,
    }
}

/// The identity of a function value, by the address of its shared parts, or
/// `None` for a value that is not a function. An address can be reused once the
/// old value is freed, so a stale match is possible, but it needs that exact
/// reuse.
pub(crate) fn value_key(value: &TulispValue) -> Option<usize> {
    match value {
        TulispValue::Defun { call, .. } => Some(call.addr_as_usize()),
        TulispValue::Special { call, .. } => Some(call.addr_as_usize()),
        TulispValue::Macro(func) => Some(func.addr_as_usize()),
        TulispValue::CompiledDefun { value } => Some(value.code_addr()),
        TulispValue::Defmacro { compiled, .. } => Some(compiled.addr_as_usize()),
        TulispValue::SpecialForm => Some(0),
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce => None,
    }
}

/// The signature a value itself shows: an arity, or a Lisp parameter list's
/// names.
pub(crate) fn derived_signature(value: &TulispValue) -> Option<Signature> {
    match value {
        TulispValue::Defun { arity, .. } | TulispValue::Special { arity, .. } => {
            Some(Signature::from_arity(arity))
        }
        TulispValue::CompiledDefun { value } => {
            let (required, optional, rest) = value.params();
            let param = |p: &TulispObject, position| SignatureParam {
                name: p.as_symbol().ok(),
                position,
                type_name: None,
            };
            let mut params: Vec<SignatureParam> = required
                .iter()
                .map(|p| param(p, ParamPosition::Required))
                .collect();
            params.extend(optional.iter().map(|p| param(p, ParamPosition::Optional)));
            params.extend(rest.map(|p| param(p, ParamPosition::Rest)));
            Some(Signature { params })
        }
        TulispValue::Defmacro { lambda, .. } => {
            let params = lambda.cadr().ok()?;
            let names: Vec<String> = params
                .base_iter()
                .filter_map(|p| p.as_symbol().ok())
                .collect();
            Some(Signature::from_lambda_list(
                names.iter().map(String::as_str),
            ))
        }
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Macro(_)
        | TulispValue::SpecialForm => None,
    }
}

/// What the context records of a name beyond its value: the signature a
/// Rust registration declared, and a docstring. `describe` uses it only
/// while the name still holds the value it was recorded for.
pub(crate) struct DocEntry {
    pub(crate) kind: SymbolKind,
    /// The `value_key` of the value the entry describes; `None` for a
    /// variable's entry, which describes the variable whatever value it
    /// holds.
    pub(crate) key: Option<usize>,
    pub(crate) signature: Option<Signature>,
    pub(crate) doc: Option<Cow<'static, str>>,
}

impl TulispContext {
    /// Sets, or with `None` removes, the entry for SYM. Only a symbol
    /// interned under its name has one: describe looks names up in the
    /// obarray.
    pub(crate) fn set_doc_entry(&mut self, sym: &TulispObject, entry: Option<DocEntry>) {
        let Ok(name) = sym.as_symbol() else {
            return;
        };
        if !self
            .obarray
            .get(&name)
            .is_some_and(|interned| interned.eq_ptr(sym))
        {
            return;
        }
        match entry {
            Some(entry) => {
                self.docs.insert(name, entry);
            }
            None => {
                self.docs.remove(&name);
            }
        }
    }

    /// What NAME holds, its signature and its docstring, for editor
    /// tools. `None` when NAME has no value and was not declared with
    /// `defvar`, or is a keyword. It does not intern NAME.
    pub fn describe(&self, name: &str) -> Option<SymbolInfo> {
        if name.starts_with(':') {
            return None;
        }
        let sym = self.obarray.get(name)?;
        let value = sym.global();
        if value.is_none() && !sym.is_special() {
            return None;
        }
        let (kind, key, signature) = match &value {
            Some(value) => {
                let value = &value.inner_ref().0;
                (kind_of(value), value_key(value), derived_signature(value))
            }
            None => (SymbolKind::Variable, None, None),
        };
        let entry = self
            .docs
            .get(name)
            .filter(|entry| entry.kind == kind && (entry.key.is_none() || entry.key == key));
        let signature = entry
            .and_then(|entry| entry.signature.clone())
            .or(signature);
        let doc = entry.and_then(|entry| entry.doc.as_deref().map(str::to_string));
        Some(SymbolInfo::new(kind, signature, doc))
    }

    /// Every name that has a value or was declared with `defvar`, with what
    /// [`describe`](Self::describe) says of it, in no particular order. Symbols
    /// that were only interned, and keywords, are left out.
    pub fn symbols(&self) -> impl Iterator<Item = (&str, SymbolInfo)> + '_ {
        self.obarray
            .keys()
            .filter_map(|name| Some((name.as_str(), self.describe(name)?)))
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::symbols::SymbolKind;

    fn rendered(ctx: &TulispContext, name: &str) -> String {
        let info = ctx
            .describe(name)
            .unwrap_or_else(|| panic!("{name} is not defined"));
        let signature = info
            .signature
            .unwrap_or_else(|| panic!("{name} has no signature"));
        signature.render(name)
    }

    #[test]
    fn describe_reports_each_kind() {
        let mut ctx = TulispContext::new();
        ctx.defun("rust-fn", |a: i64, b: Option<i64>| a + b.unwrap_or(0));
        ctx.eval_string(
            "(defun lisp-fn (x &optional y &rest z) x)
             (defmacro lisp-mac (a) a)
             (defvar some-var 1)
             (defvar declared-var)",
        )
        .unwrap();
        let kind = |name: &str| ctx.describe(name).map(|info| info.kind);
        assert_eq!(kind("rust-fn"), Some(SymbolKind::Function));
        assert_eq!(kind("lisp-fn"), Some(SymbolKind::Function));
        assert_eq!(kind("lisp-mac"), Some(SymbolKind::Macro));
        assert_eq!(kind("if"), Some(SymbolKind::SpecialForm));
        assert_eq!(kind("some-var"), Some(SymbolKind::Variable));
        assert_eq!(kind("declared-var"), Some(SymbolKind::Variable));
        assert_eq!(kind("never-defined"), None);
    }

    #[test]
    fn lisp_definitions_have_their_parameter_names() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            "(defun lisp-fn (x &optional y &rest z) x) (defmacro lisp-mac (a &rest b) a)",
        )
        .unwrap();
        assert_eq!(rendered(&ctx, "lisp-fn"), "(lisp-fn X &optional Y &rest Z)");
        assert_eq!(rendered(&ctx, "lisp-mac"), "(lisp-mac A &rest B)");
    }

    #[test]
    fn a_rust_function_has_its_arity() {
        let mut ctx = TulispContext::new();
        ctx.defun("rust-fn", |a: i64, b: Option<i64>| a + b.unwrap_or(0));
        assert_eq!(
            rendered(&ctx, "rust-fn"),
            "(rust-fn INTEGER &optional INTEGER)"
        );
    }

    #[test]
    fn a_name_setq_over_a_function_is_a_variable() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", || 1);
        ctx.eval_string("(setq f 5)").unwrap();
        let info = ctx.describe("f").unwrap();
        assert_eq!(info.kind, SymbolKind::Variable);
        assert_eq!(info.signature, None);
    }

    #[test]
    fn symbols_lists_bound_and_declared_names_only() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar declared-var) '(only-interned)")
            .unwrap();
        let names: Vec<&str> = ctx.symbols().map(|(name, _)| name).collect();
        assert!(names.contains(&"declared-var"));
        assert!(names.contains(&"car"));
        assert!(names.contains(&"if"));
        assert!(!names.contains(&"only-interned"));
        assert!(!names.iter().any(|name| name.starts_with(':')));
    }

    #[test]
    fn describe_does_not_intern() {
        let ctx = TulispContext::new();
        let before = ctx.obarray.len();
        assert_eq!(ctx.describe("not-a-name-yet"), None);
        assert_eq!(ctx.obarray.len(), before);
    }

    crate::AsList! {
        struct Cfg {
            a: i64,
        }
    }

    #[derive(Clone)]
    struct Handle;

    impl std::fmt::Display for Handle {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "#<handle>")
        }
    }

    impl crate::TulispAny for Handle {}

    #[test]
    fn rust_parameters_have_lisp_type_names() {
        use crate::{Number, Rest, TulispObject};
        let mut ctx = TulispContext::new();
        ctx.defun(
            "typed",
            |_a: i64,
             _b: f64,
             _c: String,
             _d: Number,
             _e: Vec<i64>,
             _f: TulispObject,
             _g: Rest<String>| {},
        );
        assert_eq!(
            rendered(&ctx, "typed"),
            "(typed INTEGER NUMBER STRING NUMBER LIST ARG &rest STRING)"
        );
    }

    #[test]
    fn a_keyword_tail_shows_as_keywords() {
        let mut ctx = TulispContext::new();
        ctx.defun("cfg", |scale: i64, c: crate::Plist<Cfg>| scale + c.a);
        assert_eq!(rendered(&ctx, "cfg"), "(cfg INTEGER &key ARG)");
    }

    #[test]
    fn a_host_type_parameter_has_its_type_name() {
        let mut ctx = TulispContext::new();
        ctx.defun("h", |_h: Handle| {});
        assert_eq!(rendered(&ctx, "h"), "(h HANDLE)");
    }

    #[test]
    fn a_special_form_from_rust_has_its_parameters() {
        let mut ctx = TulispContext::new();
        ctx.defspecial("sp", |_form: crate::Form, b: i64| b);
        assert_eq!(rendered(&ctx, "sp"), "(sp ARG INTEGER)");
    }

    #[test]
    fn registering_again_replaces_the_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.defun("f", |s: String| s);
        assert_eq!(rendered(&ctx, "f"), "(f STRING)");
    }

    #[test]
    fn fmakunbound_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("gone", |a: i64| a);
        assert!(ctx.docs.contains_key("gone"));
        ctx.fmakunbound("gone").unwrap();
        assert!(!ctx.docs.contains_key("gone"));
    }

    #[test]
    fn a_lambda_set_over_a_rust_function_has_its_own_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.eval_string("(setq f (lambda (x) x))").unwrap();
        assert_eq!(rendered(&ctx, "f"), "(f X)");
    }

    #[test]
    fn fset_and_defmacro_replace_a_rust_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("g", |a: i64| a);
        let lambda = ctx.eval_string("(lambda (y) y)").unwrap();
        ctx.fset("g", lambda).unwrap();
        assert_eq!(rendered(&ctx, "g"), "(g Y)");
        ctx.defun("h", |a: i64| a);
        ctx.eval_string("(defmacro h (z) z)").unwrap();
        assert_eq!(rendered(&ctx, "h"), "(h Z)");
    }

    #[test]
    fn a_zero_parameter_function_has_an_empty_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("z", || 1);
        ctx.defspecial("zs", || 1);
        assert_eq!(rendered(&ctx, "z"), "(z)");
        assert_eq!(rendered(&ctx, "zs"), "(zs)");
    }
}
