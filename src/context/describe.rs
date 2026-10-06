//! What a context knows about each name, for editor tools.

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

/// The signature a value itself shows: an arity, or a Lisp parameter
/// list's names.
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

impl TulispContext {
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
        let (kind, signature) = match &value {
            Some(value) => {
                let value = &value.inner_ref().0;
                (kind_of(value), derived_signature(value))
            }
            None => (SymbolKind::Variable, None),
        };
        Some(SymbolInfo::new(kind, signature, None))
    }

    /// Every name that has a value or was declared with `defvar`, with
    /// what [`describe`](Self::describe) says of it, in no particular
    /// order. Symbols that were only interned, and keywords, are left out.
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
        assert_eq!(rendered(&ctx, "rust-fn"), "(rust-fn ARG &optional ARG)");
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
}
