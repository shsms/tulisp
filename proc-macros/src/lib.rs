mod parse_args;

use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{parse_macro_input, parse_quote, AttributeArgs};
use syn::{ItemFn, ReturnType};

fn gen_function_call(
    fn_name: &Ident,
    params_for_call: TokenStream2,
    self_param: &TokenStream2,
    ret_type: &ReturnType,
) -> Result<TokenStream2, TokenStream> {
    let ret_type_err = |tt: &Box<syn::Type>| {
        const ALLOWED_TYPES: &str = "i64, f64, String, Rc<dyn Any>, TulispValue";
        Err(syn::Error::new(
            tt.__span(),
            format!(
                r#"Expected return types: {}, or Result<"{}", tulisp::Error>, but got: {}"#,
                ALLOWED_TYPES,
                ALLOWED_TYPES,
                tt.to_token_stream().to_string()
            ),
        )
        .to_compile_error()
        .into())
    };

    let self_prefix = if self_param.is_empty() {
        quote!()
    } else {
        quote!(self.)
    };

    let call_and_ret = match ret_type {
        syn::ReturnType::Default => {
            quote! {#self_prefix #fn_name(#params_for_call); Ok(TulispValue::nil())}
        }
        syn::ReturnType::Type(_, tt) => match &**tt {
            syn::Type::Path(tp) => {
                let seg = tp.path.segments.last().unwrap();
                let ret_str = seg.ident.to_string();
                match ret_str.as_str() {
                    "i64" | "f64" | "bool" | "String" | "TulispValue" => {
                        quote! {Ok(#self_prefix #fn_name(#params_for_call).into())}
                    }
                    "Result" => match &seg.arguments {
                        syn::PathArguments::AngleBracketed(agbkt) => {
                            let err_type_str =
                                agbkt.args.last().unwrap().to_token_stream().to_string();
                            if err_type_str != "Error" && err_type_str != "tulisp :: error :: Error"
                            {
                                return ret_type_err(tt);
                            }
                            let inner_str =
                                agbkt.args.first().unwrap().to_token_stream().to_string();
                            match inner_str.as_str() {
                                "i64" | "f64" | "bool" | "String" | "TulispValue"
                                | "Rc < dyn Any >" => {
                                    quote! {#self_prefix #fn_name(#params_for_call).map(|x| x.into())}
                                }
                                _ => {
                                    return ret_type_err(tt);
                                }
                            }
                        }
                        _ => {
                            return ret_type_err(tt);
                        }
                    },
                    "Rc" => match &seg.arguments {
                        syn::PathArguments::AngleBracketed(agbkt) => {
                            let inner_str =
                                agbkt.args.first().unwrap().to_token_stream().to_string();
                            if inner_str == "dyn Any" {
                                quote! {Ok(#self_prefix #fn_name(#params_for_call).into())}
                            } else {
                                return ret_type_err(tt);
                            }
                        }
                        _ => {
                            return ret_type_err(tt);
                        }
                    },
                    _ => {
                        return ret_type_err(tt);
                    }
                }
            }
            _ => {
                return ret_type_err(tt);
            }
        },
    };

    Ok(call_and_ret)
}

fn make_generated_fn_name(fn_name: &TokenStream2) -> TokenStream2 {
    let fn_name = fn_name.to_string();
    let parts = fn_name.rsplit_once('.');
    let (prefix, fn_name) = if let Some((a, b)) = parts {
        let a = syn::Ident::new(format!("{a}").as_str(), proc_macro2::Span::call_site());
        (quote!(#a .), b.to_string())
    } else {
        (quote!(), fn_name)
    };
    let ident = syn::Ident::new(
        format!("__tulisp_generated_{fn_name}").as_str(),
        proc_macro2::Span::call_site(),
    );

    quote! {
        #prefix #ident
    }
}

fn tulisp_fn_impl(
    attr: TokenStream,
    item: TokenStream,
    crate_name: TokenStream2,
    eval_args: bool,
) -> TokenStream {
    let inp = parse_macro_input!(item as ItemFn);
    let fn_name = &inp.sig.ident;
    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let mut add_to_ctx = None;
    let mut fn_name_in_ctx = None;
    for attr in attr_args {
        match attr {
            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                let path = nv.path.to_token_stream().to_string();
                let val: TokenStream2 = match nv.lit {
                    syn::Lit::Str(ref vv) => vv.parse().unwrap(), // ("ctx must have an identifier as value."),
                    _ => panic!("attributes must have string values"),
                };
                match path.as_str() {
                    "add_func" => add_to_ctx = Some((val, false)),
                    "add_macro" => add_to_ctx = Some((val, true)),
                    "name" => fn_name_in_ctx = Some(nv.lit),
                    e => panic!("unknown attribute: {}", e),
                }
            }
            _ => panic!("attributes have to be a=,"),
        }
    }
    let tulisp_fn_name = fn_name_in_ctx.unwrap_or_else(|| {
        syn::Lit::Str(syn::LitStr::new(
            &fn_name.to_string(),
            proc_macro2::Span::call_site(),
        ))
    });
    let (arg_extract_stmts, params_for_call, self_param) =
        match parse_args::parse_args(&inp, &crate_name, &tulisp_fn_name, eval_args) {
            Ok(vv) => vv,
            Err(e) => return e,
        };
    let call_and_ret =
        match gen_function_call(fn_name, params_for_call, &self_param, &inp.sig.output) {
            Ok(vv) => vv,
            Err(e) => return e,
        };
    let generated_fn_name = make_generated_fn_name(&fn_name.to_token_stream());
    let mut generated = quote! {
        #inp

        fn #generated_fn_name(
            #self_param
            ctx: &mut #crate_name::TulispContext,
            __tulisp_internal_value: &#crate_name::TulispValue,
        ) -> Result<#crate_name::TulispValue, #crate_name::Error> {
            use #crate_name::Error;


            let __defun_name = __tulisp_internal_value.car()?;
            let __tulisp_internal_value = __tulisp_internal_value.cdr()?;

            #arg_extract_stmts
            #call_and_ret
        }
    };

    if let Some((ctx, add_macro)) = add_to_ctx {
        let ctx = Ident::new(
            &ctx.to_token_stream().to_string(),
            proc_macro2::Span::call_site(),
        );
        let ctxobj_type = if add_macro {
            quote! {Macro}
        } else {
            quote! {Func}
        };
        generated.extend(quote! {
            let tfunc = #ctx.intern(#tulisp_fn_name);
            tfunc.set_scope(
                #crate_name::TulispValueEnum::#ctxobj_type(std::rc::Rc::new(#generated_fn_name))
                .into_ref()
            ).unwrap();
        })
    }

    generated.into()
}

#[proc_macro_attribute]
pub fn tulisp_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    tulisp_fn_impl(attr, item, quote!(tulisp), true)
}

#[proc_macro_attribute]
pub fn tulisp_fn_no_eval(attr: TokenStream, item: TokenStream) -> TokenStream {
    tulisp_fn_impl(attr, item, quote!(tulisp), false)
}

#[proc_macro_attribute]
pub fn crate_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    tulisp_fn_impl(attr, item, quote!(crate), true)
}

#[proc_macro_attribute]
pub fn crate_fn_no_eval(attr: TokenStream, item: TokenStream) -> TokenStream {
    tulisp_fn_impl(attr, item, quote!(crate), false)
}

fn tulisp_add_impl(input: TokenStream, crate_name: TokenStream2, add_macro: bool) -> TokenStream {
    let ctxobj_type = if add_macro {
        quote!(Macro)
    } else {
        quote!(Func)
    };
    let mut tokens = vec![quote!()];
    let input: TokenStream2 = input.clone().into();
    for tok in input.clone() {
        if tok.to_string() == "," {
            tokens.push(quote!());
            continue;
        }
        tokens.last_mut().map(|x| x.extend(quote!(#tok)));
    }
    let (ctx, fn_name, tulisp_fn_name) = match &tokens[..] {
        [a, b, c] => {
            let c: syn::Lit = parse_quote!(#c);
            let c = match c {
                syn::Lit::Str(c) => c.value(),
                _ => {
                    return syn::Error::new(c.span(), "expected string literal for tulisp_fn_name")
                        .to_compile_error()
                        .into()
                }
            };
            (a, b, c)
        }
        [a, b] => (a, b, b.to_string()),
        _ => {
            return syn::Error::new(input.__span(), "incorrect number of arguments")
                .to_compile_error()
                .into()
        }
    };

    let generated_fn_name = make_generated_fn_name(&fn_name);
    let generated = quote! {
        let tfunc = #ctx.intern(#tulisp_fn_name);
        tfunc.set_scope(
            #crate_name::TulispValueEnum::#ctxobj_type(
                std::rc::Rc::new(move |_1, _2|#generated_fn_name(_1, _2))
            ).into_ref()
        ).unwrap();
    };

    generated.into()
}

#[proc_macro]
pub fn tulisp_add_func(input: TokenStream) -> TokenStream {
    tulisp_add_impl(input, quote!(tulisp), false)
}

#[proc_macro]
pub fn tulisp_add_macro(input: TokenStream) -> TokenStream {
    tulisp_add_impl(input, quote!(tulisp), true)
}

#[proc_macro]
pub fn crate_add_func(input: TokenStream) -> TokenStream {
    tulisp_add_impl(input, quote!(crate), false)
}

#[proc_macro]
pub fn crate_add_macro(input: TokenStream) -> TokenStream {
    tulisp_add_impl(input, quote!(crate), true)
}
