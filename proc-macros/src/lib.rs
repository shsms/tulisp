mod parse_args;

use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{parse_macro_input, AttributeArgs};
use syn::{ItemFn, ReturnType};

fn gen_function_call(
    crate_name: &TokenStream2,
    fn_name: &Ident,
    params_for_call: TokenStream2,
    ret_type: &ReturnType,
) -> Result<TokenStream2, TokenStream> {
    let ret_type_err = |tt: &Box<syn::Type>| {
        const ALLOWED_TYPES: &str = "i64, f64, String or TulispValue";
        Err(syn::Error::new(
            tt.__span(),
            format!(
                r#"Expected return types: {}, or Result<"{}", tulisp::error::Error>, but got: {}"#,
                ALLOWED_TYPES,
                ALLOWED_TYPES,
                tt.to_token_stream().to_string()
            ),
        )
        .to_compile_error()
        .into())
    };

    let call_and_ret = match ret_type {
        syn::ReturnType::Default => {
            quote! {#fn_name(#params_for_call); Ok(#crate_name::Nil)}
        }
        syn::ReturnType::Type(_, tt) => match &**tt {
            syn::Type::Path(tp) => {
                let seg = tp.path.segments.last().unwrap();
                let ret_str = seg.ident.to_string();
                match ret_str.as_str() {
                    "i64" | "f64" | "bool" | "String" | "TulispValueRef" => {
                        quote! {Ok(#fn_name(#params_for_call).into())}
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
                                "i64" | "f64" | "bool" | "String" | "TulispValueRef" => {
                                    quote! {#fn_name(#params_for_call).map(|x| x.into())}
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
    let (arg_extract_stmts, params_for_call) =
        match parse_args::parse_args(&inp, &crate_name, &tulisp_fn_name, eval_args) {
            Ok(vv) => vv,
            Err(e) => return e,
        };
    let call_and_ret =
        match gen_function_call(&crate_name, fn_name, params_for_call, &inp.sig.output) {
            Ok(vv) => vv,
            Err(e) => return e,
        };

    let mut generated = quote! {
        fn #fn_name(
            ctx: &mut #crate_name::context::TulispContext,
            __tulisp_internal_value: &#crate_name::TulispValueRef,
        ) -> Result<#crate_name::TulispValueRef, #crate_name::error::Error> {
            use #crate_name::error::Error;

            #inp

            let __defun_name = #crate_name::car(__tulisp_internal_value)?;
            let __tulisp_internal_value = #crate_name::cdr(__tulisp_internal_value)?;

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
            // TODO: how to avoid unwrap?
            #ctx.set_str(#tulisp_fn_name.to_string(), #crate_name::context::ContextObject::#ctxobj_type(#fn_name)).unwrap();
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
