use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::parse_macro_input;
use syn::{FnArg, ItemFn, PatType, ReturnType};

fn parse_args(
    inp: &ItemFn,
    crate_name: &TokenStream2,
    fn_name: &Ident,
    eval_args: bool,
) -> Result<(TokenStream2, TokenStream2), TokenStream> {
    let mut arg_extract_stmts = quote!();
    let mut params_for_call = quote!();
    let mut have_rest_var = false;
    let mut arg_pos = -1;
    let mut ctx_var_name = None;

    let args = &inp.sig.inputs;
    for arg in args.iter() {
        arg_pos += 1;
        if have_rest_var {
            return Err(syn::Error::new(
                arg.__span(),
                "rest: TulispValueRef has to be the last argument.",
            )
            .to_compile_error()
            .into());
        }
        match arg {
            FnArg::Receiver(_) => {
                panic!("Tulisp functions can't have `self` receivers.");
            }
            FnArg::Typed(arg) => {
                let PatType { pat, ty, .. } = arg;
                let ty_str = ty.to_token_stream().to_string();
                params_for_call.extend(quote! {#pat , });

                if pat.to_token_stream().to_string() == "rest" && ty_str == "TulispValueRef" {
                    have_rest_var = true;
                    if eval_args {
                        arg_extract_stmts.extend(quote! {
                            let #pat = #crate_name::eval::eval_each(tulisp_context, value)?;
                            let value = TulispValue::Nil;
                        })
                    } else {
                        arg_extract_stmts.extend(quote! {
                            let #pat = value;
                            let value = TulispValue::Nil;
                        })
                    }
                    continue;
                }
                if eval_args {
                    arg_extract_stmts.extend(quote!{
                        let __tulisp_internal_car = #crate_name::eval::eval(tulisp_context, #crate_name::cons::car(value.clone())?)?;
                    })
                } else {
                    arg_extract_stmts.extend(quote! {
                        let __tulisp_internal_car =  #crate_name::cons::car(value.clone())?;
                    })
                }
                let ty_extractor = match ty_str.as_str() {
                    "i64" => quote! {.as_int()?},
                    "f64" => quote! {.as_float()?},
                    "String" => quote! {.as_string()?},
                    "TulispValueRef" => quote!(),
                    "& mut TulispContext" => {
                        if arg_pos == 0 {
                            ctx_var_name = Some(pat);
                            continue;
                        }
                        return Err(syn::Error::new(
                            ty.__span(),
                            "TulispContext has to the first argument.",
                        )
                        .to_compile_error()
                        .into());
                    }
                    _ => {
                        return Err(syn::Error::new(
                            ty.__span(),
                            format!("Unknown type: {}", ty.to_token_stream().to_string()),
                        )
                        .to_compile_error()
                        .into());
                    }
                };

                arg_extract_stmts.extend(quote! {
                    let #pat = __tulisp_internal_car #ty_extractor;
                    let value = #crate_name::cons::cdr(value)?;
                });
            }
        };
    }

    arg_extract_stmts.extend(quote! {
        if !value.is_null() {
            return Err(#crate_name::error::Error::new(
                #crate_name::error::ErrorKind::TypeMismatch,
                format!("{}: Too many arguments", stringify!{#fn_name})));
        }
    });

    if let Some(pat) = ctx_var_name {
        arg_extract_stmts.extend(quote!{
            let #pat = tulisp_context;
        })
    }

    return Ok((arg_extract_stmts, params_for_call));
}

fn gen_function_call(
    fn_name: &Ident,
    params_for_call: TokenStream2,
    ret_type: &ReturnType,
) -> Result<TokenStream2, TokenStream> {
    let ret_type_err = |tt: &Box<syn::Type>| {
        const ALLOWED_TYPES: &str = "i64, f64, String or TulispValue";
        Err(syn::Error::new(
            tt.__span(),
            format!(
                "Expected return types: {}, or Result<\"{}\", tulisp::error::Error>, but got: {}",
                ALLOWED_TYPES,
                ALLOWED_TYPES,
                tt.to_token_stream().to_string()
            ),
        )
        .to_compile_error()
        .into())
    };

    let call_and_ret = match ret_type {
        syn::ReturnType::Default => quote! {#fn_name(#params_for_call); Ok(TulispValue::Nil)},
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
    _attr: TokenStream,
    item: TokenStream,
    crate_name: TokenStream2,
    eval_args: bool,
) -> TokenStream {
    let inp = parse_macro_input!(item as ItemFn);
    let fn_name = &inp.sig.ident;

    let (arg_extract_stmts, params_for_call) =
        match parse_args(&inp, &crate_name, &fn_name, eval_args) {
            Ok(vv) => vv,
            Err(e) => return e,
        };
    let call_and_ret = match gen_function_call(fn_name, params_for_call, &inp.sig.output) {
        Ok(vv) => vv,
        Err(e) => return e,
    };

    let ret = quote! {
        fn #fn_name(
            tulisp_context: &mut #crate_name::context::TulispContext,
            value: #crate_name::value_ref::TulispValueRef,
        ) -> Result<#crate_name::value_ref::TulispValueRef, #crate_name::error::Error> {
            use #crate_name::error::Error;

            #inp

            let __defun_name = #crate_name::cons::car(value.clone())?;
            let value = #crate_name::cons::cdr(value.clone())?;

            #arg_extract_stmts
            #call_and_ret
        }
    };

    ret.into()
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
