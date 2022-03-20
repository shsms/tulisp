use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{parse_macro_input, AttributeArgs};
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

    let type_err = |tt: &Box<syn::Type>| {
        const ALLOWED_TYPES: &str = "i64, f64, String or TulispValueRef";
        Err(syn::Error::new(
            tt.__span(),
            format!(
                "Expected arg types types: {}, but got: {}",
                ALLOWED_TYPES,
                tt.to_token_stream().to_string()
            ),
        )
        .to_compile_error()
        .into())
    };
    let mut has_optional = false;
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
        let mut optional = false;
        match arg {
            FnArg::Receiver(_) => {
                panic!("Tulisp functions can't have `self` receivers.");
            }
            FnArg::Typed(arg) => {
                let PatType { pat, ty, .. } = arg;
                let ty_str = match &**ty {
                    syn::Type::Path(tp) => {
                        let sgmt = tp.path.segments.last().unwrap();
                        let tp_str = sgmt.ident.to_string();
                        let out_tp = if tp_str != "Option" {
                            tp_str
                        } else {
                            has_optional = true;
                            optional = true;
                            match &sgmt.arguments {
                                syn::PathArguments::AngleBracketed(b) => {
                                    b.args.last().to_token_stream().to_string()
                                }
                                _ => return type_err(ty),
                            }
                        };
                        out_tp
                    }
                    syn::Type::Reference(tr) => {
                        String::from("&")
                            + (&tr.mutability.and(Some("mut ")).unwrap_or(" ")).to_owned()
                            + match &*tr.elem {
                                syn::Type::Path(tp) => tp
                                    .path
                                    .segments
                                    .last()
                                    .unwrap()
                                    .to_token_stream()
                                    .to_string(),
                                _ => return type_err(ty),
                            }
                            .as_str()
                    }
                    _ => todo!(),
                };
                params_for_call.extend(quote! {#pat , });

                if pat.to_token_stream().to_string() == "rest"
                    && ty_str == "TulispValueRef"
                    && !optional
                {
                    have_rest_var = true;
                    if eval_args {
                        arg_extract_stmts.extend(quote! {
                            let #pat = #crate_name::eval::eval_each(__tulisp_internal_context, __tulisp_internal_value)?;
                            let __tulisp_internal_value = #crate_name::value::TulispValue::Nil;
                        })
                    } else {
                        arg_extract_stmts.extend(quote! {
                            let #pat = __tulisp_internal_value;
                            let __tulisp_internal_value = #crate_name::value::TulispValue::Nil;
                        })
                    }
                    continue;
                }

                if has_optional && !optional {
                    return Err(syn::Error::new(
                        ty.__span(),
                        "Can't have non-optional arguments following an optional argument.",
                    )
                    .to_compile_error()
                    .into());
                }

                let ty_extractor = match ty_str.as_str() {
                    "i64" | "f64" | "String" | "bool" => {
                        if optional {
                            quote! {(|x: #crate_name::value_ref::TulispValueRef|if x == #crate_name::value::TulispValue::Nil { Ok(None)} else {Ok(Some(x.try_into()?))})}
                        } else {
                            quote! {(|x: #crate_name::value_ref::TulispValueRef| x.try_into())}
                        }
                    }
                    "TulispValueRef" => {
                        if optional {
                            quote! {(|x: #crate_name::value_ref::TulispValueRef|if x == #crate_name::value::TulispValue::Nil { Ok(None)} else {Ok(x.into())})}
                        } else {
                            quote! {(|x: #crate_name::value_ref::TulispValueRef| Ok(x.into()))}
                        }
                    }
                    "&mut TulispContext" => {
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

                // TODO: if optional, get car only if is_list
                if eval_args {
                    arg_extract_stmts.extend(quote!{
                        let __tulisp_internal_car = __tulisp_internal_context.eval(#crate_name::cons::car(__tulisp_internal_value.clone())?)?;
                    })
                } else {
                    arg_extract_stmts.extend(quote! {
                        let __tulisp_internal_car =  #crate_name::cons::car(__tulisp_internal_value.clone())?;
                    })
                }

                arg_extract_stmts.extend(quote! {
                    let #pat = #ty_extractor(__tulisp_internal_car)? ;
                    let __tulisp_internal_value = #crate_name::cons::cdr(__tulisp_internal_value)?;
                });
            }
        };
    }

    arg_extract_stmts.extend(quote! {
        if !__tulisp_internal_value.is_null() {
            return Err(#crate_name::error::Error::new(
                #crate_name::error::ErrorKind::TypeMismatch,
                format!("{}: Too many arguments: {}", stringify!{#fn_name}, __tulisp_internal_value)));
        }
    });

    if let Some(pat) = ctx_var_name {
        arg_extract_stmts.extend(quote! {
            let #pat = __tulisp_internal_context;
        })
    }

    return Ok((arg_extract_stmts, params_for_call));
}

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
        syn::ReturnType::Default => {
            quote! {#fn_name(#params_for_call); Ok(#crate_name::value::TulispValue::Nil)}
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
    let (arg_extract_stmts, params_for_call) =
        match parse_args(&inp, &crate_name, &fn_name, eval_args) {
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
            __tulisp_internal_context: &mut #crate_name::context::TulispContext,
            __tulisp_internal_value: #crate_name::value_ref::TulispValueRef,
        ) -> Result<#crate_name::value_ref::TulispValueRef, #crate_name::error::Error> {
            use #crate_name::error::Error;

            #inp

            let __defun_name = #crate_name::cons::car(__tulisp_internal_value.clone())?;
            let __tulisp_internal_value = #crate_name::cons::cdr(__tulisp_internal_value.clone())?;

            #arg_extract_stmts
            #call_and_ret
        }
    };

    if let Some((ctx, add_macro)) = add_to_ctx {
        let ctx = Ident::new(
            &ctx.to_token_stream().to_string(),
            proc_macro2::Span::call_site(),
        );
        let name = fn_name_in_ctx.unwrap_or_else(|| {
            syn::Lit::Str(syn::LitStr::new(
                &fn_name.to_string(),
                proc_macro2::Span::call_site(),
            ))
        });
        let ctxobj_type = if add_macro {
            quote! {Macro}
        } else {
            quote! {Func}
        };
        generated.extend(quote! {
            // TODO: how to avoid unwrap?
            #ctx.set_str(#name.to_string(), #crate_name::context::ContextObject::#ctxobj_type(#fn_name)).unwrap();
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
