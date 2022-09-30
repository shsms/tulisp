use proc_macro::{self, TokenStream};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{FnArg, ItemFn, PatType};

macro_rules! tulisp_compile_error {
    ($item: expr, $msg: expr) => {
        return Err(syn::Error::new($item.__span(), $msg)
            .to_compile_error()
            .into())
    };
}

struct ArgInfo {
    pos: usize,
    name: TokenStream2,
    /// argument type
    ty: syn::Type,
    /// argument type, with Optional, Iter stripped out
    val_ty: syn::Type,
    optional: bool,
    iter: bool,
    ctx: bool,
    rest: bool,
    has_receiver: bool,
    extract_stmts: TokenStream2,
}

impl ArgInfo {
    // Checks if type is `&mut TulispContext`
    fn is_ctx(&mut self) -> Result<bool, TokenStream> {
        let ref_type = match &self.ty {
            syn::Type::Reference(tr) => tr,
            _ => return Ok(false),
        };

        if ref_type.mutability.is_none() || self.name.to_string() != "ctx" {
            tulisp_compile_error!(self.ty, "Tulisp functions currently support `ctx: &mut TulispContext` as the only reference argument type.");
        }

        let (type_name, _) = parse_type_path(&ref_type.elem)?;

        if type_name != "TulispContext" {
            tulisp_compile_error!(self.ty, "Tulisp functions currently support `ctx: &mut TulispContext` as the only reference argument type.");
        }

        if self.pos > self.has_receiver as usize {
            tulisp_compile_error!(
                self.ty,
                "If a context argument is specified, it has to be the first one."
            );
        }
        self.ctx = true;
        Ok(true)
    }

    fn is_optional(&mut self) -> Result<bool, TokenStream> {
        let (type_name, generic_type) = parse_type_path(&self.ty)?;

        if type_name == "Option" {
            if generic_type.is_none() {
                tulisp_compile_error!(self.ty, "Option must have a generic type");
            }
            self.val_ty = generic_type.unwrap();
            self.optional = true;
        }
        Ok(self.optional)
    }

    fn is_rest(&mut self) -> bool {
        if self.name.to_string() == "rest" && self.ty.to_token_stream().to_string() == "TulispValue"
        {
            self.rest = true;
        }
        self.rest
    }

    fn is_iter(&mut self) -> Result<bool, TokenStream> {
        let (type_name, generic_type) = parse_type_path(&self.val_ty)?;
        if type_name == "Iter" {
            if generic_type.is_none() {
                tulisp_compile_error!(self.val_ty, "Iter parameters must have a generic type.");
            }
            self.iter = true;
        }
        Ok(self.iter)
    }
}

pub(crate) fn parse_args(
    inp: &ItemFn,
    crate_name: &TokenStream2,
    fn_name: &syn::Lit,
    eval_args: bool,
) -> Result<(TokenStream2, TokenStream2, TokenStream2), TokenStream> {
    let mut arg_extract_stmts = quote!();
    let mut params_for_call = quote!();
    let mut self_param = quote!();
    let mut has_optional = false;
    let mut has_rest = false;
    let mut has_receiver = false;
    let args = &inp.sig.inputs;

    for (pos, arg) in args.iter().enumerate() {
        let arg_info = match process_arg(
            crate_name,
            fn_name,
            pos,
            arg,
            eval_args,
            has_optional,
            has_rest,
            has_receiver,
        )? {
            Some(vv) => vv,
            None => {
                has_receiver = true;
                self_param.extend(quote!(#arg, ));
                continue;
            }
        };
        has_optional = arg_info.optional;
        has_rest = arg_info.rest;
        let arg_name = arg_info.name;
        arg_extract_stmts.extend(arg_info.extract_stmts);
        params_for_call.extend(quote! {#arg_name , })
    }
    Ok((arg_extract_stmts, params_for_call, self_param))
}

fn process_arg(
    crate_name: &TokenStream2,
    fn_name: &syn::Lit,
    pos: usize,
    arg: &FnArg,
    eval_args: bool,
    has_optional: bool,
    has_rest: bool,
    has_receiver: bool,
) -> Result<Option<ArgInfo>, TokenStream> {
    let (arg_name, arg_type) = match get_name_and_type(arg)? {
        Some(vv) => vv,
        None => return Ok(None),
    };

    if has_rest {
        tulisp_compile_error!(fn_name, "`rest: TulispValue` has to be the last argument");
    }
    let mut arg_info = ArgInfo {
        pos,
        name: arg_name.clone(),
        ty: arg_type.clone(),
        val_ty: arg_type,
        optional: false,
        iter: false,
        ctx: false,
        rest: false,
        has_receiver,
        extract_stmts: quote!(),
    };
    if arg_info.is_ctx()? {
        return Ok(Some(arg_info));
    }

    if arg_info.is_rest() {
        if eval_args {
            arg_info.extract_stmts.extend(quote! {
                let #arg_name = ctx.eval_each(&__tulisp_internal_value)?;
                let __tulisp_internal_value = TulispValue::nil();
            })
        } else {
            arg_info.extract_stmts.extend(quote! {
                let #arg_name = __tulisp_internal_value;
                let __tulisp_internal_value = TulispValue::nil();
            })
        }
        return Ok(Some(arg_info));
    }

    let optional = arg_info.is_optional()?;

    if has_optional && !optional {
        tulisp_compile_error!(
            arg_info.ty,
            "Can't have non-optional arguments following an optional argument."
        );
    }

    let ty_extractor = if arg_info.is_iter()? {
        if optional {
            quote! {(
                |x: #crate_name::TulispValue| {
                    if x.null() {
                        Ok(None)
                    } else if !x.consp() {
                        Err(#crate_name::Error::new(
                            #crate_name::ErrorKind::TypeMismatch,
                            format!(
                                "In call to {}, arg \"{}\" needs to be a list",
                                stringify!(#fn_name),
                                stringify!(#arg_name)
                            )
                        ).with_span(x.span()))
                    } else {
                        Ok(Some(x.iter()))
                    }
                })
            }
        } else {
            quote! {(
                |x: #crate_name::TulispValue| {
                    if !x.consp() {
                        Err(#crate_name::Error::new(
                            #crate_name::ErrorKind::TypeMismatch,
                            format!(
                                "In call to {}, arg \"{}\" needs to be a list",
                                stringify!(#fn_name),
                                stringify!(#arg_name)
                            )
                        ).with_span(x.span()))
                    } else {
                        Ok(x.iter())
                    }
                }
            )}
        }
    } else {
        match arg_info.val_ty.to_token_stream().to_string().as_str() {
            "i64" | "f64" | "String" | "bool" | "Rc < dyn Any >" => {
                if optional {
                    quote! {(|x: #crate_name::TulispValue|if x.null() { Ok(None)} else {Ok(Some(x.try_into()?))})}
                } else {
                    quote! {(|x: #crate_name::TulispValue| x.try_into())}
                }
            }
            "TulispValue" => {
                if optional {
                    quote! {(|x: #crate_name::TulispValue|if x.null() { Ok(None)} else {Ok(x.into())})}
                } else {
                    quote! {(|x: #crate_name::TulispValue| Ok(x.into()))}
                }
            }
            _ => {
                tulisp_compile_error!(
                    arg_info.ty,
                    format!(
                        "Unknown type: {}",
                        arg_info.ty.to_token_stream().to_string()
                    )
                );
            }
        }
    };

    let car_extractor = if optional {
        quote! {
            __tulisp_internal_value
                .car()
                .or(Ok(Default::default()))?
        }
    } else {
        quote! {
            __tulisp_internal_value
                .car()
                .map_err(|e| #crate_name::Error::new(
                #crate_name::ErrorKind::MissingArgument,
                format!("Missing argument for required parameter '{}', in call to '{}'",
                        stringify!(#arg_name),
                        #fn_name
                ),
            ))?
        }
    };

    if eval_args {
        arg_info.extract_stmts.extend(quote! {
            let __tulisp_internal_car = ctx.eval(& #car_extractor)?;
        })
    } else {
        arg_info.extract_stmts.extend(quote! {
            let __tulisp_internal_car =  #car_extractor;
        })
    }

    arg_info.extract_stmts.extend(quote! {
        let #arg_name = #ty_extractor(__tulisp_internal_car)? ;
    });

    if optional {
        arg_info.extract_stmts.extend(quote! {
            let __tulisp_internal_value = __tulisp_internal_value.cdr().
                or(Ok(Default::default()))?;
        });
    } else {
        arg_info.extract_stmts.extend(quote! {
            let __tulisp_internal_value = __tulisp_internal_value.cdr()?;
        });
    }
    Ok(Some(arg_info))
}

/// Returns the name of the last segment, and generic params if any.
fn parse_type_path(arg_type: &syn::Type) -> Result<(String, Option<syn::Type>), TokenStream> {
    let type_path = match arg_type {
        syn::Type::Path(tp) => tp,
        _ => tulisp_compile_error!(arg_type, "Expected a segmented path type."),
    };

    let segment = type_path.path.segments.last().unwrap();
    let segment_name = segment.ident.to_string();

    let generic_type = match &segment.arguments {
        syn::PathArguments::None => None,
        syn::PathArguments::AngleBracketed(b) => {
            // return just the last generic arg for now, because right
            // now, we only support types with a single generic arg,
            // like Option and tulisp::Iter.
            match b.args.last().unwrap() {
                syn::GenericArgument::Type(ty) => Some(ty.to_owned()),
                _ => tulisp_compile_error!(
                    arg_type,
                    "Don't know how to deal with non-type generic args."
                ),
            }
        }
        _ => tulisp_compile_error!(arg_type, "Parenthesized arguments not supported."),
    };

    Ok((segment_name, generic_type))
}

fn get_name_and_type(arg: &FnArg) -> Result<Option<(TokenStream2, syn::Type)>, TokenStream> {
    match arg {
        FnArg::Receiver(_) => Ok(None),
        FnArg::Typed(pattype) => {
            let PatType { pat, ty, .. } = pattype;
            Ok(Some((pat.to_token_stream(), *ty.to_owned())))
        }
    }
}
