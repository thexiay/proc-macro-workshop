use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{DeriveInput, Data, Fields, Type, TypePath, Meta, MetaNameValue, punctuated::Punctuated, Token, Expr, ExprLit, Lit, spanned::Spanned, parse_macro_input};

struct BuilderContext {
    name: Ident,
    fields_ctx: Vec<BuilderFieldCtx>,
}

enum BuilderFieldCtx {
    Option(OptionFieldCtx),
    VecEach(VecEachFieldCtx),
    // other
    Other(OtherFieldCtx),
}

struct OptionFieldCtx {
    name: Ident,
    _outter_ty: Type,
    inner_ty: Type
}

struct VecEachFieldCtx {
    name: Ident,
    each_name: Ident,
    outter_ty: Type,
    inner_ty: Type,
}

struct OtherFieldCtx {
    name: Ident,
    ty: Type
}

impl BuilderFieldCtx {
    fn struct_filed(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldCtx::Option(OptionFieldCtx{ name, inner_ty, ..}) => {
                quote!(#name: std::option::Option<#inner_ty>)
            }
            BuilderFieldCtx::VecEach(VecEachFieldCtx{ name, outter_ty, ..}) => {
                quote!(#name: #outter_ty)
            }
            BuilderFieldCtx::Other(OtherFieldCtx{ name, ty}) => {
                quote!(#name: std::option::Option<#ty>)
            }
        }
    }

    fn init_filed(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldCtx::Option(OptionFieldCtx {name, ..}) => {
                quote!(#name: std::option::Option::None)
            }
            BuilderFieldCtx::VecEach(VecEachFieldCtx {name, ..}) => {
                quote!(#name: std::vec::Vec::new())
            }
            BuilderFieldCtx::Other(OtherFieldCtx {name, ..}) => {
                quote!(#name: std::option::Option::None)
            }
        }
    }

    fn setter_field(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldCtx::Option(OptionFieldCtx {name, inner_ty, ..}) => {
                quote! {
                    fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }
            }
            BuilderFieldCtx::VecEach(VecEachFieldCtx{name, each_name, inner_ty, outter_ty, ..}) => {
                let token = quote! {
                    fn #each_name(&mut self, #each_name: #inner_ty) -> &mut Self {
                        self.#name.push(#each_name);
                        self
                    }
                };
                if name != each_name {
                    quote! {
                        fn #name(&mut self, #name: #outter_ty) -> &mut Self {
                            self.#name.extend(#name);
                            self
                        }    
                        #token
                    }
                } else {
                    token
                }
            }
            BuilderFieldCtx::Other(OtherFieldCtx {name, ty}) => {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }    
                }
            }
        }
    }

    fn builder_field(&self) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        match self {
            BuilderFieldCtx::Option(OptionFieldCtx { name, .. }) => {
                (
                    proc_macro2::TokenStream::new(), 
                    quote!(#name: self.#name.clone())
                )
            }
            BuilderFieldCtx::VecEach(VecEachFieldCtx{ name, .. }) => {
                (
                    proc_macro2::TokenStream::new(),  
                    quote!(#name: self.#name.clone())
                )
            }
            BuilderFieldCtx::Other(OtherFieldCtx {name, ..}) => {
                (
                    quote! {
                        if self.#name.is_none() {
                            let err = format!("{} have not set!", stringify!(name));
                            return std::result::Result::Err(err.into())
                        }
                    }, 
                    quote!(#name: self.#name.clone().unwrap())
                )
            }
        }
    }
}


#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match &build_ctx(&derive_input) {
        Ok(builder_ctx) => {
            let builder_struct = build_struct(builder_ctx);
            let builder_construct = build_construct(builder_ctx);
            let builder_setter = build_setter(builder_ctx);
            let builder_builder = build_builder(builder_ctx);
            quote! {
                #builder_struct
                #builder_setter
                #builder_construct
                #builder_builder
            }.into()
        }
        Err(e) => return e.to_compile_error().into()
    }
}

fn build_ctx(input: &DeriveInput) -> syn::Result<BuilderContext> {
    let name = &input.ident;
    match &input.data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let fields_ctx_with_err: Vec<syn::Result<BuilderFieldCtx>> = fields.named.iter()
                        .map(|f | -> syn::Result<BuilderFieldCtx> {
                            if let Some(inner_type) = parse_inner_type(&f.ty, "Option") {
                                
                                return Ok(BuilderFieldCtx::Option(OptionFieldCtx {
                                    name: f.ident.as_ref().unwrap().clone(),
                                    _outter_ty: f.ty.clone(),
                                    inner_ty: inner_type.clone(),
                                }));
                            } 
                            
                            if let Some(inner_type) = parse_inner_type(&f.ty, "Vec") {
                                if let Some(name) = parse_helper_each(f)? {
                                    return Ok(BuilderFieldCtx::VecEach(VecEachFieldCtx { 
                                        name: f.ident.as_ref().unwrap().clone(),
                                        each_name: name, 
                                        outter_ty: f.ty.clone(),
                                        inner_ty: inner_type.clone(), 
                                    }));
                                } 
                            }
                            Ok(BuilderFieldCtx::Other(OtherFieldCtx { 
                                name: f.ident.as_ref().unwrap().clone(),
                                ty: f.ty.clone(),
                            }))
                        }).collect();
                    let mut fields_ctx = vec![];
                    for field in fields_ctx_with_err {
                        fields_ctx.push(field?);
                    }
                    Ok(BuilderContext {
                        name: input.ident.clone(),
                        fields_ctx,
                    })
                    
                }
                _ => Err(syn::Error::new_spanned(name, "not fielded names"))
            }
        }
        Data::Union(_) | Data::Enum(_) => Err(syn::Error::new_spanned(name, "unimpelement union and enum"))
    }
    
}

fn build_struct(ctx: &BuilderContext) -> proc_macro2::TokenStream {
    let struct_fields = ctx.fields_ctx.iter().map(|f| f.struct_filed());
    let strcut_name = format_ident!("{}Builder", ctx.name);
    quote! {
        pub struct #strcut_name {
            #(#struct_fields),*
        }
    }
}

fn build_construct(ctx: &BuilderContext) -> proc_macro2::TokenStream {
    let init_fields = ctx.fields_ctx.iter().map(|f| f.init_filed());
    let name = &ctx.name;
    let strcut_name = format_ident!("{}Builder", ctx.name);
    quote! {
        impl #name {
            pub fn builder() -> #strcut_name {
                #strcut_name {
                    #(#init_fields),*
                }
            }
        }
    }
}

fn build_setter(ctx: &BuilderContext) -> proc_macro2::TokenStream {
    let setter_fields = ctx.fields_ctx.iter().map(|f| {
        let a = f.setter_field();
        a
    });
    let strcut_name = format_ident!("{}Builder", ctx.name);
    quote! {
        impl #strcut_name {
            #(#setter_fields)*
        }
    }
}

fn build_builder(ctx: &BuilderContext) -> proc_macro2::TokenStream {
    let name = &ctx.name;
    let strcut_name = format_ident!("{}Builder", ctx.name);
    let build_fileds = ctx.fields_ctx.iter().map(|f| f.builder_field());
    let checker_code = build_fileds.clone().map(|f|f.0);
    let build_code = build_fileds.map(|f|f.1);
    quote! {
        extern crate alloc;
        impl #strcut_name {
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #(#checker_code)*

                Ok(#name {
                    #(#build_code),*
                })
            }
        }
    }
}

// parse inner type. only support 'vec' and 'option'
fn parse_inner_type<'a>(ty: &'a Type, outter_type: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {ref path, ..}) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == outter_type {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

/// support attribute: #[helper(each = "xxx")] on vec data type
fn parse_helper_each(field: &syn::Field) -> syn::Result<Option<Ident>> {
    // find first helper attribute
    if let Some(attr) = field.attrs.first() {
        let mut each_parse_res = None;
        if attr.path().is_ident("builder") {
            let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
            for meta in nested {
                match &meta {
                    Meta::NameValue(MetaNameValue {
                        path,
                        value: Expr::Lit(ExprLit{ 
                            lit: Lit::Str(str),
                            ..
                        }),
                        ..
                    }) if path.is_ident("each") => {
                        each_parse_res = Some(Ident::new(str.value().as_str(), str.span()));
                    }
                    _ => ()
                }
            }
        } 
        
        if let Some(each_value) = each_parse_res {
            Ok(Some(each_value))
        } else {
            Err(syn::Error::new(attr.meta.span(), "expected `builder(each = \"...\")`"))
        }
    } else {
        Ok(None)
    }
}