use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput, Data, Fields, FieldsNamed, Type, TypePath};


#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    let ident = &derive_input.ident;
    let attr = derive_input.attrs.first().unwrap();
    eprintln!("{:#?}", attr);
    match &derive_input.data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let builder_struct = build_struct(ident, fields).unwrap();
                    let builder_construct = build_construct(ident, fields).unwrap();
                    let builder_setter = build_setter(ident, fields).unwrap();
                    let builder_builder = build_builder(ident, fields).unwrap();
                    quote! {
                        #builder_struct
                        #builder_setter
                        #builder_construct
                        #builder_builder
                    }.into()
                }
                _ => unimplemented!()
            } 
        }
        Data::Union(_) | Data::Enum(_) => unimplemented!(),
    }
}

fn build_struct(ident: &Ident, fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let strcut_fields: Vec<proc_macro2::TokenStream> = fields.named.iter()
        .map(|f| {
            let name = &f.ident;
            let outter_type = &f.ty;
            if let Some(inner_type) = parse_type(outter_type) {
                quote!(#name: std::option::Option<#inner_type>)
            } else {
                quote!(#name: std::option::Option<#outter_type>)
            }
        }).collect();
    let strcut_name = format_ident!("{}Builder", ident);
    let tokens = quote! {
        pub struct #strcut_name {
            #(#strcut_fields),*
        }
    };
    Ok(tokens)
}

fn build_construct(ident: &Ident, fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let idents = fields.named.iter().map(|f| &f.ident);
    let strcut_name = format_ident!("{}Builder", ident);
    let tokens = quote! {
        impl #ident {
            pub fn builder() -> #strcut_name {
                #strcut_name {
                    #(#idents: std::option::Option::None),*
                }
            }
        }
    };
    Ok(tokens)
}

fn build_setter(ident: &Ident, fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let mut setter_funcs: Vec<proc_macro2::TokenStream> = vec![];
    fields.named.iter().for_each(|f| {
        let name = &f.ident;
        let outter_type = &f.ty;
        let token = if let Some(inner_type) = parse_type(outter_type) {
            quote! {
                fn #name(&mut self, #name: #inner_type) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #outter_type) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        };
        setter_funcs.push(token);
    });

    let strcut_name = format_ident!("{}Builder", ident);
    let tokens = quote! {
        impl #strcut_name {
            #(#setter_funcs)*
        }
    };
    Ok(tokens)
}

fn build_builder(ident: &Ident, fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let strcut_name = format_ident!("{}Builder", ident);
    let checker_with_init: Vec<(proc_macro2::TokenStream, proc_macro2::TokenStream)>  = fields.named.iter()
        .map(|f| {
            
            let name = &f.ident;
            let outter_type = &f.ty;
            if let None = parse_type(outter_type) {
                let checker = quote! {
                    if self.#name.is_none() {
                        let err = format!("{} have not set!", stringify!(name));
                        return std::result::Result::Err(err.into())
                    }
                };
                let init = quote!(#name: self.#name.clone().unwrap());
                (checker, init)
            } else {
                (proc_macro2::TokenStream::new(), quote!(#name: self.#name.clone()))
            } 
        }).collect();
    let checkers = checker_with_init.iter().map(|ci| &ci.0);
    let inits = checker_with_init.iter().map(|ci| &ci.1);
    let tokens = quote! {
        extern crate alloc;
        impl #strcut_name {
            pub fn build(&mut self) -> std::result::Result<#ident, Box<dyn std::error::Error>> {
                #(#checkers)*

                Ok(#ident {
                    #(#inits),*
                })
            }
        }
    };
    Ok(tokens)
}

fn parse_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {ref path, ..}) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" {
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