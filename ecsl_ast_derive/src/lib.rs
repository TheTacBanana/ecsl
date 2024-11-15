extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, Ident, PatIdent
};

#[proc_macro_derive(AST)]
pub fn ast_attribute(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the string representation
    let ast = parse_macro_input!(input as DeriveInput);

    // Build the impl
    impl_partial_eq(&ast).into()
}

fn impl_partial_eq(ast: &DeriveInput) -> TokenStream {
    let name = &ast.ident;
    match &ast.data {
        Data::Struct(data) => {
            let mut lident_list = Vec::new();
            let mut rident_list = Vec::new();
            let mut c_list = Vec::new();
            for (i, f) in data.fields.iter().enumerate() {
                let l = f.ident.clone().unwrap();
                let r = f.ident.clone().unwrap();
                let c = match &f.ty {
                    syn::Type::Path(path)
                        if path
                            .path
                            .segments
                            .iter()
                            .any(|f| f.ident == "Span" || f.ident == "SymbolId") =>
                    {
                        TokenStream::new()
                    }
                    _ => {
                        quote!{
                            (lhs.#l == rhs.#r) &&
                        }
                    }
                };
                lident_list.push(l);
                rident_list.push(r);
                c_list.push(c);
            }
            if c_list.is_empty() {
                c_list.push(quote!{ true });
            }

            quote!(
                impl PartialEq for #name {
                    fn eq(&self, rhs: &Self) -> bool {
                        let lhs = self;
                        #(#c_list)* true
                    }
                }
            )
        }
        Data::Enum(vec) => {
            let mut match_arms = Vec::new();
            for v in vec.variants.iter() {
                let v_name = &v.ident;
                let arm = match &v.fields {
                    Fields::Unnamed(fields) => {
                        let mut lident_list = Vec::new();
                        let mut rident_list = Vec::new();
                        let mut c_list = Vec::new();
                        for (i, f) in fields.unnamed.iter().enumerate() {
                            let lhs = format_ident!("l{}", i);
                            let rhs = format_ident!("r{}", i);
                            let c = match &f.ty {
                                syn::Type::Path(path)
                                    if path
                                        .path
                                        .segments
                                        .iter()
                                        .any(|f| f.ident == "Span" || f.ident == "SymbolId") =>
                                {
                                    TokenStream::new()
                                }
                                _ => {
                                    quote!(#lhs == #rhs &&)
                                }
                            };
                            lident_list.push(lhs);
                            rident_list.push(rhs);
                            c_list.push(c);
                        }
                        if c_list.is_empty() {
                            c_list.push(quote!{ true });
                        }

                        quote! {
                            (#name::#v_name ( #( #lident_list),* ), #name::#v_name ( #( #rident_list),* )) => {
                                #(#c_list)* true
                            },
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            (#name::#v_name, #name::#v_name) => {
                                true
                            },
                        }
                    },
                    Fields::Named(_) => todo!(),
                };
                match_arms.push(arm);
            }

            let q = quote!(
                impl PartialEq for #name {
                    fn eq(&self, rhs: &Self) -> bool {
                        let lhs = self;
                        match (lhs, rhs) {
                            #(#match_arms)*
                            _ => false,
                        }
                    }
                }
            );
            // panic!("{q}")
            q
        }
        Data::Union(_) => panic!(),
    }
}
