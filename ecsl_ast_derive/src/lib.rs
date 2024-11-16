extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, PathSegment};

#[proc_macro_derive(AST)]
pub fn ast_attribute(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    impl_partial_eq(&ast).into()
}

fn exclude_symbols(s: &PathSegment) -> bool {
    s.ident == "Span" || s.ident == "SymbolId"
}

fn impl_partial_eq(ast: &DeriveInput) -> TokenStream {
    let name = &ast.ident;
    match &ast.data {
        Data::Struct(data) => {
            let mut left = Vec::new();
            let mut right = Vec::new();
            let mut comparions = Vec::new();
            for f in data.fields.iter() {
                let l = f.ident.clone().unwrap();
                let r = f.ident.clone().unwrap();
                let c = match &f.ty {
                    syn::Type::Path(path) if path.path.segments.iter().any(exclude_symbols) => {
                        TokenStream::new()
                    }
                    _ => {
                        quote! {
                            (lhs.#l == rhs.#r) &&
                        }
                    }
                };
                left.push(l);
                right.push(r);
                comparions.push(c);
            }
            if comparions.is_empty() {
                comparions.push(quote! { true });
            }

            quote!(
                impl PartialEq for #name {
                    fn eq(&self, rhs: &Self) -> bool {
                        let lhs = self;
                        #(#comparions)* true
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
                        let mut left = Vec::new();
                        let mut right = Vec::new();
                        let mut comparisons = Vec::new();
                        for (i, f) in fields.unnamed.iter().enumerate() {
                            let lhs = format_ident!("l{}", i);
                            let rhs = format_ident!("r{}", i);
                            let c = match &f.ty {
                                syn::Type::Path(path)
                                    if path.path.segments.iter().any(exclude_symbols) =>
                                {
                                    TokenStream::new()
                                }
                                _ => {
                                    quote!(#lhs == #rhs &&)
                                }
                            };
                            left.push(lhs);
                            right.push(rhs);
                            comparisons.push(c);
                        }
                        if comparisons.is_empty() {
                            comparisons.push(quote! { true });
                        }

                        quote! {
                            (#name::#v_name ( #( #left),* ), #name::#v_name ( #( #right),* )) => {
                                #(#comparisons)* true
                            },
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            (#name::#v_name, #name::#v_name) => {
                                true
                            },
                        }
                    }
                    Fields::Named(_) => todo!(),
                };
                match_arms.push(arm);
            }

            quote!(
                impl PartialEq for #name {
                    fn eq(&self, rhs: &Self) -> bool {
                        let lhs = self;
                        match (lhs, rhs) {
                            #(#match_arms)*
                            _ => false,
                        }
                    }
                }
            )
        }
        Data::Union(_) => panic!("Unions not supported"),
    }
}
