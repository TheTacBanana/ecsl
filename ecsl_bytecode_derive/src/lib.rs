extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Fields, PathSegment};

const PATH_TO_VM_SRC: &str = "./ecsl_vm/src/";

#[proc_macro_derive(Bytecode)]
pub fn ast_attribute(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);


    let Data::Enum(e) = ast.data else { panic!("Only enums supported")};

    let out = generate_opcode_enum(&e);
    // panic!("{out}");
    out.into()
}

fn generate_opcode_enum(e: &DataEnum) -> TokenStream{
    let mut variant_names = Vec::new();
    let mut variant_match_arms = Vec::new();

    for variant in e.variants.iter() {
        let ident = variant.ident.clone();
        variant_names.push(ident);

        let ident = variant.ident.clone();
        let operand_len = variant.fields.len();
        variant_match_arms.push(
            quote! {
                #ident => #operand_len
            }
        );
    }

    quote! {
        #[derive(Debug)]
        pub enum Opcode {
            #(#variant_names),*
        }

        impl Opcode {
            pub fn operand_count(&self) -> usize {
                match &self {
                    #(Opcode::#variant_match_arms),*
                }
            }
        }
    }
}

// fn zig_opcode_generation(e: &DataEnum) {
//     std::fs::File::create(format!("{PATH_TO_VM_SRC}/temp_file")).unwrap();
// }