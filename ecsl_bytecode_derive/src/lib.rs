extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::TokenStream;
use quote::quote;
use std::io::Write;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Expr, Lit};

const PATH_TO_VM_SRC: &str = "./ecsl_vm/src/";

#[proc_macro_derive(Bytecode)]
pub fn ast_attribute(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let Data::Enum(e) = ast.data else {
        panic!("Only enums supported")
    };

    let out = generate_opcode_enum(&e);

    zig_opcode_generation(&e);

    out.into()
}

fn generate_opcode_enum(e: &DataEnum) -> TokenStream {
    let mut variant_names = Vec::new();
    let mut variant_match_arms = Vec::new();

    for variant in e.variants.iter() {
        let ident = variant.ident.clone();
        variant_names.push(ident);

        let ident = variant.ident.clone();
        let operand_len = variant.fields.len();
        variant_match_arms.push(quote! {
            #ident => #operand_len
        });
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

fn zig_opcode_generation(e: &DataEnum) {
    let mut variant_names = String::new();

    for variant in e.variants.iter() {
        let mut docs = String::new();
        for c in &variant.attrs {
            match &c.meta {
                syn::Meta::NameValue(pair) if pair.path.segments[0].ident == "doc" => {
                    let Expr::Lit(l) = &pair.value else {
                        continue;
                    };
                    let Lit::Str(token) = &l.lit else {
                        continue;
                    };

                    docs.push_str(&format!("///{}\n\t", token.value()));
                }
                _ => (),
            }
        }
        variant_names.push_str(&format!("{}{},\n\t", docs, variant.ident.to_string()));
    }

    let operations_zig = format!(
        r#"const std = @import("std");
const ins = @import("instruction.zig");

pub const Opcode = enum(u8) {{
    {}

    pub fn from_byte(b: u8) error{{InvalidInstruction}}!Opcode {{
        return std.meta.intToEnum(Opcode, b) catch return error.InvalidInstruction;
    }}
}};

"#,
        variant_names.trim_end()
    );

    std::fs::File::create(format!("{PATH_TO_VM_SRC}/opcode.zig"))
        .unwrap()
        .write_all(operations_zig.as_bytes())
        .unwrap();
}
