extern crate proc_macro;
extern crate syn;
// #[macro_use]
extern crate quote;

use proc_macro2::TokenStream;
use quote::quote;
use std::io::Write;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput};

const PATH_TO_VM_SRC: &str = "./ecsl_vm/src/";

#[proc_macro_derive(Bytecode, attributes(execute))]
pub fn bytecode_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let Data::Enum(e) = ast.data else {
        panic!("Only enums supported")
    };

    let opcode_enum = generate_opcode_enum(&e);
    // let conversion_impl = generate_conversion_impl(&e);

    zig_opcode_generation(&e);

    opcode_enum.into()
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

// fn generate_conversion_impl(e: &DataEnum) -> TokenStream {
//     let mut variant_names = Vec::new();
//     // let mut variant_match_arms = Vec::new();

//     for variant in e.variants.iter() {
//         let ident = variant.ident.clone();
//         variant_names.push(ident);

//     }

//     quote! {
//         impl BytecodeInstruction {
//             pub fn convert(self) -> Bytecode {

//             }
//         }
//     }
// }

fn zig_opcode_generation(e: &DataEnum) {
    let mut variant_names = String::new();
    let mut match_arms = String::new();

    for variant in e.variants.iter() {
        {
            let mut docs = String::new();
            for c in &variant.attrs {
                let expr = match c.meta {
                    syn::Meta::NameValue(ref name_value) if name_value.path.is_ident("doc") => {
                        &name_value.value
                    }
                    _ => continue,
                };

                let lit = match expr {
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }) => s.value(),
                    _ => continue,
                };

                docs.push_str(&format!("///{}\n\t", lit));
            }
            variant_names.push_str(&format!("{}{},\n\t", docs, variant.ident.to_string()));
        }

        {
            let mut execute_string = (|| {
                for c in &variant.attrs {
                    if c.path().is_ident("execute") {
                        let Ok(args): Result<syn::LitStr, syn::Error> = c.parse_args() else {
                            continue;
                        };
                        return Some(args.value());
                    }
                }
                return None;
            })();

            if execute_string.is_none() {
                let method_name = variant.ident.to_string().to_lowercase();
                let mut fields = String::new();
                for field in &variant.fields {
                    let field_ty = match &field.ty {
                        syn::Type::Path(p) => p.path.get_ident().unwrap().to_string(),
                        _ => panic!("Unsupported Ty"),
                    };

                    fields.push_str(&format!(", t.next_immediate({})", field_ty));
                }

                execute_string = Some(format!("ins.{}(t{})", method_name, fields));
            }

            match_arms.push_str(&format!(
                "Opcode.{} => {},\n\t\t\t",
                variant.ident.to_string(),
                execute_string.unwrap()
            ));
        }
    }

    let operations_zig = format!(
        r#"const std = @import("std");
const ins = @import("instruction.zig");
const thread = @import("thread.zig");

pub const Opcode = enum(u8) {{
    {}

    /// Convert a byte into an Opcode
    pub fn from_byte(b: u8) error{{InvalidInstruction}}!Opcode {{
        return std.meta.intToEnum(Opcode, b) catch return error.InvalidInstruction;
    }}

    /// Execute an Opcode
    pub fn execute(t: *thread.ProgramThread, op: Opcode) thread.ExecutionStatus {{
        switch (op) {{
            {}
        }}

        return thread.ExecutionStatus.Success;
    }}
}};

"#,
        variant_names.trim_end(),
        match_arms.trim_end(),
    );

    std::fs::File::create(format!("{PATH_TO_VM_SRC}/opcode.zig"))
        .unwrap()
        .write_all(operations_zig.as_bytes())
        .unwrap();
}
