extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::io::Write;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput};

const PATH_TO_VM_SRC: &str = "./ecsl_vm/src/";

#[proc_macro_derive(Bytecode, attributes(execute))]
pub fn bytecode_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let Data::Enum(e) = ast.data else {
        panic!("Only enums supported")
    };

    let generated = generate(&e).into();
    zig_opcode_generation(&e).unwrap();

    generated
}

fn generate(e: &DataEnum) -> TokenStream {
    let mut variant_names = Vec::new();
    let mut variant_match_arms = Vec::new();
    let mut to_bytes = Vec::new();
    let mut to_bytecode = Vec::new();

    for variant in e.variants.iter() {
        // Names
        {
            let ident = variant.ident.clone();
            variant_names.push(ident);
        }

        // Match Arms
        {
            let ident = variant.ident.clone();
            let operand_len = variant.fields.len();
            variant_match_arms.push(quote! {
                #ident => #operand_len
            });
        }

        // to_bytecode Method
        {
            let ident = variant.ident.clone();
            let mut fields = Vec::new();
            let mut convert_bytes = Vec::new();
            for (i, field) in variant.fields.iter().enumerate() {
                let field_ident = format_ident!("o{}", i);
                fields.push(field_ident.clone());
                let syn::Type::Path(path) = &field.ty else {
                    panic!()
                };
                let method_name = format_ident!("to_{}", path.path.get_ident().unwrap());
                convert_bytes.push(quote! {
                    #field_ident.#method_name()?
                });
            }

            if fields.len() == 0 {
                to_bytecode.push(quote! {
                    (Opcode::#ident, []) => { Some(Bytecode::#ident) }
                });
            } else {
                to_bytecode.push(quote! {
                    (Opcode::#ident, [#(#fields),*]) => { Some(Bytecode::#ident(#(#convert_bytes),*)) }
                });
            }
        }

        // to_bytes Method
        {
            let ident = variant.ident.clone();
            let mut fields = Vec::new();
            let mut write_bytes = Vec::new();
            for i in 0..variant.fields.len() {
                let field_ident = format_ident!("f{}", i);
                fields.push(field_ident.clone());
                write_bytes.push(quote! {
                    bytes.extend_from_slice(&#field_ident.to_le_bytes());
                });
            }

            if variant.fields.len() == 0 {
                to_bytes.push(quote! {
                    #ident => { #(#write_bytes)* }
                });
            } else {
                to_bytes.push(quote! {
                    #ident (#(#fields),*) => { #(#write_bytes)* }
                });
            }
        }
    }

    quote! {
        #[derive(Debug, Clone, Copy)]
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

        impl BytecodeInstruction {
            pub fn to_bytecode(self) -> Option<Bytecode> {
                match (&self.op, self.operand.as_slice()) {
                    // (Opcode::PSHI, [o1]) => Some(Bytecode::PSHI(o1.to_word()?)),
                    #(#to_bytecode),*
                    _ => None,
                }
            }
        }

        impl Bytecode {
            fn discriminant(&self) -> u8 {
                unsafe { *(self as *const Self as *const u8) }
            }

            pub fn to_bytes(self) -> Vec<u8> {
                let mut bytes = Vec::new();
                bytes.push(self.discriminant());
                match self {
                    #(Bytecode::#to_bytes),*
                }
                return bytes;
            }
        }
    }
}

fn zig_opcode_generation(e: &DataEnum) -> std::io::Result<()> {
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
        r#"// This file is automatically generated
const std = @import("std");
const ins = @import("instruction.zig");
const thread = @import("thread.zig");

pub const Opcode = enum(u8) {{
    {}

    /// Convert a byte into an Opcode
    pub fn from_byte(b: u8) error{{InvalidInstruction}}!Opcode {{
        return std.meta.intToEnum(Opcode, b) catch return error.InvalidInstruction;
    }}

    /// Execute an Opcode
    pub fn execute(t: *thread.ProgramThread, op: Opcode) !void {{
        try switch (op) {{
            {}
        }};
    }}
}};

"#,
        variant_names.trim_end(),
        match_arms.trim_end(),
    );

    std::fs::File::create(format!("{PATH_TO_VM_SRC}/opcode.zig"))?
        .write(operations_zig.as_bytes())?;

    Ok(())
}
