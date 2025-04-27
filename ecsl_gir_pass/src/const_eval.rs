use crate::{comp_ids::ComponentDefinitions, GIRPass};
use ecsl_assembler::{Assembler, ConstData};
use ecsl_bytecode::Immediate;
use ecsl_diagnostics::{DiagConn, Diagnostics};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::{
    cons::{Constant, Literal},
    visit::{Visitor, VisitorCF},
    GIR,
};
use ecsl_index::ConstID;
use ecsl_parse::{LexerTy, NonStreamingLexer};
use std::{collections::BTreeMap, ops::Deref};
use unescape::unescape;

pub struct ConstEval<'a> {
    lexer: &'a LexerTy<'a, 'a>,
    comp_defs: &'a ComponentDefinitions,
    assembler: &'a Assembler<ConstData>,
    diag: DiagConn,
    out: ConstMap,
}

#[derive(Debug, Default)]
pub struct ConstMap {
    consts: BTreeMap<ConstID, Immediate>,
}

impl Deref for ConstMap {
    type Target = BTreeMap<ConstID, Immediate>;

    fn deref(&self) -> &Self::Target {
        &self.consts
    }
}

impl<'a> GIRPass for ConstEval<'a> {
    type PassInput<'t> = (
        &'t LexerTy<'t, 't>,
        &'t ComponentDefinitions,
        &'t Assembler<ConstData>,
        DiagConn,
    );
    type PassResult = ConstMap;

    fn apply_pass<'t>(
        gir: &mut GIR,
        (lexer, comp_defs, assembler, diag): Self::PassInput<'t>,
    ) -> ConstMap {
        let mut s = ConstEval {
            lexer,
            assembler,
            comp_defs,
            out: Default::default(),
            diag,
        };

        s.visit_gir(gir);
        s.out
    }
}

impl<'a> Visitor for ConstEval<'a> {
    fn visit_const(&mut self, i: ConstID, c: &Constant) -> VisitorCF {
        let out = match c {
            Constant::Internal { imm, .. } => Some(*imm),
            Constant::External { kind, span, .. } => {
                let s = self.lexer.span_str(*span);
                match kind {
                    Literal::Int => {
                        if let Ok(i) = s.parse() {
                            Some(Immediate::Int(i))
                        } else {
                            self.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    "Int literal exceeds the bounds of type",
                                )
                                .with_span(|_| *span),
                            );
                            None
                        }
                    }

                    Literal::Float => {
                        if let Ok(f) = s.parse() {
                            Some(Immediate::Float(f))
                        } else {
                            self.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    "Float literal exceeds the bounds of type",
                                )
                                .with_span(|_| *span),
                            );
                            None
                        }
                    }

                    Literal::Bool => Some(Immediate::Bool(match s {
                        "true" => true,
                        "false" => false,
                        _ => panic!("Cannot parse {:?} as bool", s),
                    })),
                    Literal::Char => {
                        let c = &s.to_string()[1..(s.len() - 1)];
                        let mut chars = c.chars();
                        let c = match (chars.next().unwrap(), chars.next()) {
                            ('\\', Some('t')) => '\t',
                            ('\\', Some('n')) => '\n',
                            ('\\', Some('r')) => '\r',
                            ('\\', Some('\'')) => '\'',
                            ('\\', Some('\"')) => '\"',
                            ('\\', Some('\\')) => '\\',
                            (c, None) => c,
                            _ => panic!(),
                        };
                        let mut buffer = [0u8];
                        c.encode_utf8(&mut buffer);
                        Some(Immediate::UByte(buffer[0]))
                    }
                    Literal::String => {
                        let s = unescape(&s.to_string()[1..(s.len() - 1)]);
                        if let Some(s) = s {
                            let mut bytes = s.bytes().collect::<Vec<u8>>();
                            bytes.push(0);
                            let id = self.assembler.add_const_data(bytes);
                            Some(Immediate::ConstAddressOf(id))
                        } else {
                            self.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Cannot parse string")
                                    .with_span(|_| *span),
                            );
                            None
                        }
                    }
                }
            }
            Constant::Query { query, .. } => {
                let mut bytes = Vec::new();

                // Push lengths of with and without arrays
                bytes.extend_from_slice(&(query.with.len() as u16).to_be_bytes());
                bytes.extend_from_slice(&(query.without.len() as u16).to_be_bytes());

                // Push with component IDs
                for tyid in query.with.iter() {
                    let comp_id = self.comp_defs.get_component(*tyid);
                    bytes.extend_from_slice(&(comp_id.inner() as u32).to_be_bytes());
                }

                // Push without component IDs
                for tyid in query.without.iter() {
                    let comp_id = self.comp_defs.get_component(*tyid);
                    bytes.extend_from_slice(&(comp_id.inner() as u32).to_be_bytes());
                }

                let id = self.assembler.add_const_data(bytes);
                Some(Immediate::ConstAddressOf(id))
            }
            Constant::Schedule { kind, contents } => {
                let mut bytes = Vec::new();
                bytes.push(kind.discriminant());
                bytes.extend_from_slice(&(contents.len() as u32).to_be_bytes());

                let mut patches = Vec::new();
                for (i, c) in contents.iter().enumerate() {
                    match self.out.consts.get(c).unwrap() {
                        Immediate::AddressOf(tyid) => {
                            bytes.extend_from_slice(&(tyid.inner() as u64).to_be_bytes());
                            patches.push((5 + i * 8) as u64);
                        }
                        Immediate::ConstAddressOf(id) => {
                            let offset = self.assembler.get_offset(*id).unwrap();
                            bytes.extend_from_slice(&offset.to_be_bytes());
                        }
                        _ => panic!(),
                    }
                }

                let id = self.assembler.add_const_data(bytes);

                for p in patches {
                    self.assembler.add_fn_patch_marker(id, p);
                }

                Some(Immediate::ConstAddressOf(id))
            }
        };

        if let Some(cons) = out {
            self.out.consts.insert(i, cons);
        }

        VisitorCF::Continue
    }
}
