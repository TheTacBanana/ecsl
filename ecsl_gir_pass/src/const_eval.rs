use crate::GIRPass;
use ecsl_assembler::{Assembler, ConstData};
use ecsl_bytecode::Immediate;
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
    assembler: &'a Assembler<ConstData>,
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
    type PassInput<'t> = (&'t LexerTy<'t, 't>, &'t Assembler<ConstData>);
    type PassResult = ConstMap;

    fn apply_pass<'t>(gir: &mut GIR, (lexer, assembler): Self::PassInput<'t>) -> ConstMap {
        let mut s = ConstEval {
            lexer,
            assembler,
            out: Default::default(),
        };

        s.visit_gir(gir);
        s.out
    }
}

impl<'a> Visitor for ConstEval<'a> {
    fn visit_const(&mut self, i: ConstID, c: &Constant) -> VisitorCF {
        self.out.consts.insert(
            i,
            match c {
                Constant::Internal { imm } => *imm,
                Constant::External { kind, span, .. } => {
                    let s = self.lexer.span_str(*span);
                    match kind {
                        Literal::Int => Immediate::Int(s.parse().unwrap()),
                        Literal::Float => Immediate::Float(s.parse().unwrap()),
                        Literal::Bool => Immediate::Bool(match s {
                            "true" => true,
                            "false" => false,
                            _ => panic!("Cannot parse {:?} as bool", s),
                        }),
                        Literal::Char => Immediate::UByte({
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
                            buffer[0]
                        }),
                        Literal::String => {
                            let s = unescape(&s.to_string()[1..(s.len() - 1)]).unwrap(); // TODO: Perhaps remove dependency in the future
                            let mut bytes = s.bytes().collect::<Vec<u8>>();
                            bytes.push(0);
                            let id = self.assembler.add_const_data(bytes);
                            Immediate::ConstAddressOf(id)
                        }
                    }
                }
            },
        );
        VisitorCF::Continue
    }
}
