use ecsl_bytecode::{function::FunctionBytecode, BytecodeInstruction, Opcode};
use ecsl_index::{BlockID, TyID};
use std::{collections::BTreeMap, sync::RwLock};

use crate::CodegenPass;

pub struct InlineableFunctions {
    bytecode: RwLock<BTreeMap<TyID, Vec<BytecodeInstruction>>>,
}

impl InlineableFunctions {
    pub fn new() -> Self {
        Self {
            bytecode: Default::default(),
        }
    }

    pub fn inlined(&self, tyid: TyID) -> bool {
        self.bytecode.read().unwrap().contains_key(&tyid)
    }

    pub fn try_inline(&self, tyid: TyID) -> Option<Vec<BytecodeInstruction>> {
        self.bytecode.read().unwrap().get(&tyid).cloned()
    }
}

pub struct CanInline;

impl CodegenPass for CanInline {
    type PassInput<'a> = &'a InlineableFunctions;

    type PassResult = ();

    fn apply_pass<'a>(
        bytecode: &mut FunctionBytecode,
        inline: Self::PassInput<'a>,
    ) -> Self::PassResult {
        macro_rules! ret {
            ($e:expr) => {
                if $e {
                    return;
                }
            };
        }

        ret!(bytecode.blocks.len() > 1);
        ret!(bytecode.contains(Opcode::SETSP));
        ret!(bytecode.contains(Opcode::LDR));
        ret!(bytecode.contains(Opcode::BPLDR));
        ret!(bytecode.contains(Opcode::CALL));
        ret!(bytecode.contains(Opcode::JMP));
        ret!(bytecode.contains(Opcode::JMPT));

        let mut block = bytecode.blocks.get(&BlockID::ZERO).unwrap().clone();

        if let Some([l, r]) = block.last_chunk::<2>() {
            if l.op == Opcode::BPSTR && r.op == Opcode::RET {
                block.pop();
                block.pop();
            }
        }

        let mut inline = inline.bytecode.write().unwrap();
        inline.insert(bytecode.tyid, block);
    }
}

pub struct Inline;

impl CodegenPass for Inline {
    type PassInput<'a> = &'a InlineableFunctions;

    type PassResult = ();

    fn apply_pass<'a>(
        bytecode: &mut FunctionBytecode,
        inline: Self::PassInput<'a>,
    ) -> Self::PassResult {
        for (_, block) in bytecode.blocks.iter_mut() {
            let mut out = Vec::new();
            {
                let mut iter = block.drain(..);
                while let Some(byt) = iter.next() {
                    if byt.op == Opcode::CALL {
                        let tyid = byt.operand[0].to_tyid().unwrap();
                        if let Some(to_inline) = inline.try_inline(tyid) {
                            out.extend(to_inline);
                        } else {
                            out.push(byt);
                        }
                    } else {
                        out.push(byt);
                    }
                }
            }

            *block = out;
        }
    }
}
