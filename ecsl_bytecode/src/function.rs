use crate::{ext::BytecodeExt, BytecodeInstruction, Opcode};
use ecsl_index::{BlockID, TyID};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct FunctionBytecode {
    pub tyid: TyID,
    pub visit_order: Vec<BlockID>,
    pub blocks: BTreeMap<BlockID, Vec<BytecodeInstruction>>,
}

impl FunctionBytecode {
    pub fn into_instructions(self) -> (Vec<BytecodeInstruction>, BTreeMap<BlockID, usize>) {
        // Calculate sizes
        let sizes = self
            .blocks
            .iter()
            .map(|(i, b)| (*i, b.bytecode_size()))
            .collect::<BTreeMap<_, _>>();

        // Calculate offsets
        let mut offsets = BTreeMap::new();
        self.visit_order.iter().fold(0, |cur_offset, block_id| {
            offsets.insert(*block_id, cur_offset);
            cur_offset + sizes.get(block_id).unwrap()
        });

        // Concat blocks into sequence
        let mut ins = Vec::new();
        for block_id in self.visit_order.iter() {
            ins.extend_from_slice(self.blocks.get(block_id).unwrap());
        }

        (ins, offsets)
    }

    pub fn map(&mut self, f: impl Copy + Fn(&mut BytecodeInstruction)) {
        self.blocks.iter_mut().for_each(|(_, block)| {
            _ = block
                .iter_mut()
                .map(|byt| {
                    f(byt);
                })
                .collect::<Vec<_>>();
        });
    }

    // Remove if false
    pub fn retain(&mut self, f: impl Copy + Fn(&BytecodeInstruction) -> bool) {
        self.blocks
            .iter_mut()
            .for_each(|(_, block)| block.retain(f));
    }

    // Remove if true
    pub fn remove(&mut self, f: impl Copy + Fn(&BytecodeInstruction) -> bool) {
        self.blocks
            .iter_mut()
            .for_each(|(_, block)| block.retain(|i| !f(i)));
    }

    pub fn contains(&self, op: Opcode) -> bool {
        self.blocks
            .iter()
            .any(|(_, block)| block.iter().any(|byt| byt.op == op))
    }
}

impl BytecodeExt for FunctionBytecode {
    fn bytecode_size(&self) -> usize {
        self.blocks.iter().fold(0, |l, r| l + r.1.bytecode_size())
    }
}
