use crate::{ext::BytecodeExt, BytecodeInstruction};
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

    pub fn retain(&mut self, f: impl Copy + Fn(&BytecodeInstruction) -> bool) {
        self.blocks
            .iter_mut()
            .for_each(|(_, block)| block.retain(f));
    }
}

impl BytecodeExt for FunctionBytecode {
    fn bytecode_size(&self) -> usize {
        self.blocks.iter().fold(0, |l, r| l + r.1.bytecode_size())
    }
}
