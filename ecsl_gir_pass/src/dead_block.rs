use ecsl_gir::{
    visit::{VisitorCF, VisitorMut},
    GIR,
};

use crate::GIRPass;

pub struct DeadBlocks;

impl GIRPass for DeadBlocks {
    fn apply_pass(&mut self, gir: &mut GIR) {
        self.visit_gir_mut(gir);
    }
}

impl VisitorMut for DeadBlocks {
    fn visit_gir_mut(&mut self, gir: &mut GIR) -> VisitorCF {
        let mut removals = Vec::new();
        for (i, b) in gir.blocks() {
            if b.empty() {
                removals.push(*i);
            }
        }
        for i in removals {
            _ = gir.remove_block(i);
        }
        VisitorCF::Continue
    }
}
