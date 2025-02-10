use crate::GIRPass;
use ecsl_gir::{
    visit::{VisitorCF, VisitorMut},
    GIR,
};

pub struct DeadBlocks;

impl GIRPass for DeadBlocks {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass(gir: &mut GIR, _: Self::PassInput<'_>) -> () {
        DeadBlocks {}.visit_gir_mut(gir);
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
