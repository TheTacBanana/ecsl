use crate::GIRPass;
use ecsl_gir::GIR;
use ecsl_index::BlockID;

pub struct DeadBlocks;

impl GIRPass for DeadBlocks {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass(gir: &mut GIR, _: Self::PassInput<'_>) -> () {
        let mut removals = Vec::new();
        for (i, b) in gir.blocks() {
            if b.empty() && *i != BlockID::ZERO {
                removals.push(*i);
            }
        }
        for i in removals {
            _ = gir.remove_block(i);
        }
    }
}
