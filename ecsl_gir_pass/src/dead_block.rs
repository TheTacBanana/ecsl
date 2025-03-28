use crate::GIRPass;
use ecsl_gir::GIR;

pub struct DeadBlocks;

impl GIRPass for DeadBlocks {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass(gir: &mut GIR, _: Self::PassInput<'_>) -> () {
        let mut removals = Vec::new();
        for (i, _) in gir.blocks() {
            if !gir.ordering().contains_node(*i) {
                removals.push(*i);
            }
        }
        for i in removals {
            _ = gir.remove_block(i);
        }
    }
}
