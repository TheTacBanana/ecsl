use crate::GIRPass;
use ecsl_gir::term::{SwitchCase, TerminatorKind};
use ecsl_index::BlockID;
use petgraph::prelude::DiGraphMap;
use std::collections::BTreeSet;

pub struct BlockOrder;

impl GIRPass for BlockOrder {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut ecsl_gir::GIR, _: Self::PassInput<'a>) -> Self::PassResult {
        let mut visit_order = DiGraphMap::new();
        let mut visited = BTreeSet::new();
        let mut frontier = vec![(BlockID::ZERO, BlockID::ZERO)];
        let mut next_frontier = Vec::new();

        loop {
            if frontier.is_empty() {
                break;
            }
            for (from_id, to_id) in frontier.iter() {
                let key = (*from_id, *to_id);

                if visited.contains(&key) {
                    continue;
                }

                visited.insert(key);

                visit_order.add_node(*to_id);
                if from_id != to_id {
                    visit_order.add_edge(*from_id, *to_id, ());
                }

                // Extend frontier
                if let Some(term) = gir.get_block(*to_id).unwrap().term() {
                    match &term.kind {
                        TerminatorKind::Jump(id) => next_frontier.push((*to_id, *id)),
                        TerminatorKind::Switch(_, switch_cases) => {
                            next_frontier.extend(switch_cases.iter().map(|c| match c {
                                SwitchCase::Value(_, id) => (*to_id, *id),
                                SwitchCase::Default(id) => (*to_id, *id),
                            }));
                        }
                        TerminatorKind::Return => (),
                    }
                }
            }
            frontier = std::mem::take(&mut next_frontier);
        }

        gir.with_ordering(visit_order);
    }
}
