use crate::GIRPass;
use ecsl_ast::parse::AttributeMarker;
use ecsl_bytecode::Opcode;
use ecsl_gir::{
    expr::ExprKind,
    stmt::StmtKind,
    term::{SwitchCase, TerminatorKind},
};
use ecsl_index::BlockID;
use ecsl_ty::local::LocalTyCtxt;
use petgraph::prelude::DiGraphMap;
use std::{collections::BTreeSet, sync::Arc};

pub struct BlockOrder;

impl GIRPass for BlockOrder {
    type PassInput<'t> = &'t Arc<LocalTyCtxt>;
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut ecsl_gir::GIR, ty_ctxt: Self::PassInput<'a>) -> Self::PassResult {
        let mut visit_order = DiGraphMap::new();
        let mut visited = BTreeSet::new();
        let mut frontier = vec![(BlockID::ZERO, BlockID::ZERO)];
        let mut next_frontier = Vec::new();

        loop {
            if frontier.is_empty() {
                break;
            }
            'outer: for (from_id, to_id) in frontier.iter() {
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
                let block = gir.get_block_mut(*to_id).unwrap();

                let out = block.remove_after(|stmt| match &stmt.kind {
                    StmtKind::Assign(_, expr) => match &expr.kind {
                        ExprKind::Call(ty_id, _) => {
                            let tyir = ty_ctxt.global.get_tyir(*ty_id).into_fn().unwrap();
                            if tyir.attributes.get_marker(AttributeMarker::Terminator) {
                                return true;
                            }
                            return false;
                        }
                        _ => return false,
                    },
                    StmtKind::BYT(bytecode_instruction) => match bytecode_instruction.op {
                        Opcode::HALT => return true,
                        Opcode::PANIC => return true,
                        _ => false,
                    },
                    _ => false,
                });

                if out {
                    continue 'outer;
                }

                if let Some(term) = block.term() {
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
