use crate::GIRPass;
use ecsl_ast::parse::AttributeMarker;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::{stmt::StmtKind, GIR};
use ecsl_ty::local::LocalTyCtxt;
use std::sync::Arc;

pub struct ReturnPath;

impl GIRPass for ReturnPath {
    type PassInput<'t> = &'t Arc<LocalTyCtxt>;
    type PassResult = ();

    fn apply_pass<'t>(gir: &mut GIR, ty_ctxt: Self::PassInput<'t>) -> Self::PassResult {
        let ordering = gir.ordering();
        let leaf_blocks = ordering
            .nodes()
            .filter(|node| {
                ordering
                    .neighbors_directed(*node, petgraph::Direction::Outgoing)
                    .count()
                    == 0
            })
            .collect::<Vec<_>>();

        'outer: for block in leaf_blocks {
            let block = gir.get_block(block).unwrap();

            for stmt in block.stmts() {
                match &stmt.kind {
                    StmtKind::Assign(_, expr) => match &expr.kind {
                        ecsl_gir::expr::ExprKind::Call(ty_id, _) => {
                            let tyir = ty_ctxt.global.get_tyir(*ty_id).into_fn().unwrap();
                            if tyir.attributes.get_marker(AttributeMarker::Terminator) {
                                continue 'outer;
                            }
                        }
                        _ => (),
                    },
                    StmtKind::BYT(bytecode_instruction) => match bytecode_instruction.op {
                        ecsl_bytecode::Opcode::HALT => continue 'outer,
                        ecsl_bytecode::Opcode::PANIC => continue 'outer,
                        _ => (),
                    },
                    _ => (),
                }
            }
            if !block.terminated() {
                ty_ctxt.diag.push_error(
                    EcslError::new(ErrorLevel::Error, "Return value expected")
                        .with_span(|_| block.stmts().last().map(|s| s.span).unwrap_or(gir.span)),
                );
            }
        }
    }
}
