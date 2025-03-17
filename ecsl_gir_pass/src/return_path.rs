use crate::GIRPass;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{EcslError, ErrorLevel};
use ecsl_gir::GIR;

pub struct ReturnPath;

impl GIRPass for ReturnPath {
    type PassInput<'t> = DiagConn;
    type PassResult = ();

    fn apply_pass<'t>(gir: &mut GIR, diag: Self::PassInput<'t>) -> Self::PassResult {
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

        for block in leaf_blocks {
            if !gir.get_block(block).unwrap().terminated() {
                diag.push_error(EcslError::new(ErrorLevel::Error, "Return value expected"));
            }
        }
    }
}
