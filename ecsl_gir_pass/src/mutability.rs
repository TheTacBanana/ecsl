// use ecsl_diagnostics::DiagConn;
// use ecsl_gir::GIR;
// use ecsl_index::BlockID;
// use petgraph::visit::Bfs;

// use crate::GIRPass;

// pub struct Mutability<'a> {
//     diag: DiagConn,
//     gir: &'a GIR,
// }

// impl<'a> GIRPass for Mutability<'a> {
//     type PassInput<'t> = DiagConn;
//     type PassResult = ();

//     fn apply_pass<'t>(gir: &mut GIR, t: Self::PassInput<'a>) -> Self::PassResult {
//         let mut m = Mutability { diag: t, gir };

//         let mut bfs = Bfs::new(gir.ordering(), BlockID::ZERO);
//         while let Some(node) = bfs.next(gir.ordering()) {}
//     }
// }

// impl<'a> Visitor for Mutability<'a> {
//     fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
//         match &s.kind {
//             StmtKind::Assign(place, expr) => {
//                 let local = self.gir.get_local(place.local);
//                 use {LocalKind::*, Mutable::*};
//                 // match (local.kind, local.mutable) {
//                 //     (Ret, Imm) => todo!(),
//                 //     (Ret, Mut) => todo!(),
//                 //     (Arg, Imm) => todo!(),
//                 //     (Arg, Mut) => todo!(),
//                 //     (Temp, Imm) => todo!(),
//                 //     (Temp, Mut) => todo!(),
//                 //     (Let, Imm) => todo!(),
//                 //     (Let, Mut) => todo!(),
//                 //     (Internal, Imm) => todo!(),
//                 //     (Internal, Mut) => todo!(),
//                 // }

//                 VisitorCF::Continue
//             }
//             _ => VisitorCF::Continue,
//         }
//     }
// }
