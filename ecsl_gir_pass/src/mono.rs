use ecsl_gir::{stmt::StmtKind, visit::VisitorMut, GIR};
use ecsl_index::TyID;
use ecsl_ty::{local::LocalTyCtxt, MonoFnDef};
use std::sync::Arc;

use crate::GIRPass;

pub struct MonomorphizeFn {
    ctxt: Arc<LocalTyCtxt>,
    monofn: MonoFnDef,
}

impl MonomorphizeFn {
    pub fn replace_tyid(&mut self, tyid: TyID) -> TyID {
        todo!()
    }
}

impl GIRPass for MonomorphizeFn {
    type PassInput<'a> = (MonoFnDef, Arc<LocalTyCtxt>);
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut GIR, t: Self::PassInput<'a>) -> Self::PassResult {
        let mut m = MonomorphizeFn {
            monofn: t.0,
            ctxt: t.1,
        };
        m.visit_gir_mut(gir);
    }
}

impl VisitorMut for MonomorphizeFn {
    fn visit_stmt_mut(&mut self, s: &mut ecsl_gir::stmt::Stmt) -> ecsl_gir::visit::VisitorCF {
        match &mut s.kind {
            StmtKind::Assign(place, expr) => match &mut expr.kind {
                ecsl_gir::expr::ExprKind::Value(operand) => todo!(),
                ecsl_gir::expr::ExprKind::BinOp(bin_op, operand, operand1) => todo!(),
                ecsl_gir::expr::ExprKind::UnOp(un_op, operand) => todo!(),
                ecsl_gir::expr::ExprKind::Reference(mutable, local_id) => todo!(),
                ecsl_gir::expr::ExprKind::Cast(operand, operand_kind, operand_kind1) => todo!(),
                ecsl_gir::expr::ExprKind::Call(ty_id, operands) => todo!(),
            },
            StmtKind::AllocReturn(ty_id) => todo!(),
            StmtKind::BYT(bytecode_instruction) => todo!(),
        }
    }
}
