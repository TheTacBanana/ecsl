use ecsl_gir::{
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
    visit::{VisitorCF, VisitorMut},
    Local, GIR,
};
use ecsl_index::{LocalID, TyID};
use ecsl_ty::{local::LocalTyCtxt, MonoFnDef};
use std::sync::Arc;

use crate::GIRPass;

pub struct MonomorphizeFn {
    ctxt: Arc<LocalTyCtxt>,
    monofn: MonoFnDef,
}

impl MonomorphizeFn {
    pub fn replace_tyid(&mut self, tyid: &mut TyID) {
        *tyid = self.monofn.mono.get(&tyid).cloned().unwrap_or(*tyid);
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
    fn visit_local_mut(&mut self, _: LocalID, l: &mut Local) -> VisitorCF {
        self.replace_tyid(&mut l.tyid);
        VisitorCF::Continue
    }

    fn visit_stmt_mut(&mut self, s: &mut Stmt) -> VisitorCF {
        match &mut s.kind {
            StmtKind::Assign(_, expr) => match &mut expr.kind {
                ExprKind::Call(ty_id, _) => self.replace_tyid(ty_id),
                ExprKind::Cast(_, _, _)
                | ExprKind::Value(_)
                | ExprKind::BinOp(_, _, _)
                | ExprKind::UnOp(_, _)
                | ExprKind::Reference(_, _) => (),
            },
            StmtKind::AllocReturn(ty_id) => self.replace_tyid(ty_id),
            StmtKind::BYT(_) => (),
        }
        VisitorCF::Continue
    }
}
