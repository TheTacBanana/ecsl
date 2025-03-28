use crate::{linker::FunctionLinker, GIRPass};
use ecsl_gir::{
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
    visit::{VisitorCF, VisitorMut},
    Local, GIR,
};
use ecsl_index::{LocalID, TyID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use log::debug;
use std::{collections::BTreeMap, sync::Arc};

pub fn monomorphize(linker: &mut FunctionLinker, ctxt: &Arc<LocalTyCtxt>) {
    let mut generated = BTreeMap::new();
    for (f, gir) in linker.fn_gir.iter() {
        let variants = linker.mono.take_variants(*f).unwrap_or_default();

        for (v, generics) in variants {
            let mut new_gir = gir.clone();

            let mapping = generics
                .iter()
                .enumerate()
                .map(|(i, tyid)| (ctxt.global.tyid_from_tyir(TyIr::GenericParam(i)), *tyid))
                .collect();

            MonomorphizeFn::apply_pass(&mut new_gir, mapping);
            new_gir.fn_id = v;

            generated.insert(v, new_gir);
        }
        for g in &generated {
            debug!("{}", g.1);
        }
    }
    linker.fn_gir.extend(generated);
}

pub struct MonomorphizeFn {
    mapping: BTreeMap<TyID, TyID>,
}

impl MonomorphizeFn {
    pub fn replace_tyid(&mut self, tyid: &mut TyID) {
        *tyid = self.mapping.get(&tyid).cloned().unwrap_or(*tyid);
    }
}

impl GIRPass for MonomorphizeFn {
    type PassInput<'a> = BTreeMap<TyID, TyID>;
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut GIR, t: BTreeMap<TyID, TyID>) -> Self::PassResult {
        let mut m = MonomorphizeFn { mapping: t };
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
