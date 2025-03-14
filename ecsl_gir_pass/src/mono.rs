use crate::{linker::FunctionLinker, GIRPass};
use ecsl_gir::{
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
    visit::{VisitorCF, VisitorMut},
    Local, GIR,
};
use ecsl_index::{LocalID, TyID};
use ecsl_ty::{local::LocalTyCtxt, MonoFnDef, TyIr};
use log::debug;
use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    sync::{Arc, RwLock},
};

#[derive(Debug, Default)]
pub struct Mono {
    /// Function which needs to be monomorphized
    variants: RwLock<BTreeMap<TyID, BTreeSet<TyID>>>,
}

impl Mono {
    pub fn new() -> Self {
        Mono {
            variants: Default::default(),
        }
    }

    pub fn insert(&self, tyid: TyID, variant: TyID) {
        let mut lock = self.variants.write().unwrap();
        match lock.entry(tyid) {
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(vec![variant].drain(..).collect());
            }
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.get_mut().insert(variant);
            }
        }
    }

    pub fn take_variants(&self, tyid: TyID) -> Option<BTreeSet<TyID>> {
        let mut lock = self.variants.write().unwrap();
        lock.remove(&tyid)
    }
}

pub fn monomorphize(linker: &mut FunctionLinker, mono: &Arc<Mono>, ctxt: &Arc<LocalTyCtxt>) {
    let mut generated = BTreeMap::new();
    for (f, gir) in linker.fn_gir.iter() {
        let variants = mono.take_variants(*f).unwrap_or_default();

        debug!("{:?} {:?}", f, variants);
        for v in variants {
            debug!("Monomorphizing {v:?}");

            let mut new_gir = gir.clone();
            let TyIr::MonoFn(mono_fn) = ctxt.global.get_tyir(v) else {
                panic!()
            };
            MonomorphizeFn::apply_pass(&mut new_gir, mono_fn);
            new_gir.set_fn_id(v);

            generated.insert(v, new_gir);
        }
        debug!("{:?}", generated);
    }
    linker.fn_gir.extend(generated);
}

pub struct MonomorphizeFn {
    monofn: MonoFnDef,
}

impl MonomorphizeFn {
    pub fn replace_tyid(&mut self, tyid: &mut TyID) {
        *tyid = self.monofn.mono.get(&tyid).cloned().unwrap_or(*tyid);
    }
}

impl GIRPass for MonomorphizeFn {
    type PassInput<'a> = MonoFnDef;
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut GIR, t: MonoFnDef) -> Self::PassResult {
        let mut m = MonomorphizeFn { monofn: t };
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
