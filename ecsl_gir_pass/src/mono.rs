use crate::{linker::FunctionLinker, GIRPass};
use cfgrammar::Span;
use ecsl_gir::{
    expr::Expr,
    stmt::{Stmt, StmtKind},
    visit::{walk_expr_mut, walk_stmt_mut, VisitorCF, VisitorMut},
    Local, Place, GIR,
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

            debug!("{}", new_gir);

            let mapping = generics
                .iter()
                .enumerate()
                .map(|(i, tyid)| (ctxt.global.tyid_from_tyir(TyIr::GenericParam(i)), *tyid))
                .collect::<BTreeMap<_, _>>();

            MonomorphizeFn::apply_pass(&mut new_gir, (ctxt, mapping.clone()));
            new_gir.fn_id = v;

            generated.insert(v, new_gir);
        }
    }
    linker.fn_gir.extend(generated);
}

pub struct MonomorphizeFn<'a> {
    ctxt: &'a Arc<LocalTyCtxt>,
    mapping: BTreeMap<TyID, TyID>,
}

impl<'a> MonomorphizeFn<'a> {
    pub fn replace_tyid(&self, tyid: &mut TyID, span: Span) {
        {
            let params = {
                self.ctxt
                    .global
                    .monos
                    .mono_map
                    .read()
                    .unwrap()
                    .get_by_right(tyid)
                    .cloned()
            };

            if let Some((_, params)) = params {
                let params = params
                    .clone()
                    .drain(..)
                    .map(|ty| {
                        let mut ty = ty;
                        self.replace_tyid(&mut ty, span);
                        ty
                    })
                    .collect::<Vec<_>>();

                *tyid = self.ctxt.get_mono_variant(*tyid, &params, span).unwrap();
                return;
            }
        }
        {
            let mut tyir = self.ctxt.global.get_tyir(*tyid);
            *tyid = match &mut tyir {
                TyIr::Ref(_, field_def) => {
                    self.replace_tyid(&mut field_def.ty, span);
                    self.ctxt.global.tyid_from_tyir(tyir)
                }
                _ => self.mapping.get(&tyid).cloned().unwrap_or(*tyid),
            };
            return;
        }
    }
}

impl<'a> GIRPass for MonomorphizeFn<'a> {
    type PassInput<'t> = (&'t Arc<LocalTyCtxt>, BTreeMap<TyID, TyID>);
    type PassResult = ();

    fn apply_pass<'t>(gir: &mut GIR, t: Self::PassInput<'t>) -> Self::PassResult {
        let mut m = MonomorphizeFn {
            ctxt: t.0,
            mapping: t.1,
        };
        m.visit_gir_mut(gir);
    }
}

impl<'a> VisitorMut for MonomorphizeFn<'a> {
    fn visit_local_mut(&mut self, _: LocalID, l: &mut Local) -> VisitorCF {
        self.replace_tyid(&mut l.tyid, l.span);
        VisitorCF::Continue
    }

    fn visit_stmt_mut(&mut self, s: &mut Stmt) -> VisitorCF {
        match &mut s.kind {
            StmtKind::AllocReturn(ty_id) => self.replace_tyid(ty_id, s.span),
            _ => (),
        }
        walk_stmt_mut(self, s)
    }

    fn visit_expr_mut(&mut self, e: &mut Expr) -> VisitorCF {
        walk_expr_mut(self, e)
    }

    fn visit_place_mut(&mut self, p: &mut Place) -> VisitorCF {
        let span = p.span;
        p.rewrite_tyid(|f| self.replace_tyid(f, span));
        VisitorCF::Continue
    }
}
