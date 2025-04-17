use std::{
    collections::{BTreeMap, BTreeSet},
    sync::{Arc, RwLock},
};

use cfgrammar::Span;
use ecsl_ast::parse::FnKind;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::{
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
    visit::{Visitor, VisitorCF},
    GIR,
};
use ecsl_index::{SourceFileID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use petgraph::{prelude::DiGraphMap, visit::Bfs};

use crate::GIRPass;

#[derive(Debug)]
pub struct FunctionGraph {
    pub graph: RwLock<DiGraphMap<TyID, Span>>,
    pub fnkinds: RwLock<BTreeMap<TyID, (FnKind, SourceFileID, Span)>>,
}

impl FunctionGraph {
    pub fn new() -> Self {
        Self {
            graph: Default::default(),
            fnkinds: Default::default(),
        }
    }

    fn add_dependencies(&self, deps: FunctionDependencies) {
        {
            let mut graph = self.graph.write().unwrap();
            graph.add_node(deps.fnid);
            for dep in deps.depends {
                graph.add_edge(deps.fnid, dep.0, dep.1);
            }
        }
        {
            let mut fnkinds = self.fnkinds.write().unwrap();
            fnkinds.insert(deps.fnid, (deps.fnkind, deps.fid, deps.span));
        }
    }

    pub fn find_sys_calls(&self, ctxt: &Arc<TyCtxt>) {
        let graph = self.graph.read().unwrap();
        let fnkinds = self.fnkinds.read().unwrap();

        let edges = graph.all_edges();
        for (from, to, span) in edges {
            let (fkind, tkind) = (fnkinds.get(&from).unwrap(), fnkinds.get(&to).unwrap());
            match (fkind.0, tkind.0) {
                (FnKind::Fn, FnKind::Sys) => {
                    ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, "Cannot call sys from fn")
                            .with_span(|_| *span)
                            .with_file(|_| fkind.1),
                    );
                }
                _ => (),
            }
        }
    }

    pub fn prune_unused(&self, root: TyID) {
        let mut graph = self.graph.write().unwrap();
        let mut bfs = Bfs::new(&*graph, root);
        let mut reachable = BTreeSet::new();
        while let Some(node) = bfs.next(&*graph) {
            reachable.insert(node);
        }

        let unused = graph
            .nodes()
            .filter_map(|node| {
                if reachable.contains(&node) {
                    None
                } else {
                    Some(node)
                }
            })
            .collect::<BTreeSet<TyID>>();

        for un in unused {
            graph.remove_node(un);
        }
    }
}

#[derive(Debug)]
pub struct FunctionDependencies {
    pub fnid: TyID,
    pub fnkind: FnKind,
    pub fid: SourceFileID,
    pub span: Span,
    pub depends: Vec<(TyID, Span)>,
}

impl GIRPass for FunctionDependencies {
    type PassInput<'a> = &'a Arc<FunctionGraph>;
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut GIR, fn_graph: &'a Arc<FunctionGraph>) -> () {
        let mut s = FunctionDependencies {
            fnid: gir.fn_id,
            fnkind: gir.fn_kind,
            fid: gir.fid,
            span: gir.span,
            depends: Vec::new(),
        };
        s.visit_gir(gir);
        fn_graph.add_dependencies(s);
    }
}

impl Visitor for FunctionDependencies {
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        match &s.kind {
            StmtKind::Assign(_, expr) => {
                match expr.kind {
                    ExprKind::Call(ty_id, _) => {
                        self.depends.push((ty_id, expr.span));
                    }
                    _ => (),
                };
            }
            _ => (),
        }
        VisitorCF::Continue
    }
}
