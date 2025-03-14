use std::{
    collections::BTreeSet,
    sync::{Arc, RwLock},
};

use ecsl_gir::{
    expr::ExprKind,
    stmt::{Stmt, StmtKind},
    visit::{Visitor, VisitorCF},
    GIR,
};
use ecsl_index::TyID;
use log::debug;
use petgraph::{prelude::DiGraphMap, visit::Bfs};

use crate::GIRPass;

#[derive(Debug)]
pub struct FunctionGraph {
    pub graph: RwLock<DiGraphMap<TyID, ()>>,
}

impl FunctionGraph {
    pub fn new() -> Self {
        Self {
            graph: Default::default(),
        }
    }

    fn add_dependencies(&self, deps: FunctionDependencies) {
        let mut graph = self.graph.write().unwrap();
        graph.add_node(deps.fnid);
        for dep in deps.depends {
            graph.add_edge(deps.fnid, dep, ());
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
    pub depends: Vec<TyID>,
}

impl GIRPass for FunctionDependencies {
    type PassInput<'a> = &'a Arc<FunctionGraph>;
    type PassResult = ();

    fn apply_pass<'a>(gir: &mut GIR, fn_graph: &'a Arc<FunctionGraph>) -> () {
        let mut s = FunctionDependencies {
            fnid: gir.fn_id,
            depends: Vec::new(),
        };
        s.visit_gir(gir);
        debug!("{:?}", s);
        fn_graph.add_dependencies(s);
    }
}

impl Visitor for FunctionDependencies {
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        match &s.kind {
            StmtKind::Assign(_, expr) => {
                match expr.kind {
                    ExprKind::Call(ty_id, _) => {
                        self.depends.push(ty_id);
                    }
                    _ => (),
                };
            }
            _ => (),
        }
        VisitorCF::Continue
    }
}
