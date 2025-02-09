use crate::{cons::Constant, stmt::Stmt, term::Terminator, Block, Local, GIR};

pub use ecsl_ast::visit::VisitorCF;
use ecsl_index::{BlockID, ConstID, LocalID};

#[allow(unused)]
pub trait Visitor: Sized {
    fn visit_gir(&mut self, gir: &GIR) -> VisitorCF {
        walk_gir(self, gir)
    }
    #[must_use]
    fn visit_const(&mut self, i: ConstID, c: &Constant) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_local(&mut self, i: LocalID, l: &Local) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        walk_block(self, b)
    }
    #[must_use]
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_term(&mut self, s: &Terminator) -> VisitorCF {
        VisitorCF::Continue
    }
}

pub fn walk_gir<V: Visitor>(v: &mut V, gir: &GIR) -> VisitorCF {
    for (i, c) in &gir.consts {
        v.visit_const(*i, c)?;
    }
    for (i, l) in &gir.locals {
        v.visit_local(*i, l)?;
    }
    for (_, b) in &gir.blocks {
        v.visit_block(b)?;
    }
    VisitorCF::Continue
}

pub fn walk_block<V: Visitor>(v: &mut V, b: &Block) -> VisitorCF {
    for s in &b.stmts {
        v.visit_stmt(s)?;
    }
    v.visit_term(b.term.as_ref().expect("No terminator for block"))?;
    VisitorCF::Continue
}

#[allow(unused)]
pub trait VisitorMut: Sized {
    fn visit_gir_mut(&mut self, gir: &mut GIR) -> VisitorCF {
        walk_gir_mut(self, gir)
    }
    #[must_use]
    fn visit_const_mut(&mut self, i: ConstID, c: &mut Constant) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_local_mut(&mut self, i: LocalID, l: &mut Local) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_block_mut(&mut self, i: BlockID, b: &mut Block) -> VisitorCF {
        walk_block_mut(self, b)
    }
    #[must_use]
    fn visit_stmt_mut(&mut self, s: &mut Stmt) -> VisitorCF {
        VisitorCF::Continue
    }
    #[must_use]
    fn visit_term_mut(&mut self, s: &mut Terminator) -> VisitorCF {
        VisitorCF::Continue
    }
}

pub fn walk_gir_mut<V: VisitorMut>(v: &mut V, gir: &mut GIR) -> VisitorCF {
    for (i, c) in &mut gir.consts {
        v.visit_const_mut(*i, c)?;
    }
    for (i, l) in &mut gir.locals {
        v.visit_local_mut(*i, l)?;
    }
    for (i, b) in &mut gir.blocks {
        v.visit_block_mut(*i, b)?;
    }
    VisitorCF::Continue
}

pub fn walk_block_mut<V: VisitorMut>(v: &mut V, b: &mut Block) -> VisitorCF {
    for s in &mut b.stmts {
        v.visit_stmt_mut(s)?;
    }
    if let Some(term) = &mut b.term {
        v.visit_term_mut(term)?;
    }
    VisitorCF::Continue
}
