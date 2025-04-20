use crate::{
    cons::Constant,
    expr::{Expr, ExprKind, Operand},
    stmt::{Stmt, StmtKind},
    term::Terminator,
    Block, Local, Place, GIR,
};
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
    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        walk_expr(self, e)
    }
    fn visit_operand(&mut self, o: &Operand) -> VisitorCF {
        walk_operand(self, o)
    }
    #[must_use]
    fn visit_place(&mut self, s: &Place) -> VisitorCF {
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
    if let Some(term) = b.term.as_ref() {
        v.visit_term(term)?;
    }
    VisitorCF::Continue
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &Stmt) -> VisitorCF {
    match &s.kind {
        StmtKind::Assign(place, expr) => {
            v.visit_place(place)?;
            v.visit_expr(expr)?;
            VisitorCF::Continue
        }
        _ => VisitorCF::Continue,
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) -> VisitorCF {
    match &e.kind {
        ExprKind::Value(operand) => v.visit_operand(operand),
        ExprKind::BinOp(_, lhs, rhs) => {
            v.visit_operand(lhs)?;
            v.visit_operand(rhs)?;
            VisitorCF::Continue
        }
        ExprKind::UnOp(_, operand) => v.visit_operand(operand),
        ExprKind::Cast(operand, _, _) => v.visit_operand(operand),
        ExprKind::Call(_, operands) => {
            for op in operands {
                v.visit_operand(op)?;
            }
            VisitorCF::Continue
        }
        ExprKind::Reference(_, _) => VisitorCF::Continue,
        ExprKind::Query(_, operand) => v.visit_operand(operand),
    }
}

pub fn walk_operand<V: Visitor>(v: &mut V, o: &Operand) -> VisitorCF {
    match o {
        Operand::Copy(place) => v.visit_place(place),
        Operand::Move(place) => v.visit_place(place),
        Operand::Constant(_) => VisitorCF::Continue,
    }
}

pub fn walk_place<V: Visitor>(v: &mut V, p: &Place) -> VisitorCF {
    v.visit_place(p)
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
        walk_stmt_mut(self, s)
    }
    #[must_use]
    fn visit_expr_mut(&mut self, e: &mut Expr) -> VisitorCF {
        walk_expr_mut(self, e)
    }
    fn visit_operand_mut(&mut self, o: &mut Operand) -> VisitorCF {
        walk_operand_mut(self, o)
    }
    #[must_use]
    fn visit_place_mut(&mut self, p: &mut Place) -> VisitorCF {
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
    if let Some(term) = b.term.as_mut() {
        v.visit_term_mut(term)?;
    }
    VisitorCF::Continue
}

pub fn walk_stmt_mut<V: VisitorMut>(v: &mut V, s: &mut Stmt) -> VisitorCF {
    match &mut s.kind {
        StmtKind::Assign(place, expr) => {
            v.visit_place_mut(place)?;
            v.visit_expr_mut(expr)?;
            VisitorCF::Continue
        }
        _ => VisitorCF::Continue,
    }
}

pub fn walk_expr_mut<V: VisitorMut>(v: &mut V, e: &mut Expr) -> VisitorCF {
    match &mut e.kind {
        ExprKind::Value(operand) => v.visit_operand_mut(operand),
        ExprKind::BinOp(_, lhs, rhs) => {
            v.visit_operand_mut(lhs)?;
            v.visit_operand_mut(rhs)?;
            VisitorCF::Continue
        }
        ExprKind::UnOp(_, operand) => v.visit_operand_mut(operand),
        ExprKind::Cast(operand, _, _) => v.visit_operand_mut(operand),
        ExprKind::Call(_, operands) => {
            for op in operands {
                v.visit_operand_mut(op)?;
            }
            VisitorCF::Continue
        }
        ExprKind::Reference(_, _) => VisitorCF::Continue,
        ExprKind::Query(_, operand) => v.visit_operand_mut(operand),
    }
}

pub fn walk_operand_mut<V: VisitorMut>(v: &mut V, o: &mut Operand) -> VisitorCF {
    match o {
        Operand::Copy(place) => v.visit_place_mut(place),
        Operand::Move(place) => v.visit_place_mut(place),
        Operand::Constant(_) => VisitorCF::Continue,
    }
}

pub fn walk_place_mut<V: VisitorMut>(v: &mut V, p: &mut Place) -> VisitorCF {
    v.visit_place_mut(p)
}
