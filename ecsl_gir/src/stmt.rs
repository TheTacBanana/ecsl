use crate::{expr::Expr, P};
use cfgrammar::Span;
use ecsl_index::LocalID;

pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

pub enum StmtKind {
    Assign(LocalID, P<Expr>),
    Expr(P<Expr>),
}
