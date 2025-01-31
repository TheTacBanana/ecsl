use crate::{expr::Expr, P};
use cfgrammar::Span;
use ecsl_index::LocalID;

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Assign(LocalID, P<Expr>),
    Expr(P<Expr>),
}
