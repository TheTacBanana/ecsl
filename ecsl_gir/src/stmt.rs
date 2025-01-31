use crate::{expr::Expr, P};
use cfgrammar::Span;
use ecsl_index::LocalID;

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Assign(LocalID, P<Expr>),
    Expr(P<Expr>),
}

impl std::fmt::Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::Assign(local_id, expr) => write!(f, "{} = {}", local_id, expr),
            StmtKind::Expr(expr) => write!(f, "{}", expr),
        }
    }
}
