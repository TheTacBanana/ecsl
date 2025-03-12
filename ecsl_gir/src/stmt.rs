use crate::{expr::Expr, Place};
use cfgrammar::Span;
use ecsl_ast::parse::BytecodeInstruction;

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
    Assign(Place, Expr),
    Expr(Expr),
    BYT(BytecodeInstruction),
}

impl std::fmt::Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::Assign(place, expr) => write!(f, "{} = {}", place, expr),
            StmtKind::Expr(expr) => write!(f, "{}", expr),
            StmtKind::BYT(ins) => {
                write!(f, "{:?} {:?}", ins.op, ins.operand)
            }
        }
    }
}
