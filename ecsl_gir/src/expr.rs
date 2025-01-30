use cfgrammar::Span;
use ecsl_ast::ty::Mutable;
use ecsl_index::{LocalID, TyID};

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {
    BinOp(BinOpKind),
    UnOp(UnOpKind),
    Reference(Mutable, LocalID),
    Call(TyID, Vec<Operand>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOpKind {
    Neg,
    Not,
    Deref,
}

pub enum Operand {
    Copy(LocalID),
    Move(LocalID),
    Constant(LocalID),
}
