use cfgrammar::Span;
use ecsl_ast::{
    expr::{BinOpKind, UnOpKind},
    ty::Mutable,
};
use ecsl_index::{ConstID, LocalID, TyID};

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Value(Operand),
    BinOp(BinOpKind, Operand, Operand),
    UnOp(UnOpKind, Operand),
    Reference(Mutable, LocalID),
    Call(TyID, Vec<Operand>),
}

#[derive(Debug)]
pub enum Operand {
    Unknown,
    Copy(LocalID),
    Move(LocalID),
    Constant(ConstID),
}
