use crate::Place;
use cfgrammar::Span;
pub use ecsl_ast::expr::{BinOpKind, UnOpKind};
pub use ecsl_ast::ty::Mutable;
use ecsl_index::{ConstID, TyID};

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Value(Operand),
    BinOp(BinOp, Operand, Operand),
    UnOp(UnOp, Operand),
    Reference(Mutable, Place),
    /// From To
    Cast(Operand, OperandKind, OperandKind),
    Call(TyID, Vec<Operand>),
}

impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Value(operand) => write!(f, "{}", operand),
            ExprKind::BinOp(op, lhs, rhs) => {
                write!(f, "{} {:?} {}", lhs, op, rhs)
            }
            ExprKind::UnOp(op, operand) => write!(f, "{:?} {}", op, operand),
            ExprKind::Reference(mutable, place) => write!(f, "&{} {}", mutable, place),
            ExprKind::Call(ty_id, operands) => {
                write!(f, "{:?}(", ty_id)?;
                for op in operands {
                    write!(f, "{}, ", op)?;
                }
                write!(f, ")")
            }
            ExprKind::Cast(operand, from, to) => write!(f, "{} -> {:?} as {:?}", operand, from, to),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(ConstID),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Copy(place) => write!(f, "{} Copied", place),
            Operand::Move(place) => write!(f, "{} Moved", place),
            Operand::Constant(const_id) => write!(f, "{}", const_id),
        }
    }
}

impl Operand {
    pub fn place_mut(&mut self) -> Option<&mut Place> {
        match self {
            Operand::Copy(place) | Operand::Move(place) => Some(place),
            Operand::Constant(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BinOp(pub OperandKind, pub BinOpKind);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnOp(pub OperandKind, pub UnOpKind);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandKind {
    Bool,
    Int,
    Float,
}
