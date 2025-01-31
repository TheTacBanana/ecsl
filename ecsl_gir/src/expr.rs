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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Value(Operand),
    BinOp(BinOpKind, Operand, Operand),
    UnOp(UnOpKind, Operand),
    Reference(Mutable, LocalID),
    Call(TyID, Vec<Operand>),
}

impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Value(operand) => write!(f, "{}", operand),
            ExprKind::BinOp(op, lhs, rhs) => {
                write!(f, "{} {} {}", lhs, op, rhs)
            }
            ExprKind::UnOp(op, operand) => write!(f, "{} {}", op, operand),
            ExprKind::Reference(mutable, local_id) => write!(f, "{} {}", mutable, local_id),
            ExprKind::Call(ty_id, operands) => {
                write!(f, "{}", ty_id)?;
                for op in operands {
                    write!(f, "{}", op)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub enum Operand {
    Unknown,
    Copy(LocalID),
    Move(LocalID),
    Constant(ConstID),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Unknown => write!(f, "??"),
            Operand::Copy(local_id) => write!(f, "{} Copied", local_id),
            Operand::Move(local_id) => write!(f, "{} Moved", local_id),
            Operand::Constant(const_id) => write!(f, "C{}", const_id),
        }
    }
}
