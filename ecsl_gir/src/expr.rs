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
    Query(QueryOpKind, Operand),
    Bundle(Vec<(TyID, Operand)>),
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
            ExprKind::Query(kind, operand) => write!(f, "{:?} Query {}", kind, operand),
            ExprKind::Bundle(operands) => {
                write!(f, "{{")?;
                for (_, op) in operands {
                    write!(f, "{}, ", op)?;
                }
                write!(f, "}}")
            }
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
    pub fn place(self) -> Option<Place> {
        match self {
            Operand::Copy(place) | Operand::Move(place) => Some(place),
            Operand::Constant(_) => None,
        }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryOpKind {
    Start,
    Next,
    Take,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query {
    pub with: Vec<TyID>,
    pub without: Vec<TyID>,
}

impl std::fmt::Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Query")?;

        if !self.with.is_empty() {
            write!(f, " With<")?;
            for tyid in self.with.iter() {
                write!(f, "{:?}, ", tyid)?;
            }
            write!(f, ">")?;
        }

        if !self.without.is_empty() {
            write!(f, " Without<")?;
            for tyid in self.without.iter() {
                write!(f, "{:?}, ", tyid)?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScheduleKind {
    Ordered,
    Unordered,
}

impl ScheduleKind {
    pub fn discriminant(&self) -> u8 {
        *self as u8
    }

    pub fn opening(&self) -> &'static str {
        match self {
            ScheduleKind::Ordered => "[",
            ScheduleKind::Unordered => "{",
        }
    }

    pub fn closing(&self) -> &'static str {
        match self {
            ScheduleKind::Ordered => "]",
            ScheduleKind::Unordered => "}",
        }
    }
}
