use cfgrammar::Span;

use crate::{expr::Expr, ty::Ty, SymbolId, P};

#[derive(Debug, Clone)]
pub struct EntityTy {
    pub bounds: Vec<EntityAttribute>,
}

#[derive(Debug, Clone)]
pub struct EntityAttribute {
    pub span: Span,
    pub ident: SymbolId,
    pub ty: P<Ty>,
}

// #[derive(Debug, Clone)]
// pub struct QueryTy {
//     pub with: Vec<Ty>,
//     pub without: Vec<Ty>,
// }

#[derive(Debug, Clone)]
pub struct QueryExpr {
    pub with: Vec<Ty>,
    pub without: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct Schedule {
    pub span: Span,
    pub kind: ScheduleKind,
}

impl Schedule {
    pub fn new(span: Span, kind: ScheduleKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ScheduleKind {
    /// An expr which returns a schedule or system
    /// `foo` `bar.get_schedule()`
    Expr(P<Expr>),
    /// A schedule which must be executed in order
    /// `[ foo, bar ]`
    Ordered(Vec<Schedule>),
    /// A schedule where the order is non-deterministic
    /// `{ foo, bar }`
    Unordered(Vec<Schedule>),
    // /// A schedule where the systems will be scheduled in parallel
    // /// `| foo, bar |`
    // Parallelized(Vec<P<Schedule>>),
}
