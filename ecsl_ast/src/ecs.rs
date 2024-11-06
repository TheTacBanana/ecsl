use cfgrammar::Span;

use crate::{ty::Ty, SymbolId, P};

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

#[derive(Debug, Clone)]
pub enum ScheduleKind {
    /// A system in the schedule
    /// `foo`
    System(SymbolId), // TODO: This will require a pathed identifier for nice usage
    /// A schedule which must be executed in order
    /// `[ foo, bar ]`
    Ordered(Vec<P<Schedule>>),
    /// A schedule where the order is non-deterministic
    /// `{ foo, bar }`
    Unordered(Vec<P<Schedule>>),
    // /// A schedule where the systems will be scheduled in parallel
    // /// `| foo, bar |`
    // Parallelized(Vec<P<Schedule>>),
}