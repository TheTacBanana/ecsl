use cfgrammar::Span;

use crate::{path::Path, ty::Ty, Ident, P};

#[derive(Debug, Clone)]
pub struct EntityTy {
    bounds: Option<Vec<P<EntityComponent>>>,
}

#[derive(Debug, Clone)]
pub struct EntityComponent {
    span: Span,
    ident: Ident,
    ty: P<Ty>,
}

#[derive(Debug, Clone)]
pub struct QueryTy {
    with: Vec<Ty>,
    without: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct QueryExpr {
    with: Vec<Ty>,
    without: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct Schedule {
    span: Span,
    kind: ScheduleKind,
}

#[derive(Debug, Clone)]
pub enum ScheduleKind {
    /// A system in the schedule
    /// `foo`
    System(Path), // TODO: This will require a pathed identifier for nice usage
    /// A schedule which must be executed in order
    /// `[ foo, bar ]`
    Ordered(Vec<P<Schedule>>),
    /// A schedule where the order is non-deterministic
    /// `{ foo, bar }`
    Unordered(Vec<P<Schedule>>),
    /// A schedule where the systems will be scheduled in parallel
    /// `| foo, bar |`
    Parallelized(Vec<P<Schedule>>),
}