use cfgrammar::Span;

use crate::{
    expr::Expr,
    ty::{Mutable, Ty},
    SymbolId, P,
};

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

#[derive(Debug, Clone)]
pub struct QueryExpr {
    pub filters: Vec<QueryFilter>,
}

#[derive(Debug, Clone)]
pub struct QueryFilter {
    pub span: Span,
    pub kind: FilterKind,
}

impl QueryFilter {
    pub fn new(span: Span, kind: FilterKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum FilterKind {
    With(Vec<MutTy>),
    Without(Vec<Ty>),
    Added(Vec<Ty>),
    Removed(Vec<Ty>),
}

#[derive(Debug, Clone)]
pub struct MutTy {
    pub mutable: Mutable,
    pub ty: Ty,
}

impl MutTy {
    pub fn new(mutable: Mutable, ty: Ty) -> Self {
        Self { mutable, ty }
    }
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
