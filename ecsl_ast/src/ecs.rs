use crate::{ty::Ty, P};
use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

#[derive(Debug, Clone, AST)]
pub struct EntityTy {
    pub bounds: Vec<EntityAttribute>,
}

#[derive(Debug, Clone, AST)]
pub struct EntityAttribute {
    pub span: Span,
    pub ident: SymbolID,
    pub ty: P<Ty>,
}

#[derive(Debug, Clone, AST)]
pub struct QueryExpr {
    pub filters: Vec<QueryFilter>,
}

#[derive(Debug, Clone, AST)]
pub struct QueryFilter {
    pub span: Span,
    pub kind: FilterKind,
    pub items: Vec<Ty>,
}

impl QueryFilter {
    pub fn new(span: Span, kind: FilterKind, items: Vec<Ty>) -> Self {
        Self { span, kind, items }
    }
}

#[derive(Debug, Clone, AST)]
pub enum FilterKind {
    With,
    Without,
    Added,
    Removed,
}

#[derive(Debug, Clone, AST)]
pub struct Schedule {
    pub span: Span,
    pub kind: ScheduleKind,
}

impl Schedule {
    pub fn new(span: Span, kind: ScheduleKind) -> Self {
        Self { span, kind }
    }

    pub fn is_empty(&self) -> bool {
        match &self.kind {
            ScheduleKind::Sys(_, _) => false,
            ScheduleKind::Ordered(schedules) => schedules.iter().all(|s| s.is_empty()),
            ScheduleKind::Unordered(schedules) => schedules.iter().all(|s| s.is_empty()),
        }
    }
}

#[derive(Debug, Clone, AST)]
pub enum ScheduleKind {
    /// A plain system
    /// `foo` 'Bar->foo'
    Sys(Option<SymbolID>, SymbolID),

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
