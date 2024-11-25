use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

use crate::{expr::Expr, ty::Ty, P};

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
}

impl QueryFilter {
    pub fn new(span: Span, kind: FilterKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
pub enum FilterKind {
    With(Vec<Ty>),
    Without(Vec<Ty>),
    Added(Vec<Ty>),
    Removed(Vec<Ty>),
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
}

#[derive(Debug, Clone, AST)]
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
