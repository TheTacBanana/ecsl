use ecsl_span::Span;

use crate::{data::Variant, expr::Expr, item::Item, ty::Mutable, Ident, P};

#[derive(Debug, Clone)]
pub struct Block {
    span: Span,
    stmts: Vec<P<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `let mut ident : int = 1;`
    Let(Mutable, Ident, P<Expr>),

    /// Item
    Item(P<Item>),

    /// `if (*expr*) { .. }` Option of Else
    If(P<Expr>, P<Block>, Option<Expr>),
    /// `for (i in *expr*) { .. }`
    For(Ident, P<Expr>),
    /// `while (*expr*) { .. }`
    While(P<Expr>, P<Block>),

    /// match (*expr*) {
    ///     Variant1 { field: T } => { .. },
    ///     Variant2 => { .. },
    /// }
    Match(P<Expr>, Vec<P<MatchArm>>),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    span: Span,
    variant: P<Variant>,
    block: P<Block>,
}
