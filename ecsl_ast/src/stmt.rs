use cfgrammar::Span;

use crate::{data::Variant, expr::Expr, item::Item, ty::{Mutable, Ty}, Ident, P};

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
    Let(Mutable, Ident, P<Ty>, P<Expr>),

    /// `if (*expr*) { .. }` Option of Else
    If(P<Expr>, P<Block>, Option<Expr>),
    /// `for (i : *ty* in *expr*) { .. }`
    For(Ident, P<Ty>, P<Expr>, P<Block>),
    /// `while (*expr*) { .. }`
    While(P<Expr>, P<Block>),

    /// match (*expr*) {
    ///     Variant1 { field: T } => { .. },
    ///     Variant2 => { .. },
    /// }
    Match(P<Expr>, Vec<P<MatchArm>>),

    /// Expression of any kind terminated by a semicolon
    Expr(P<Expr>),

    /// Empty Semi Colon
    Semi,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    span: Span,
    variant: P<Variant>,
    block: P<Block>,
}
