use cfgrammar::Span;

use crate::{data::Variant, expr::Expr, ty::{Mutable, Ty}, SymbolId, P};

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `let mut ident : int = 1;`
    Let(Mutable, SymbolId, P<Ty>, P<Expr>),

    /// `if (*expr*) { .. }` Option of Else
    If(P<Expr>, P<Block>, Option<P<Stmt>>),
    /// `for (i : *ty* in *expr*) { .. }`
    For(SymbolId, P<Ty>, P<Expr>, P<Block>),
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
    pub span: Span,
    pub variant: P<Variant>,
    pub block: P<Block>,
}
