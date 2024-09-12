use ecsl_span::Span;

use crate::{ecs::QueryExpr, op::{BinOp, UnOp}, stmt::Block, ty::Ty, Ident, P};


#[derive(Debug, Clone)]
pub struct Expr {
    span: Span,
    kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Assign expression to ident
    /// `ident = *expr*`
    Assign(Ident, P<Expr>),

    /// Unary Operator
    /// `-value`
    UnOp(UnOp, P<Expr>),
    /// Binary Operator
    /// `l + r`
    BinOp(BinOp, P<Expr>, P<Expr>),

    /// Array `[1, 2, 3, 4]`
    Array(Vec<P<Expr>>),
    /// Literal value
    /// `1`
    /// `"string"`
    Lit(Literal),

    /// Casting expression into type
    /// `6 as int`
    Cast(P<Expr>, P<Ty>),
    /// Field Access
    /// `foo.bar`
    Field(P<Expr>, Ident),
    /// Function call
    /// Called On, Function Ident, Args
    /// `x.foo(1, 2)`
    Function(Option<P<Expr>>, P<Ident>, Option<Vec<P<Expr>>>),

    /// `break` loop
    Break,
    /// `continue` loop
    Continue,
    /// Return Value
    /// `return 1`
    Return(Option<P<Expr>>),

    // ECS Features
    /// Use of the Resource key word to access resources
    Resource,
    /// Query the world to get an iterator of components
    Query(P<QueryExpr>),

}

#[derive(Debug, Clone)]
pub enum Literal {
    Int,
    Float,
    String,
    Char,
}