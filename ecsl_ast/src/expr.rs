use cfgrammar::Span;

use crate::{
    ecs::{QueryExpr, Schedule},
    ty::{Mutable, Ty},
    SymbolId, P,
};

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Assign expression to ident
    /// `ident = *expr*`
    Assign(SymbolId, P<Expr>),

    /// Create a reference to an expression
    /// `&foo &mut bar`
    Ref(Mutable, P<Expr>),

    /// Unary Operator
    /// `-value`
    UnOp(UnOpKind, P<Expr>),
    /// Binary Operator
    /// `l + r`
    BinOp(BinOpKind, P<Expr>, P<Expr>),

    /// Array `[1, 2, 3, 4]`
    Array(Vec<Expr>),
    /// Locally accesible symbol
    /// `foo` `bar`
    Ident(SymbolId),
    /// Literal value
    /// `1`
    /// `"string"`
    Lit(Literal),
    /// Struct `Foo { bar : 1 }`
    Struct(SymbolId, Vec<FieldExpr>),
    /// Enum `Foo::Bar { baz : 2 }`
    Enum(SymbolId, SymbolId, Vec<FieldExpr>),

    /// Casting expression into type
    /// `6 as int`
    Cast(P<Expr>, P<Ty>),
    /// Field Access
    /// `foo.bar`
    Field(P<Expr>, SymbolId),
    /// Function call
    /// Called On, Function Ident, Args
    /// `x.foo(1, 2)`
    Function(Option<P<Expr>>, SymbolId, Vec<Expr>),

    /// `break` loop
    Break,
    /// `continue` loop
    Continue,
    /// Return Value
    /// `return 1`
    Return(Option<P<Expr>>),

    // ECS Features
    /// Use of the Entity Keyword to create new Entities
    Entity,
    /// Use of the Resource Keyword to access resources
    Resource,
    /// Query the world to get an iterator of components
    Query(P<QueryExpr>),
    /// Define a order for a series of system
    /// ```
    /// Schedule [
    ///     { foo, bar },
    ///     baz
    /// ]
    /// ```
    Schedule(P<Schedule>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int,
    Float,
    String,
    Char,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOpKind {
    Neg,
    Not,
    Deref
}

#[derive(Debug, Clone)]
pub struct FieldExpr {}
