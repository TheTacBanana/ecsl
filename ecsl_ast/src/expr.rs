use crate::{
    ecs::{QueryExpr, Schedule},
    ty::{ConcreteGenerics, Mutable, Ty},
    P,
};
use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

#[derive(Debug, Clone, AST)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
pub enum ExprKind {
    /// Assign expression to ident
    /// `ident = *expr*`
    Assign(SymbolID, Span, P<Expr>),

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
    Ident(SymbolID),
    /// Self in method
    /// `self`
    MethodSelf,
    /// Literal value
    /// `1`
    /// `"string"`
    Lit(Literal),
    /// Struct `Foo { bar : 1 }`
    Struct(P<Ty>, Vec<FieldExpr>),
    /// Enum `Foo::Bar { baz : 2 }`
    Enum(P<Ty>, SymbolID, Vec<FieldExpr>),

    Range(P<Expr>, P<Expr>, RangeType),

    /// Casting expression into type
    /// `6 as int`
    Cast(P<Expr>, P<Ty>),
    /// Field Access
    /// `foo.bar`
    Field(P<Expr>, SymbolID),
    /// Function call
    /// Called On, Function Ident, Args
    /// `x.foo(1, 2)`
    Function(
        Option<P<Expr>>,
        Option<ConcreteGenerics>,
        SymbolID,
        Vec<Expr>,
    ),

    // ECS Features
    /// Use of the Entity Keyword to create new Entities
    Entity,
    /// Use of the Resource Keyword to access resources
    Resource,
    /// Query the world to get an iterator of components
    Query(P<QueryExpr>),
    /// Define a order for a series of system
    /// ```ignore
    /// Schedule [
    ///     { foo, bar },
    ///     baz
    /// ]
    /// ```
    Schedule(P<Schedule>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    Int,
    Float,
    String,
    Char,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntKind {
    /// I32
    Int,
    /// U32 or 0U
    UInt,
    /// I64 or 0L
    Long,
    /// U64 or 0UL
    ULong,
    /// I8 or 0B
    Byte,
    /// U8 or 0UB
    UByte,
}

impl IntKind {
    pub fn from_str(s: &str) -> Self {
        match s {
            "U" => Self::UInt,
            "L" => Self::Long,
            "UL" => Self::ULong,
            "B" => Self::Byte,
            "UB" => Self::UByte,
            "" => Self::Int,
            _ => panic!("Invalid Int Kind"),
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int => write!(f, "int"),
            Literal::Float => write!(f, "float"),
            Literal::String => write!(f, "string"),
            Literal::Char => write!(f, "char"),
            Literal::Bool => write!(f, "bool"),
        }
    }
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

impl std::fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::And => write!(f, "&&"),
            BinOpKind::Or => write!(f, "||"),
            BinOpKind::Eq => write!(f, "=="),
            BinOpKind::Neq => write!(f, "!="),
            BinOpKind::Lt => write!(f, "<"),
            BinOpKind::Leq => write!(f, "<="),
            BinOpKind::Gt => write!(f, ">"),
            BinOpKind::Geq => write!(f, ">="),
        }
    }
}

impl BinOpKind {
    pub fn operation(&self) -> bool {
        match self {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => true,
            _ => false,
        }
    }

    pub fn comparsion(&self) -> bool {
        match self {
            BinOpKind::Eq
            | BinOpKind::Neq
            | BinOpKind::Lt
            | BinOpKind::Leq
            | BinOpKind::Gt
            | BinOpKind::Geq => true,
            _ => false,
        }
    }

    pub fn boolean_logic(&self) -> bool {
        match self {
            BinOpKind::And | BinOpKind::Or | BinOpKind::Eq | BinOpKind::Neq => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOpKind {
    Neg,
    Not,
    Deref,
}

impl std::fmt::Display for UnOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Neg => write!(f, "-"),
            UnOpKind::Not => write!(f, "!"),
            UnOpKind::Deref => write!(f, "*"),
        }
    }
}

#[derive(Debug, Clone, AST)]
pub struct FieldExpr {
    pub span: Span,
    pub ident: SymbolID,
    pub expr: P<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RangeType {
    Exclusive,
    Inclusive,
}
