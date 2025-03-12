use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_bytecode::BytecodeInstruction;
use ecsl_index::SymbolID;

use crate::{
    expr::Expr,
    ty::{Mutable, Ty},
    P,
};

#[derive(Debug, Clone, AST)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, AST)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
pub enum StmtKind {
    /// `let mut ident : int = 1;`
    Let(Mutable, SymbolID, Span, Option<P<Ty>>, P<Expr>),

    /// `if (*expr*) { .. }` Option of Else
    If(P<Expr>, P<Block>, Option<P<Stmt>>),
    /// `.. else if (*expr*) {}` Option of Else
    ElseIf(P<Expr>, P<Block>, Option<P<Stmt>>),
    /// `.. else`
    Else(P<Block>),

    /// `for (*symbol* : *ty* in *expr*) { .. }`
    For(SymbolID, P<Ty>, P<Expr>, P<Block>),
    /// `while (*expr*) { .. }`
    While(P<Expr>, P<Block>),

    /// match (*expr*) {
    ///     Variant1 { field } -> { .. },
    ///     Variant2 -> { .. },
    /// }
    Match(P<Expr>, Vec<MatchArm>),

    /// `break` loop
    Break,
    /// `continue` loop
    Continue,
    /// Return Value
    /// `return 1`
    Return(Option<P<Expr>>),

    /// Expression of any kind terminated by a semicolon
    Expr(P<Expr>),

    // Inline Bytecode
    BYT(Vec<InlineBytecode>),

    /// Empty Semi Colon
    Semi,
}

#[derive(Debug, Clone, AST)]
pub struct MatchArm {
    pub span: Span,
    pub ident: Option<SymbolID>,
    pub fields: Vec<Field>,
    pub block: P<Block>,
}

#[derive(Debug, Clone, AST)]
pub struct Field {
    pub span: Span,
    pub ident: SymbolID,
}

#[derive(Debug, Clone, AST)]
pub struct InlineBytecode {
    pub span: Span,
    pub ins: BytecodeInstruction,
}
