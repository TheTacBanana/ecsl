use crate::{ecs::EntityTy, P};

use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

#[derive(Debug, Clone, AST)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl Ty {
    pub fn new(span: Span, kind: TyKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
pub enum TyKind {
    /// Identifier
    Ident(SymbolID, Option<ConcreteGenerics>),

    /// Array with associated size constant
    // Includes span of constant
    /// `[<ty> : N]`
    Array(P<Ty>, usize),

    /// Reference to Array
    /// `&[<ty>]`
    ArrayRef(Mutable, P<Ty>),

    /// Ref to Type with Mutability
    /// `&mut <ty>`
    Ref(Mutable, P<Ty>),
    /// Pointer to Type with Mutability
    /// `*imm <ty>`
    /// `*mut <ty>`
    Ptr(Mutable, P<Ty>),

    // ECS Features
    /// An Entity Type with encoded type info about the attatched components
    /// `Entity`
    /// `Entity<foo: Foo, bar: Bar>`
    /// `Entity<foo: Foo, ..>`
    Entity(EntityTy),

    /// Schedule Type
    /// `-> Schedule`
    Schedule,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutable {
    Imm,
    Mut,
}

#[derive(Debug, Clone, AST)]
pub struct Generics {
    pub span: Span,
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Clone, AST)]
pub struct GenericParam {
    pub span: Span,
    pub ident: SymbolID,
}

#[derive(Debug, Clone, AST)]
pub struct ConcreteGenerics {
    pub span: Span,
    pub params: Vec<Ty>,
}
