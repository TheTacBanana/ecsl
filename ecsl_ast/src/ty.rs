use crate::{ecs::EntityTy, SymbolId, P};

use cfgrammar::Span;


#[derive(Debug, Clone)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl Ty {
    pub fn new(span: Span, kind: TyKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Identifier
    Ident(SymbolId, Option<ConcreteGenerics>),

    /// Array with associated size constant
    // Includes span of constant
    /// `[<ty> : N]`
    Array(P<Ty>, Span),

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

    /// Query Type
    /// `Query`
    Query,

    /// System Type
    /// `-> System`
    System,

    /// Schedule Type
    /// `-> Schedule`
    Schedule,
}

#[derive(Debug, Clone, Copy)]
pub enum Mutable {
    Imm,
    Mut,
}

#[derive(Debug, Clone)]
pub struct Generics {
    pub span: Span,
    pub params: Vec<GenericParam>
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub span: Span,
    pub ident: SymbolId,
}

#[derive(Debug, Clone)]
pub struct ConcreteGenerics {
    pub span: Span,
    pub params: Vec<Ty>
}
