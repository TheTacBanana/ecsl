use crate::{ecs::{EntityTy, QueryTy}, expr::Literal, Ident, P};

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
    Ident(Ident),

    /// Array with associated size constant
    // Includes span of constant
    /// `[<ty> : N]`
    Array(P<Ty>, Span),

    /// Reference to Array
    /// `&[<ty>]`
    ArrayRef(P<Ty>),

    /// Refers to the type of the self object
    /// `Self`
    MethodSelf,

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

    /// Resource Type with one and only one generic parameter
    /// T must be a Component
    /// `Resource<T>`
    /// `Resource<mut T>`
    Resource(Mutable, P<Ty>),

    /// Query Type
    /// `Query<With<foo: mut T>>`
    /// `Query<Without<T>>`
    Query(QueryTy),

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
    span: Span,
    vec: Vec<GenericParam>
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    span: Span,
    ident: Ident,
}