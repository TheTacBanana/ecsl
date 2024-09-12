use ecsl_span::Span;

use crate::{data::Field, ecs::{EntityTy, QueryTy}, Ident, P};


#[derive(Debug, Clone)]
pub struct Ty {
    span: Span,
    kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Ident with concrete generic type
    /// `Ident<Generic>`
    Ident(Ident, Option<Vec<P<Ty>>>),

    /// Array with associated size constant
    /// `[<ty> : N]`
    Array(P<Ty>, usize),

    /// Reference to Array
    /// `&[<ty>]`
    ArrayRef(P<Ty>),

    /// A struct with no identifier
    /// `{ field1: <ty>, field2: <ty>}`
    AnonStruct(Vec<P<Field>>),

    /// Refers to the type of the self object
    /// `Self`
    MethodSelf,

    /// Ref to Type with Mutability
    /// `&mut <ty>`
    Ref(Mutable, P<Ty>),
    /// Pointer to Type with Mutability
    /// `*const <ty>`
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