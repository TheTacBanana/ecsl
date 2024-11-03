use crate::{
    callable::FnDef, data::{EnumDef, StructDef}, ty::Ty, SymbolId, P
};
use cfgrammar::Span;

#[derive(Debug, Clone)]
pub struct Item {
    pub span: Span,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Use(P<UseDef>),
    /// Function or System Definition
    Fn(P<FnDef>),
    /// Struct or Component definition
    /// `struct Foo { field: Bar }`
    /// `struct comp Bar { field: Foo }`
    Struct(P<StructDef>),
    /// Enum definition
    /// `enum Foo {}`
    Enum(P<EnumDef>),
    /// Impl block for Struct, Component or Enum for concrete generic
    /// Impls can only contain Fn Definitions
    /// `impl Foo<Bar> {..}`
    Impl(P<ImplBlock>),
}

#[derive(Debug, Clone)]
pub struct UseDef {
    pub span: Span,
    pub path: P<UsePath>,
}

#[derive(Debug, Clone)]
pub enum UsePath {
    Super(Span, P<UsePath>),
    Single(Span, SymbolId, P<UsePath>),
    Multiple(Span, Vec<UsePath>),
    Item(Span, SymbolId),
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub span: Span,
    pub ty: P<Ty>,
    pub fn_defs: Vec<P<FnDef>>,
}
