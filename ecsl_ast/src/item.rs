use crate::{
    callable::FnDef,
    data::{EnumDef, StructDef},
    parse::Attributes,
    ty::{Generics, Ty},
    P,
};
use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

#[derive(Debug, Clone, AST)]
pub struct Item {
    pub span: Span,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
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

#[derive(Debug, Clone, AST)]
pub struct UseDef {
    pub span: Span,
    pub attributes: Option<Attributes>,
    pub path: P<UsePath>,
}

#[derive(Debug, Clone, AST)]
pub enum UsePath {
    Super(Span, P<UsePath>),
    Single(Span, SymbolID, P<UsePath>),
    Multiple(Span, Vec<UsePath>),
    Item(Span, SymbolID),
}

#[derive(Debug, Clone, AST)]
pub struct ImplBlock {
    pub span: Span,
    pub generics: Option<Generics>,
    pub ty: P<Ty>,
    pub fn_defs: Vec<FnDef>,
}
