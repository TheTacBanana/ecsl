use ecsl_span::Span;

use crate::{callable::FnDef, data::{EnumDef, StructDef}, stmt::Block, ty::Ty, Ident, P};

#[derive(Debug, Clone)]
pub enum Item {
    Use(), //TODO:
    /// Function or System Definition
    Fn(P<FnDef>),
    /// Struct or Component definition
    /// `struct Foo { field: Bar }`
    /// `comp Bar { field: Foo }`
    Struct(P<StructDef>),
    /// Enum definition
    /// `enum Foo {}`
    Enum(P<EnumDef>),
    /// Impl block for Struct, Component or Enum for concrete generic
    /// `impl Foo<Bar> {..}`
    Impl(P<ImplBlock>),
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    span: Span,
    ty: P<Ty>,
    block: P<Block>
}