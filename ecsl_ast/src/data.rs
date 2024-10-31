use cfgrammar::Span;

use crate::{item::Item, ty::{Generics, Ty}, Ident, P};

#[derive(Debug, Clone)]
pub struct StructDef {
    span: Span,
    kind: DataKind,
    ident: Ident,
    generics: P<Generics>,
    fields: Vec<P<Field>>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    span: Span,
    kind: DataKind,
    ident: Ident,
    generics: P<Generics>,
    fields: Vec<P<Variant>>,
}

#[derive(Debug, Clone)]
pub enum DataKind {
    Normal,
    Component,
}

#[derive(Debug, Clone)]
pub struct Field {
    span: Span,
    ident: Ident,
    ty: P<Ty>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    span: Span,
    ident: Ident,
    fields: Vec<P<Field>>,
}
