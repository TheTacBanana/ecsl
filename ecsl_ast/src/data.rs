use cfgrammar::Span;

use crate::{ty::{Generics, Ty}, SymbolId, P};

#[derive(Debug, Clone)]
pub struct StructDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: Option<P<Generics>>,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: Option<P<Generics>>,
    pub variants: Vec<VariantDef>,
}

#[derive(Debug, Clone, Copy)]
pub enum DataKind {
    Normal,
    Component,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub span: Span,
    pub ident: SymbolId,
    pub ty: P<Ty>,
}

#[derive(Debug, Clone)]
pub struct VariantDef {
    pub span: Span,
    pub ident: SymbolId,
    pub fields: Vec<FieldDef>,
}
