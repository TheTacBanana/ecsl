use cfgrammar::Span;

use crate::{ty::{Generics, Ty}, SymbolId, P};

#[derive(Debug, Clone)]
pub struct StructDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: P<Generics>,
    pub fields: Vec<P<FieldDef>>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: P<Generics>,
    pub fields: Vec<P<Variant>>,
}

#[derive(Debug, Clone)]
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
pub struct Variant {
    pub span: Span,
    pub ident: SymbolId,
    pub fields: Vec<P<FieldDef>>,
}
