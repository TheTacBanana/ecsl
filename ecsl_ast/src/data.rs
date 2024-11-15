use cfgrammar::Span;
use ecsl_ast_derive::AST;

use crate::{ty::{Generics, Ty}, SymbolId, P};

#[derive(Debug, Clone, AST)]
pub struct StructDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: Option<Generics>,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone, AST)]
pub struct EnumDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolId,
    pub generics: Option<Generics>,
    pub variants: Vec<VariantDef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataKind {
    Normal,
    Component,
}

#[derive(Debug, Clone, AST)]
pub struct FieldDef {
    pub span: Span,
    pub ident: SymbolId,
    pub ty: P<Ty>,
}

#[derive(Debug, Clone, AST)]
pub struct VariantDef {
    pub span: Span,
    pub ident: SymbolId,
    pub fields: Vec<FieldDef>,
}
