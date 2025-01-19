use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

use crate::{
    parse::Attributes,
    ty::{Generics, Ty},
    P,
};

#[derive(Debug, Clone, AST)]
pub struct StructDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolID,
    pub attributes: Attributes,
    pub generics: Option<Generics>,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone, AST)]
pub struct EnumDef {
    pub span: Span,
    pub kind: DataKind,
    pub ident: SymbolID,
    pub attributes: Attributes,
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
    pub ident: SymbolID,
    pub ty: P<Ty>,
}

#[derive(Debug, Clone, AST)]
pub struct VariantDef {
    pub span: Span,
    pub ident: SymbolID,
    pub fields: Vec<FieldDef>,
}
