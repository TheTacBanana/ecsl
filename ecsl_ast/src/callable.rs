use cfgrammar::Span;

use crate::{
    stmt::Block,
    ty::{Generics, Mutable, Ty},
    SymbolId, P,
};

#[derive(Debug, Clone)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolId,
    pub generics: Option<Generics>,
    pub params: Vec<Param>,
    pub ret: RetTy,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub kind: ParamKind,
}

impl Param {
    pub fn new(span: Span, kind: ParamKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ParamKind {
    SelfValue(Mutable),
    SelfReference(Mutable),
    Normal(Mutable, SymbolId, P<Ty>), //TODO: Find a better name
}

#[derive(Debug, Clone, Copy)]
pub enum FnKind {
    Fn,
    Sys,
}

#[derive(Debug, Clone)]
pub enum RetTy {
    None(Span),
    Ty(P<Ty>),
}
