use cfgrammar::Span;

use crate::{stmt::Block, ty::{ConcreteGenerics, Ty}, SymbolId, P};

#[derive(Debug, Clone)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolId,
    pub generics: Option<ConcreteGenerics>,
    pub params: Vec<Param>,
    pub ret: RetTy,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub ident: SymbolId,
    pub ty: P<Ty>
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
