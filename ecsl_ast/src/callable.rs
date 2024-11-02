use cfgrammar::Span;

use crate::{stmt::Block, ty::{Generics, Ty}, SymbolId, P};

#[derive(Debug, Clone)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolId,
    pub generics: Option<P<Generics>>,
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

#[derive(Debug, Clone)]
pub enum FnKind {
    Fn,
    Sys,
}

#[derive(Debug, Clone)]
pub enum RetTy {
    None(Span),
    Ty(P<Ty>),
}
