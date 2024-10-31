use cfgrammar::Span;

use crate::{stmt::Block, ty::Ty, Ident, P};

#[derive(Debug, Clone)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: Ident,
    pub params: Vec<Param>,
    pub ret: RetTy,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub ident: Ident,
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
