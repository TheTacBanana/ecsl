use cfgrammar::Span;

use crate::{stmt::Block, ty::Ty, Ident, P};

#[derive(Debug, Clone)]
pub struct FnDef {
    span: Span,
    kind: FnKind,
    ident: Ident,
    params: Vec<Param>,
    ret: RetTy,
    block: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    span: Span,
    ident: Ident,
    ty: P<Ty>
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
