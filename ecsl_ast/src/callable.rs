use cfgrammar::Span;
use ecsl_ast_derive::AST;

use crate::{
    stmt::Block,
    ty::{Generics, Mutable, Ty},
    SymbolId, P,
};

#[derive(Debug, Clone, AST)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolId,
    pub generics: Option<Generics>,
    pub params: Vec<Param>,
    pub ret: RetTy,
    pub block: Block,
}

impl FnDef {
    pub fn to_header(&self) -> FnHeader {
        FnHeader {
            span: self.span,
            kind: self.kind,
            generics: self.generics.as_ref().map(|g| g.params.len()),
            params: self.params.len(),
            ret: matches!(self.ret, RetTy::Ty(_)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FnHeader {
    pub span: Span,
    pub kind: FnKind,
    pub generics: Option<usize>,
    pub params: usize,
    pub ret: bool,
}

#[derive(Debug, Clone, AST)]
pub struct Param {
    pub span: Span,
    pub kind: ParamKind,
}

impl Param {
    pub fn new(span: Span, kind: ParamKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, AST)]
pub enum ParamKind {
    SelfValue(Mutable),
    SelfReference(Mutable),
    Normal(Mutable, SymbolId, P<Ty>), //TODO: Find a better name
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnKind {
    Fn,
    Sys,
}

#[derive(Debug, Clone, AST)]
pub enum RetTy {
    None(Span),
    Ty(P<Ty>),
}
