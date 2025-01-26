use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

use crate::{
    stmt::Block,
    ty::{Generics, Mutable, Ty},
    P,
};

#[derive(Debug, Clone, AST)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolID,
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
            ident: self.ident,
            generics: self.generics.clone(),
            params: self.params.clone(),
            ret: self.ret.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnHeader {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolID,
    pub generics: Option<Generics>,
    pub params: Vec<Param>,
    pub ret: RetTy,
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
    Normal(Mutable, SymbolID, P<Ty>), //TODO: Find a better name
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FnKind {
    Fn,
    Sys,
}

#[derive(Debug, Clone, AST)]
pub enum RetTy {
    None(Span),
    Ty(P<Ty>),
}
