use crate::{
    parse::Attributes,
    stmt::Block,
    ty::{Generics, Mutable, Ty},
    P,
};
use cfgrammar::Span;
use ecsl_ast_derive::AST;
use ecsl_index::SymbolID;

#[derive(Debug, Clone, AST)]
pub struct FnDef {
    pub span: Span,
    pub kind: FnKind,
    pub ident: SymbolID,
    pub attributes: Attributes,
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

    pub fn ret_span(&self) -> Span {
        match &self.ret {
            RetTy::None(span) => *span,
            RetTy::Ty(ty) => ty.span,
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

    pub fn symbol(&self) -> SymbolID {
        self.kind.symbol()
    }
}

#[derive(Debug, Clone, AST)]
pub enum ParamKind {
    SelfValue(Mutable, SymbolID),
    SelfReference(Mutable, SymbolID),
    Normal(Mutable, SymbolID, P<Ty>), //TODO: Find a better name
}

impl ParamKind {
    pub fn symbol(&self) -> SymbolID {
        match self {
            ParamKind::SelfValue(_, symbol_id)
            | ParamKind::SelfReference(_, symbol_id)
            | ParamKind::Normal(_, symbol_id, _) => *symbol_id,
        }
    }
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
