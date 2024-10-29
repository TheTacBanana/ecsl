use crate::{ty::Generics, Ident};
use cfgrammar::Span;

#[derive(Debug, Clone)]
pub struct Path {
    span: Span,
    segments: Vec<Segment>,
}

impl Path {
    pub fn new(span: Span, segments: Vec<Segment>) -> Self {
        Self { span, segments }
    }
}

#[derive(Debug, Clone)]
pub struct Segment {
    span: Span,
    ident: Ident,
    generics: Option<Generics>,
}

impl Segment {
    pub fn new(span: Span, ident: Ident, generics: Option<Generics>) -> Self {
        Self { span, ident, generics }
    }
}
