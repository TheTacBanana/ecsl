use std::fmt::Debug;

use ecsl_span::Span;

pub mod snippet;

pub type EcslResult<T> = Result<T, EcslError>;

#[derive()]
pub struct EcslError {
    pub span: Option<Span>,
    pub kind: Box<dyn ErrorTrait>
}

impl EcslError {
    pub fn new<E: ErrorTrait>(kind: E) -> Self {
        EcslError {
            span: None,
            kind: Box::new(kind)
        }
    }

    pub fn spanned<E: ErrorTrait>(span: Span, kind: E) -> Self {
        EcslError {
            span: Some(span),
            kind: Box::new(kind)
        }
    }
}

pub trait ErrorTrait : 'static{
    // fn fmt_err(&self) -> std::fmt::Result;
}
