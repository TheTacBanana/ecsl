

use std::fmt::Debug;

use ecsl_span::Span;

pub mod snippet;

pub type EcslResult<T> = Result<T, EcslError>;

#[derive(Debug)]
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

pub trait ErrorTrait : 'static + Debug{
    fn fmt_err(&self) -> String {
        format!("{:?}", self)
    }
}

impl<E : ErrorTrait> From<E> for EcslError {
    fn from(value: E) -> Self {
        EcslError::new(value)
    }
}

impl ErrorTrait for std::io::Error {
    fn fmt_err(&self) -> String {
        format!("{self}")
    }
}

impl ErrorTrait for String {
    fn fmt_err(&self) -> String {
        self.clone()
    }
}