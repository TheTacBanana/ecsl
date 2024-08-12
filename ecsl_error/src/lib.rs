use ecsl_source::span::Span;

pub struct EcslError {
    pub span: Option<Span>,
    pub kind: Box<dyn ErrorTrait>
}

impl EcslError {
    pub fn new<E: ErrorTrait + 'static>(kind: E) -> Self {
        EcslError {
            span: None,
            kind: Box::new(kind)
        }
    }

    pub fn spanned<E: ErrorTrait + 'static>(span: Span, kind: E) -> Self {
        EcslError {
            span: Some(span),
            kind: Box::new(kind)
        }
    }
}

pub trait ErrorTrait {
    fn fmt_err(&self) -> std::fmt::Result;
}
