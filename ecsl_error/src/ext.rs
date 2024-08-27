use std::path::PathBuf;

use ecsl_span::Span;

use crate::{snippet::Snippet, EcslError};

/// Trait Extension of `Result<T, EcslError>` and `EcslError` to add info
pub trait EcslErrorExt<T> {
    fn with_span(self, span: impl Fn(&EcslError) -> Span) -> T;
    fn with_path(self, path: impl Fn(&EcslError) -> PathBuf) -> T;
    fn with_snippet(self, snippet: impl Fn(&EcslError) -> Snippet) -> T;
}

impl<T> EcslErrorExt<Result<T, EcslError>> for Result<T, EcslError> {
    fn with_span(self, span: impl Fn(&EcslError) -> Span) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_span(span)),
        }
    }

    fn with_path(self, path: impl Fn(&EcslError) -> PathBuf) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_path(path)),
        }
    }

    fn with_snippet(self, snippet: impl Fn(&EcslError) -> Snippet) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_snippet(snippet)),
        }
    }
}

impl EcslErrorExt<EcslError> for EcslError {
    fn with_span(mut self, span: impl Fn(&EcslError) -> Span) -> EcslError {
        self.span = Some(span(&self));
        self
    }

    fn with_path(mut self, path: impl Fn(&EcslError) -> PathBuf) -> EcslError {
        self.path = Some(path(&self));
        self
    }

    fn with_snippet(mut self, snippet: impl Fn(&EcslError) -> Snippet) -> EcslError {
        self.snippet = Some(snippet(&self));
        self
    }
}
