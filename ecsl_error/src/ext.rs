use crate::{snippet::Snippet, EcslError};
use cfgrammar::Span;
use ecsl_index::SourceFileID;
use std::path::PathBuf;

/// Trait Extension of `Result<T, EcslError>` and `EcslError` to add info
pub trait EcslErrorExt<T> {
    fn with_span(self, span: impl Fn(&EcslError) -> Span) -> T;
    fn with_file(self, file: impl Fn(&EcslError) -> SourceFileID) -> T;
    fn with_path(self, path: impl Fn(&EcslError) -> PathBuf) -> T;
    fn with_snippet(self, snippet: impl Fn(&EcslError) -> Snippet) -> T;
    fn with_note(self, note: impl Fn(&EcslError) -> String) -> T;
}

impl<T> EcslErrorExt<Result<T, EcslError>> for Result<T, EcslError> {
    fn with_span(self, span: impl Fn(&EcslError) -> Span) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_span(span)),
        }
    }

    fn with_file(self, file: impl Fn(&EcslError) -> SourceFileID) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_file(file)),
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

    fn with_note(self, note: impl Fn(&EcslError) -> String) -> Result<T, EcslError> {
        match self {
            Ok(e) => Ok(e),
            Err(e) => Err(e.with_note(note)),
        }
    }
}

impl EcslErrorExt<EcslError> for EcslError {
    fn with_span(mut self, span: impl Fn(&EcslError) -> Span) -> EcslError {
        self.span = Some(span(&self));
        self
    }

    fn with_file(mut self, file: impl Fn(&EcslError) -> SourceFileID) -> EcslError {
        self.file = Some(file(&self));
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

    fn with_note(mut self, note: impl Fn(&EcslError) -> String) -> EcslError {
        self.notes.push(note(&self));
        self
    }
}

impl<'a> EcslErrorExt<Self> for &'a mut EcslError {
    fn with_span(self, span: impl Fn(&EcslError) -> Span) -> &'a mut EcslError {
        self.span = Some(span(&self));
        self
    }

    fn with_file(self, file: impl Fn(&EcslError) -> SourceFileID) -> &'a mut EcslError {
        self.file = Some(file(&self));
        self
    }

    fn with_path(self, path: impl Fn(&EcslError) -> PathBuf) -> &'a mut EcslError {
        self.path = Some(path(&self));
        self
    }

    fn with_snippet(self, snippet: impl Fn(&EcslError) -> Snippet) -> &'a mut EcslError {
        self.snippet = Some(snippet(&self));
        self
    }

    fn with_note(self, note: impl Fn(&EcslError) -> String) -> &'a mut EcslError {
        self.notes.push(note(&self));
        self
    }
}
