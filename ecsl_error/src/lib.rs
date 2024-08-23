use std::{error::Error, fmt::Debug, path::PathBuf};

use ansi_term::Colour::{Blue, Red, White, Yellow};
use ecsl_span::Span;
use snippet::Snippet;

pub mod snippet;

pub type EcslResult<T> = Result<T, EcslError>;

#[derive(Debug, Clone)]
pub struct EcslError {
    pub level: ErrorLevel,
    pub message: String,
    pub span: Option<Span>,
}

impl EcslError {
    pub fn new(level: ErrorLevel, kind: impl Into<String>) -> Self {
        EcslError {
            level,
            message: kind.into(),
            span: None,
        }
    }

    pub fn spanned(level: ErrorLevel, kind: impl Into<String>, span: Span) -> Self {
        EcslError {
            level,
            span: Some(span),
            message: kind.into(),
        }
    }
}

impl std::fmt::Display for EcslError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.level, self.message)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorLevel {
    Error,
    Warning,
    Note,
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ErrorLevel::Error => Red.paint("Error"),
            ErrorLevel::Warning => Yellow.paint("Warning"),
            ErrorLevel::Note => White.paint("Note"),
        };
        write!(f, "{}", s)
    }
}

pub trait ErrorExt<T> {
    fn ecsl_error(self, level: ErrorLevel) -> EcslResult<T>;
    fn ecsl_error_spanned(self, level: ErrorLevel, span: Span) -> EcslResult<T>;
}

impl<T, E: Error> ErrorExt<T> for Result<T, E> {
    fn ecsl_error(self, level: ErrorLevel) -> EcslResult<T> {
        match self {
            Err(e) => Err(EcslError::new(level, e.to_string())),
            Ok(ok) => Ok(ok),
        }
    }

    fn ecsl_error_spanned(self, level: ErrorLevel, span: Span) -> EcslResult<T> {
        match self {
            Err(e) => Err(EcslError::spanned(level, e.to_string(), span)),
            Ok(ok) => Ok(ok),
        }
    }
}

#[derive(Debug)]
pub enum CompleteError {
    ErrorWithPath(ErrorWithPath),
    ErrorWithSnippet(ErrorWithSnippet),
}

impl CompleteError {
    pub fn level(&self) -> ErrorLevel {
        match self {
            CompleteError::ErrorWithPath(e) => e.error.level,
            CompleteError::ErrorWithSnippet(e) => e.error.level,
        }
    }
}

impl std::fmt::Display for CompleteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompleteError::ErrorWithPath(e) => write!(f, "{}", e),
            CompleteError::ErrorWithSnippet(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorWithPath {
    error: EcslError,
    path: PathBuf,
}

impl ErrorWithPath {
    pub fn new(error: EcslError, path: PathBuf) -> Self {
        ErrorWithPath { error, path }
    }
}

impl std::fmt::Display for ErrorWithPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.error)?;
        writeln!(f, " {} {}", Blue.paint("-->"), self.path.to_str().unwrap())
    }
}

impl Into<CompleteError> for ErrorWithPath {
    fn into(self) -> CompleteError {
        CompleteError::ErrorWithPath(self)
    }
}

#[derive(Debug, Clone)]
pub struct ErrorWithSnippet {
    error: EcslError,
    snippet: Snippet,
}

impl ErrorWithSnippet {
    pub fn new(error: EcslError, snippet: Snippet) -> Self {
        ErrorWithSnippet { error, snippet }
    }
}

impl std::fmt::Display for ErrorWithSnippet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.error)?;
        writeln!(f, "{}", self.snippet)
    }
}

impl Into<CompleteError> for ErrorWithSnippet {
    fn into(self) -> CompleteError {
        CompleteError::ErrorWithSnippet(self)
    }
}
