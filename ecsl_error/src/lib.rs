use std::{
    fmt::{Debug, Display},
    path::PathBuf,
};

use ansi_term::{
    Colour,
    Colour::{Blue, Red, White, Yellow},
};

use cfgrammar::Span;
use snippet::Snippet;

pub mod ext;
pub mod snippet;

pub type EcslResult<T> = Result<T, EcslError>;

#[derive(Debug, Clone)]
pub struct EcslError {
    // Required
    level: ErrorLevel,
    message: String,

    // Optional
    span: Option<Span>,
    path: Option<PathBuf>,
    snippet: Option<Snippet>,
    notes: Vec<String>,
}

impl EcslError {
    pub fn new(level: ErrorLevel, message: impl Display) -> Self {
        Self {
            level,
            message: message.to_string(),
            span: None,
            path: None,
            snippet: None,
            notes: Vec::new(),
        }
    }

    pub fn level(&self) -> ErrorLevel {
        self.level
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn get_span(&self) -> Option<Span> {
        self.span
    }

    pub fn get_path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn get_snippet(&self) -> Option<&Snippet> {
        self.snippet.as_ref()
    }

    pub fn get_notes(&self) -> &Vec<String> {
        self.notes.as_ref()
    }
}

impl std::fmt::Display for EcslError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const HIGHLIGHT_COLOUR: Colour = Blue;

        // Write Message
        writeln!(f, "{}: {}", self.level(), self.message())?;

        // Output the path to the error
        match (self.get_path(), self.get_snippet()) {
            (Some(path), None) => writeln!(
                f,
                " {} {}",
                HIGHLIGHT_COLOUR.paint("-->"),
                path.to_str().unwrap()
            )?,
            (Some(path), Some(snippet)) => writeln!(
                f,
                " {}{} {}:{}",
                format!(" {: >1$}", " ", snippet.number_padding() as usize),
                HIGHLIGHT_COLOUR.paint("-->"),
                path.to_str().unwrap(),
                snippet.lnc()
            )?,
            _ => (),
        }

        // Write the snippet associated with the error
        if let Some(snippet) = self.get_snippet() {
            writeln!(f, "{}", snippet)?;

            for note in &self.notes {
                writeln!(
                    f,
                    "{}{} {}",
                    format!(" {: >1$} ", " ", snippet.number_padding() as usize),
                    HIGHLIGHT_COLOUR.paint("= Note:"),
                    note
                )?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorLevel {
    Error,
    Warning,
    Note,
}

impl ErrorLevel {
    pub fn colour(&self) -> Colour {
        match self {
            ErrorLevel::Error => Red,
            ErrorLevel::Warning => Yellow,
            ErrorLevel::Note => White,
        }
    }
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ErrorLevel::Error => "Error",
            ErrorLevel::Warning => "Warning",
            ErrorLevel::Note => "Note",
        };
        write!(f, "{}", self.colour().paint(s))
    }
}
