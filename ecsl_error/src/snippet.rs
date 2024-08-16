use std::path::PathBuf;

use ansi_term::Colour::Blue;
use ecsl_span::{LineData, LineNumber, Span};

#[derive(Debug)]
pub struct Snippet {
    file_path: PathBuf,
    full_span: Span,
    error_span: Span,
    lines: Vec<(LineData, String)>,
}

impl Snippet {
    pub fn from_source_span(
        path: PathBuf,
        full_span: Span,
        error_span: Span,
        lines: Vec<(LineData, String)>,
    ) -> Self {
        Self {
            file_path: path,
            full_span,
            error_span,
            lines,
        }
    }
}

impl std::fmt::Display for Snippet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_ln = &self
            .lines
            .iter()
            .max_by_key(|l| l.0.number().to_string())
            .unwrap()
            .1
            .to_string()
            .len();

        writeln!(
            f,
            " {}{} {}",
            format!("{: >1$}", " ", max_ln),
            Blue.paint("-->"),
            self.file_path.to_str().unwrap()
        )?;

        for (ln, string) in &self.lines {
            writeln!(
                f,
                " {} {}",
                Blue.paint(format!("{: >1$} |", ln.number(), max_ln)),
                string.trim_end()
            )?;
            writeln!(
                f,
                " {} {}",
                format!("{: >1$}", " ", max_ln),
                Blue.paint("|")
            )?;
        }

        Ok(())
    }
}
