use std::{fs::File, io::Read, path::PathBuf};

use ecsl_diagnostics::Diagnostics;
use ecsl_error::{ext::EcslErrorExt, snippet::Snippet, EcslError, ErrorLevel};
use ecsl_span::{BytePos, LineNumber, SnippetLocation, SourceFileID, Span};
use lines::LineNumbers;

pub mod lines;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: SourceFileID,
    pub path: Option<PathBuf>,
    pub contents: String,
    pub file_size: usize,
    pub lines: LineNumbers,
}

impl SourceFile {
    /// Create a `SourceFile` from a `PathBuf`
    pub fn from_path(diag: &mut Diagnostics, path: PathBuf, id: SourceFileID) -> Self {
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents).unwrap();

        let mut source = Self::from_string(diag, contents, id);
        source.path = Some(path);
        source
    }

    pub fn from_string(diag: &mut Diagnostics, contents: String, id: SourceFileID) -> Self {
        let lines = LineNumbers::from(contents.as_str());

        // Find all non ascii characters
        let non_ascii = contents
            .chars()
            .enumerate()
            .filter_map(|(i, c)| (!c.is_ascii()).then(|| BytePos::new(i as u32)))
            .collect::<Vec<_>>();

        let source = SourceFile {
            id,
            path: None,
            file_size: contents.len(),
            contents,
            lines,
        };

        for pos in non_ascii {
            let span = Span::new(id, pos, pos);

            diag.push_error(
                EcslError::new(
                    ErrorLevel::Warning,
                    "Non-ASCII character used, may result in undefined parsing behaviour",
                )
                .with_span(|_| span)
                .with_snippet(|s| source.get_snippet(s.get_span().unwrap(), s.level())),
            )
        }

        source
    }

    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        self.lines.line_number(pos)
    }

    pub fn get_line_slice(&self, line: LineNumber) -> Option<&str> {
        let byte_slice = self.lines.byte_slice(line)?;
        Some(&self.contents[*byte_slice.0 as usize..=*byte_slice.1 as usize])
    }

    pub fn get_snippet(&self, error_span: Span, level: ErrorLevel) -> Snippet {
        let mut snippet_lines = Vec::new();

        // let (line, columnn) = self.lines.line_column_number(error_span.start());

        let full_span = Span::new(
            error_span.file(),
            self.lines.line_start(error_span.start()),
            self.lines.line_end(error_span.end()),
        );

        let (start, end) = self.lines.get_lines_from_span(error_span);
        for line in *start..=*end {
            let number = LineNumber::new(line);
            let contents = String::from(self.get_line_slice(number).unwrap());
            snippet_lines.push((number, contents))
        }

        Snippet::from_source_span(level, full_span, error_span, snippet_lines).unwrap()
    }
}
