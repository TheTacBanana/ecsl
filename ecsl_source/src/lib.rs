use std::{fs::File, io::Read, path::PathBuf};

use ecsl_diagnostic::Diagnostics;
use ecsl_error::{snippet::Snippet, EcslError, ErrorLevel, ErrorWithSnippet};
use ecsl_span::{BytePos, LineData, LineNumber, SnippetLocation, SourceFileID, Span};
use lines::LineNumbers;

pub mod lines;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: PathBuf,
    pub source_id: SourceFileID,
    pub contents: String,
    pub file_size: usize,
    pub lines: LineNumbers,
}

impl SourceFile {
    /// Path should be valid file
    pub fn from_path(diag: &mut Diagnostics, path: PathBuf, id: SourceFileID) -> Self {
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();

        let size = file.read_to_string(&mut contents).unwrap();

        let non_ascii = contents
            .chars()
            .enumerate()
            .filter_map(|(i, c)| (!c.is_ascii()).then(|| BytePos::new(i as u32)))
            .collect::<Vec<_>>();

        let lines = LineNumbers::from(contents.as_str());

        let source = SourceFile {
            path,
            source_id: id,
            contents: contents,
            file_size: size,
            lines,
        };

        for pos in non_ascii {
            let span = Span::new(id, pos, pos);
            diag.push_error(ErrorWithSnippet::new(
                EcslError::spanned(
                    ErrorLevel::Warning,
                    "Non-ASCII character used, may result in undefined parsing behaviour",
                    span,
                ),
                source.get_snippet(span, ErrorLevel::Warning),
            ))
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

        let (line, columnn) = self.lines.line_column_number(error_span.start());
        let location = SnippetLocation::new(self.path.clone(), line, columnn);

        let full_span = Span::new(
            error_span.file(),
            self.lines.line_start(error_span.start()),
            self.lines.line_end(error_span.end()),
        );

        let (start, end) = self.lines.get_lines_from_span(error_span);
        for line in *start..=*end {
            let number = LineNumber::new(line);
            let contents = String::from(self.get_line_slice(number).unwrap());
            let data = LineData::new(number, contents.len());
            snippet_lines.push((data, contents))
        }

        Snippet::from_source_span(level, location, full_span, error_span, snippet_lines)
    }
}
