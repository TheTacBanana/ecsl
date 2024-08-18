use std::{fs::File, io::Read, path::PathBuf};

use ecsl_error::snippet::Snippet;
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
    pub fn from_path(path: PathBuf, id: SourceFileID) -> Self {
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();

        let size = file.read_to_string(&mut contents).unwrap();
        let lines = LineNumbers::from(contents.as_str());

        SourceFile {
            path,
            source_id: id,
            contents: contents,
            file_size: size,
            lines,
        }
    }

    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        self.lines.line_number(pos)
    }

    pub fn get_line_slice(&self, line: LineNumber) -> Option<&str> {
        let byte_slice = self.lines.byte_slice(line)?;
        Some(&self.contents[*byte_slice.0 as usize..=*byte_slice.1 as usize])
    }

    pub fn get_snippet(&self, error_span: Span) -> Snippet {
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

        Snippet::from_source_span(location, full_span, error_span, snippet_lines)
    }
}

#[cfg(test)]
pub mod test {
    use ecsl_error::{EcslError, ErrorLevel, ErrorWithPath, ErrorWithSnippet};
    use ecsl_span::{BytePos, SourceFileID, Span};

    use crate::SourceFile;

    #[test]
    pub fn snippet() {
        let source = SourceFile::from_path("../example/src/main.ecsl".into(), SourceFileID::new(0));
        let span = Span::new(SourceFileID::new(0), BytePos::new(4), BytePos::new(16));

        let error = EcslError::spanned(ErrorLevel::Error, "With snippet", span);
        let snippet = source.get_snippet(span);
        println!("{}", ErrorWithSnippet::new(error, snippet));

        let error = EcslError::new(ErrorLevel::Error, "With no snippet");
        println!("{}", ErrorWithPath::new(error, source.path.clone()));
    }
}
