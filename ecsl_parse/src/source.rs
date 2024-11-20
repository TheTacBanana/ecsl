use std::{fs::File, io::Read, path::PathBuf};

use cfgrammar::Span;
use ecsl_diagnostics::Diagnostics;
use ecsl_error::{ext::EcslErrorExt, snippet::Snippet, EcslError, ErrorLevel};
use ecsl_index::SourceFileID;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::{Lexer, LexerTypes, NonStreamingLexer};

use crate::{LexerDef, LexerTy, LEXER_DEF};

pub struct SourceFile {
    pub id: SourceFileID,
    pub path: Option<PathBuf>,
    pub contents: String,
    pub file_size: usize,
}

impl SourceFile {
    /// Create a `SourceFile` from a `PathBuf`
    pub fn from_path(path: PathBuf, id: SourceFileID) -> Self {
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents).unwrap();

        let mut source = Self::from_string(contents, id);
        source.path = Some(path);
        source
    }

    pub fn from_string(contents: String, id: SourceFileID) -> Self {
        SourceFile {
            id,
            path: None,
            file_size: contents.len(),
            contents,
        }
    }

    pub fn lexer(&self) -> LexerTy {
        LEXER_DEF.lexer(&self.contents)
    }

    // pub fn get_snippet(&self, error_span: Span, level: ErrorLevel) -> Snippet {
    //     let mut snippet_lines = Vec::new();

    //     let lnc = self.lines.line_number_column(error_span.start());

    //     let full_span = Span::new(
    //         self.lines.line_start(error_span.start()),
    //         self.lines.line_end(error_span.end()),
    //     );

    //     let (start, end) = self.lines.get_lines_from_span(error_span);
    //     for line in start..=end {
    //         let contents = String::from(self.get_line_slice(line).unwrap());
    //         snippet_lines.push((line, contents))
    //     }

    //     Snippet::from_source_span(level, full_span, error_span, snippet_lines, lnc).unwrap()
    // }
}
