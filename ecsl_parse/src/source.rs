use std::{fs::File, io::Read, path::PathBuf};

use crate::{LexerTy, LEXER_DEF};
use ecsl_error::{snippet::Snippet, ErrorLevel};
use ecsl_index::{LineNumberColumn, PackageID, SourceFileID};
use lrpar::{NonStreamingLexer, Span};

#[derive(Debug)]
pub struct SourceFile {
    pub id: SourceFileID,
    pub cr: PackageID,

    pub path: PathBuf,
    pub contents: String,
    pub file_size: usize,
}

impl SourceFile {
    /// Create a `SourceFile` from a `PathBuf`
    pub fn from_path(path: PathBuf, id: SourceFileID, cr: PackageID) -> Self {
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents).unwrap();

        SourceFile {
            id,
            cr,
            path,
            file_size: contents.len(),
            contents,
        }
    }

    pub fn lexer(&self) -> LexerTy {
        LEXER_DEF.lexer(&self.contents)
    }

    pub fn get_snippet(&self, err_span: Span, level: ErrorLevel, lexer: &LexerTy) -> Snippet {
        let ((sl, sc), _) = lexer.line_col(err_span);

        let lnc = LineNumberColumn::new(sl, sc);

        let new_start = i32::max(err_span.start() as i32 - (sc - 1) as i32, 0) as usize;
        let full_span = Span::new(new_start, err_span.end());

        let mut lines = Vec::new();
        for (i, line) in lexer
            .span_lines_str(full_span)
            .split_inclusive('\n')
            .enumerate()
        {
            lines.push((sl + i, line.to_string()));
        }

        Snippet::from_source_span(level, full_span, err_span, lines, lnc).unwrap()

        //TODO: Multiline snippets

        // let new_end = (|| {
        //     let current_el = el;

        //     for i in err_span.end()..self.file_size {
        //         let (_, (el, _)) = lexer.line_col(Span::new(i, i));
        //         if el > current_el {
        //             return i;
        //         }
        //     }
        //     return err_span.end();
        // })();

        // let temp_span = Span::new(new_start, err_span.end());
        // let ((sl, sc), (_, _)) = lexer.line_col(temp_span);

        // let new_start = i32::max(temp_span.start() as i32 - sc as i32 + 1, 0) as usize;
        // let full_span = Span::new(new_start, new_end);

        // println!("{}")
    }
}
