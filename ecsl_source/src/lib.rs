use std::{fs::File, io::Read, path::PathBuf};

use ecsl_span::{BytePos, LineNumber, SourceFileID};
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
}
