use ecsl_span::{BytePos, LineNumber};
use lines::LineNumbers;

pub mod lines;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: String,
    pub contents: String,
    pub lines: LineNumbers,
    //TODO: Crate ID
}

impl SourceFile {
    pub fn from_string() -> Self {
        todo!()
        // SourceFile {
        //     lines: LineNumbers::from(contents.as_str()),
        //     path,
        //     contents,
        // }
    }

    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        self.lines.line_number(pos)
    }

}
