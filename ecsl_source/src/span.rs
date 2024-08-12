use crate::{pos::BytePos, SourceFileID};

pub struct Span {
    file: SourceFileID,
    start: BytePos,
    end: BytePos,
}

impl Span {
    pub const fn new(file: SourceFileID, start: BytePos, end: BytePos) -> Self {
        Span { file, start, end }
    }

    pub fn extend_to(&mut self, end: BytePos) {
        self.end = end;
    }
}