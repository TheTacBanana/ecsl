use index::{BytePos, LineNumber, SourceFileID};

pub mod index;

#[derive(Debug)]
pub struct Spanned<T> {
    item: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Spanned<T> {
        Spanned { item, span }
    }

    pub fn item(&self) -> &T {
        &self.item
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

/// Range is inclusive [start..end]
#[derive(Debug, Clone, Copy)]
pub struct Span {
    file: SourceFileID,
    start: BytePos,
    end: BytePos,
}

impl Span {
    pub const fn new(file: SourceFileID, start: BytePos, end: BytePos) -> Self {
        Span { file, start, end }
    }
    pub const fn file(&self) -> SourceFileID {
        self.file
    }

    pub const fn start(&self) -> BytePos {
        self.start
    }

    pub const fn end(&self) -> BytePos {
        self.end
    }

    pub fn extend_to(&mut self, end: BytePos) {
        self.end = end;
    }

    pub fn contains(&self, other: &Self) -> bool {
        self.file == other.file && self.start <= other.start && self.end >= other.end
    }

    pub fn overlaps(&self, other: &Self) -> bool {
        self.file == other.file && (self.start <= other.end || self.end >= other.start)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineNumberColumn {
    ln: LineNumber,
    col: usize,
}

impl LineNumberColumn {
    pub fn new(ln: LineNumber, col: usize) -> Self {
        Self { ln, col }
    }

    pub fn ln(&self) -> LineNumber {
        self.ln
    }

    pub fn col(&self) -> usize {
        self.col
    }
}

impl std::fmt::Display for LineNumberColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:{}", self.ln.inner() + 1, self.col + 1)
    }
}

#[cfg(test)]
pub mod test {
    use crate::{BytePos, SourceFileID, Span};

    #[test]
    pub fn contains() {
        let inner = Span::new(SourceFileID::new(0), BytePos::new(5), BytePos::new(9));
        let outer = Span::new(SourceFileID::new(0), BytePos::new(2), BytePos::new(12));

        assert!(outer.contains(&inner));
        assert!(!inner.contains(&outer));
    }

    #[test]
    pub fn overlaps() {
        let left = Span::new(SourceFileID::new(0), BytePos::new(5), BytePos::new(9));
        let right = Span::new(SourceFileID::new(0), BytePos::new(7), BytePos::new(12));

        assert!(left.overlaps(&right));
        assert!(right.overlaps(&left));
    }
}
