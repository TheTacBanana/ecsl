use std::{
    ops::{Add, Deref, Sub},
    path::PathBuf,
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CrateID(pub u32);

impl CrateID {
    pub fn new(id: u32) -> Self {
        CrateID(id)
    }
}

impl Deref for CrateID {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SourceFileID(u32);

impl SourceFileID {
    pub fn new(id: u32) -> Self {
        SourceFileID(id)
    }
}

impl Deref for SourceFileID {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct LineNumber(u32);

impl LineNumber {
    pub fn new(id: u32) -> Self {
        LineNumber(id)
    }
}

impl Deref for LineNumber {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for LineNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineData {
    number: LineNumber,
    length: usize,
}

impl LineData {
    pub fn new(number: LineNumber, length: usize) -> Self {
        Self { number, length }
    }

    pub fn number(&self) -> LineNumber {
        self.number
    }

    pub fn length(&self) -> usize {
        self.length
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BytePos(u32);

impl BytePos {
    pub const ZERO: BytePos = BytePos(0);
    pub const ONE: BytePos = BytePos(1);

    pub fn new(id: u32) -> Self {
        BytePos(id)
    }
}

impl Deref for BytePos {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add<BytePos> for BytePos {
    type Output = BytePos;

    fn add(self, rhs: BytePos) -> Self::Output {
        BytePos::new(self.0 + rhs.0)
    }
}

impl Sub<BytePos> for BytePos {
    type Output = BytePos;

    fn sub(self, rhs: BytePos) -> Self::Output {
        BytePos::new(self.0 - rhs.0)
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SnippetLocation {
    path: PathBuf,
    line: LineNumber,
    column: usize,
}

impl SnippetLocation {
    pub fn new(path: PathBuf, line: LineNumber, column: usize) -> Self {
        SnippetLocation { path, line, column }
    }
}

impl std::fmt::Display for SnippetLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.path.to_str().unwrap(),
            *self.line + 1,
            self.column
        )
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
