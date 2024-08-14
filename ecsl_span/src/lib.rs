use std::ops::Deref;

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

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BytePos(u32);

impl BytePos {
    pub const ZERO: BytePos = BytePos(0);

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

/// Range is inclusive [start..end]
#[derive(Debug)]
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

    pub fn contains(&self, other: &Self) -> bool {
        self.file == other.file && self.start <= other.start && self.end >= other.end
    }

    pub fn overlaps(&self, other: &Self) -> bool {
        self.file == other.file && (self.start <= other.end || self.end >= other.start)
    }
}

#[cfg(test)]
pub mod test {
    use crate::{BytePos, SourceFileID, Span};

    #[test]
    pub fn contains() {
        let inner = Span::new(SourceFileID::new(0), BytePos::new(5), BytePos::new(9));
        let outer  = Span::new(SourceFileID::new(0), BytePos::new(2), BytePos::new(12));

        assert!(outer.contains(&inner));
        assert!(!inner.contains(&outer));
    }

    #[test]
    pub fn overlaps() {
        let left = Span::new(SourceFileID::new(0), BytePos::new(5), BytePos::new(9));
        let right  = Span::new(SourceFileID::new(0), BytePos::new(7), BytePos::new(12));

        assert!(left.overlaps(&right));
        assert!(right.overlaps(&left));
    }
}