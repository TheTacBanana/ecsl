#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct LineNumber(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct BytePos(pub u32);

impl BytePos {
    pub const ZERO : BytePos = BytePos(0);
}
