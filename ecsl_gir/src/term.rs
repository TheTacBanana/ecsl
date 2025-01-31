use cfgrammar::Span;
use ecsl_index::BlockID;

#[derive(Debug)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

#[derive(Debug)]
pub enum TerminatorKind {
    Jump(BlockID),
    Return,
}
