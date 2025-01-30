use cfgrammar::Span;
use ecsl_index::BlockID;

pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

pub enum TerminatorKind {
    Jump(BlockID),
    Return,
}
