#[derive(Debug, Clone, Copy)]
pub struct Token {
    length: u32,
    kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Whitespace,
}
