#[derive(Debug, Clone, Copy)]
pub struct Token {
    len: u32,
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, length: u32) -> Self {
        Self { len: length, kind }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// "// This is a comment"
    Comment,

    /// Any sequence of whitespace characters
    Whitespace,

    /// Any valid identifer
    Identifier,

    /// Literals
    Literal(LiteralKind),

    // Single Character Tokens
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "#"
    Hash,
    /// ":"
    Colon,
    /// "="
    Eq,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "&"
    Ampersand,
    /// "|"
    Pipe,

    /// End of File
    Eof,

    /// Unknown character, most likely unicode
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
    Int { base: Base },
    Float,
    String { terminated: bool },
    Char { terminated: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}
