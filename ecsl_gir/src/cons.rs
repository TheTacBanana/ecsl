use cfgrammar::Span;
pub use ecsl_ast::expr::Literal;

#[derive(Debug)]
pub struct Constant {
    pub span: Span,
    pub kind: Literal,
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Constant {
    pub fn from_literal(kind: Literal, span: Span) -> Self {
        Self { span, kind }
    }
}
