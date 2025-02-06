use cfgrammar::Span;
pub use ecsl_ast::expr::Literal;
use ecsl_index::TyID;

#[derive(Debug)]
pub struct Constant {
    pub span: Span,
    pub kind: Literal,
    pub tyid: TyID,
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tyid)
    }
}
