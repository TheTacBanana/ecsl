use cfgrammar::Span;
pub use ecsl_ast::expr::Literal;
use ecsl_ast::parse::Immediate;
use ecsl_index::TyID;

use crate::expr::Query;

#[derive(Debug, Clone)]
pub enum Constant {
    Internal {
        imm: Immediate,
    },
    External {
        span: Span,
        kind: Literal,
        tyid: TyID,
    },
    Query {
        span: Span,
        query: Query,
    },
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Internal { imm } => write!(f, "{:?}", imm),
            Constant::External { kind, .. } => write!(f, "{:?}", kind),
            Constant::Query { query, .. } => write!(f, "{}", query),
        }
    }
}
