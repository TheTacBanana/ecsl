use cfgrammar::Span;
pub use ecsl_ast::expr::Literal;
use ecsl_ast::parse::Immediate;
use ecsl_index::{ConstID, TyID};

use crate::expr::{Query, ScheduleKind};

#[derive(Debug, Clone)]
pub enum Constant {
    Internal {
        span: Span,
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
    Schedule {
        kind: ScheduleKind,
        contents: Vec<ConstID>,
    },
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Internal { imm, .. } => write!(f, "{:?}", imm),
            Constant::External { kind, .. } => write!(f, "{:?}", kind),
            Constant::Query { query, .. } => write!(f, "{}", query),
            Constant::Schedule { kind, contents } => {
                write!(f, "{}", kind.opening())?;
                for c in contents {
                    write!(f, "{}, ", c)?
                }
                write!(f, "{}", kind.closing())
            }
        }
    }
}
