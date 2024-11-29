use std::path::PathBuf;

use cfgrammar::Span;
use ecsl_index::{SourceFileID, SymbolID};

#[derive(Debug, Clone)]
pub struct ImportPath {
    pub path: PathBuf,
    pub symbol: SymbolID,
    pub source: SourceFileID,
    pub span: Span,
}
