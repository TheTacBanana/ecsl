use cfgrammar::Span;
use ecsl_index::{GlobalID, SourceFileID, SymbolID};
use std::path::PathBuf;

#[derive(Debug)]
pub enum Import {
    /// Symbol has not been linked yet
    Unresolved(ImportPath),
    /// Symbol has been successfully linked
    Resolved(MappedImport),
    /// Symbol failed to be linked
    Unknown,
}

#[derive(Debug, Clone)]
pub struct ImportPath {
    pub path: PathBuf,
    pub file: SourceFileID,
    pub from: SymbolID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct MappedImport {
    pub from: GlobalID,
    pub to: GlobalID,
}
