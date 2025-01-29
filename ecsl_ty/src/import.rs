use cfgrammar::Span;
use ecsl_index::GlobalID;
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
    pub from: GlobalID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct MappedImport {
    pub from: GlobalID,
    pub to: GlobalID,
}
