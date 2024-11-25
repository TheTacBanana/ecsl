// pub struct SourceImportExport {

// }

use std::path::PathBuf;

use ecsl_index::{SourceFileID, SymbolID};

#[derive(Debug, Clone)]
pub struct ImportPath {
    pub relative_path: PathBuf,
    pub symbol: SymbolID,
    pub import_from: SourceFileID,
}

pub enum SymbolDefinition {
    Function(Option<Symbol>),
    Struct(),
}
