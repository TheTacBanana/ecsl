use ecsl_index::SourceFileID;
use local::LocalTyCtxt;
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

pub mod assoc;
pub mod def;
pub mod import;
pub mod local;
pub mod ty;

pub struct TyCtxt {
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,
}

impl TyCtxt {
    pub fn new() -> Self {
        TyCtxt {
            sources: Default::default(),
        }
    }
}

pub enum ImportError<'a> {
    MultipleImports(&'a str),
    MultipleDefinitions(&'a str),
    SelfImport,
}

impl<'a> std::fmt::Display for ImportError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ImportError::MultipleImports(s) => &format!("Symbol '{}' imported multiple times", s),
            ImportError::MultipleDefinitions(s) => {
                &format!("Symbol '{}' defined multiple times", s)
            }
            ImportError::SelfImport => &format!("Cannot import items from same file"),
        };
        write!(f, "{s}")
    }
}
