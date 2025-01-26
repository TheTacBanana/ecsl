use crate::{local::LocalTyCtxt, TyIr};
use bimap::BiHashMap;
use ecsl_index::{GlobalID, SourceFileID, TyID};
use log::debug;
use std::{
    collections::{btree_map::Entry, BTreeMap},
    sync::{Arc, RwLock},
};

pub struct TyCtxt {
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,

    pub mappings: RwLock<BTreeMap<GlobalID, TyID>>,
    pub tyirs: RwLock<BiHashMap<TyID, TyIr>>,
}

impl TyCtxt {
    pub fn new() -> Self {
        TyCtxt {
            sources: Default::default(),
            mappings: Default::default(),
            tyirs: Default::default(),
        }
    }

    pub fn get_or_create_tyid(&self, id: GlobalID) -> TyID {
        let mut lock = self.mappings.write().unwrap();
        let next_id = TyID::new(lock.len());
        match lock.entry(id) {
            Entry::Vacant(vacant) => *vacant.insert(next_id),
            Entry::Occupied(occupied) => *occupied.get(),
        }
    }

    pub fn insert_tyir(&self, id: TyID, tyir: TyIr) {
        debug!("{:?}", tyir);
        let mut defs = self.tyirs.write().unwrap();
        defs.insert(id, tyir);
    }

    pub fn tyid_from_tyir(&self, tyir: TyIr) -> TyID {
        debug!("{:?}", tyir);
        let mut tyirs = self.tyirs.write().unwrap();
        let tyid = TyID::new(tyirs.len());
        if !tyirs.contains_right(&tyir) {
            tyirs.insert(tyid, tyir);
        }
        tyid
    }

    pub fn get_tyir(&self, id: TyID) -> Option<TyIr> {
        let defs = self.tyirs.read().unwrap();
        defs.get_by_left(&id).cloned()
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
