use crate::{local::LocalTyCtxt, TyIr};
use bimap::BiHashMap;
use ecsl_index::{GlobalID, SourceFileID, TyID};
use std::{
    collections::{btree_map::Entry, BTreeMap},
    sync::{Arc, RwLock},
};

pub struct TyCtxt {
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,
    pub mappings: RwLock<BTreeMap<GlobalID, TyID>>,
    pub tyirs: RwLock<BiHashMap<TyID, TyIr>>,
    pub cur_id: RwLock<usize>,
}

impl TyCtxt {
    pub fn new() -> Self {
        let ty_ctxt = TyCtxt {
            sources: Default::default(),
            mappings: Default::default(),
            tyirs: Default::default(),
            cur_id: Default::default(),
        };
        ty_ctxt.insert_tyir(TyID::UNKNOWN, TyIr::Unknown);
        ty_ctxt.insert_tyir(TyID::BOTTOM, TyIr::Bottom);
        ty_ctxt
    }

    fn next_id(&self) -> TyID {
        let mut cur_id = self.cur_id.write().unwrap();
        let val = cur_id.clone();
        *cur_id += 1;
        return TyID::new(val);
    }

    pub fn get_or_create_tyid(&self, id: GlobalID) -> TyID {
        let mut lock = self.mappings.write().unwrap();
        let tyid = match lock.entry(id) {
            Entry::Vacant(vacant) => *vacant.insert(self.next_id()),
            Entry::Occupied(occupied) => *occupied.get(),
        };
        tyid
    }

    pub fn insert_tyir(&self, id: TyID, tyir: TyIr) {
        let mut defs = self.tyirs.write().unwrap();
        defs.insert(id, tyir);
    }

    pub fn tyid_from_tyir(&self, tyir: TyIr) -> TyID {
        let mut tyirs = self.tyirs.write().unwrap();
        if !tyirs.contains_right(&tyir) {
            let next_id = self.next_id();
            tyirs.insert(next_id, tyir);
            next_id
        } else {
            tyirs.get_by_right(&tyir).cloned().unwrap()
        }
    }

    pub fn get_tyir(&self, id: TyID) -> TyIr {
        let defs = self.tyirs.read().unwrap();
        defs.get_by_left(&id).unwrap().clone()
    }

    pub fn unknown_ty(&self) -> TyID {
        TyID::ZERO
    }

    pub fn is_primitive(&self, id: TyID) -> bool {
        match self.get_tyir(id) {
            TyIr::Bool | TyIr::Char | TyIr::Int | TyIr::Float => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self, id: TyID) -> bool {
        match self.get_tyir(id) {
            TyIr::Int | TyIr::Float => true,
            _ => false,
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
