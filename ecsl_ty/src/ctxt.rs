use crate::{local::LocalTyCtxt, TyIr};
use bimap::BiHashMap;
use cfgrammar::Span;
use ecsl_index::{FieldID, GlobalID, SourceFileID, TyID};
use log::debug;
use std::{
    collections::{btree_map::Entry, BTreeMap},
    sync::{Arc, RwLock, RwLockWriteGuard},
};

pub struct TyCtxt {
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,
    pub mappings: RwLock<BTreeMap<GlobalID, TyID>>,
    pub tyirs: RwLock<BiHashMap<TyID, TyIr>>,
    pub cur_id: RwLock<usize>,
    pub sizes: RwLock<BTreeMap<TyID, usize>>,
    pub field_offsets: RwLock<BTreeMap<(TyID, FieldID), usize>>,
    pub spans: RwLock<BTreeMap<TyID, Span>>,
    pub entry_point: RwLock<Option<TyID>>,
}

impl TyCtxt {
    pub fn new() -> Self {
        let ty_ctxt = TyCtxt {
            sources: Default::default(),
            mappings: Default::default(),
            tyirs: Default::default(),
            cur_id: Default::default(),
            sizes: Default::default(),
            entry_point: Default::default(),
            spans: Default::default(),
            field_offsets: Default::default(),
        };
        ty_ctxt.tyid_from_tyir(TyIr::Unknown);
        ty_ctxt.tyid_from_tyir(TyIr::Bottom);
        ty_ctxt.insert_size(TyID::BOTTOM, 0);
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

    pub unsafe fn insert_tyir(&self, id: TyID, tyir: TyIr, span: Span) {
        let mut defs = self.tyirs.write().unwrap();
        let mut spans = self.spans.write().unwrap();
        defs.insert(id, tyir);
        spans.insert(id, span);
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

    pub fn insert_size(&self, id: TyID, size: usize) {
        let mut sizes = self.sizes.write().unwrap();
        debug!("{:?} {:?}", id, size);
        sizes.insert(id, size);
    }

    pub fn insert_entry_point(&self, id: TyID) {
        let mut entry = self.entry_point.write().unwrap();
        *entry = Some(id)
    }

    pub fn entry_point(&self) -> TyID {
        let entry = self.entry_point.read().unwrap();
        entry.unwrap()
    }

    pub fn get_span(&self, id: TyID) -> Option<Span> {
        let spans = self.spans.read().unwrap();
        spans.get(&id).cloned()
    }

    pub fn get_size(&self, id: TyID) -> usize {
        let mut sizes = self.sizes.write().unwrap();
        self.internal_get_size(id, &mut sizes)
    }

    fn internal_get_size(
        &self,
        id: TyID,
        sizes: &mut RwLockWriteGuard<BTreeMap<TyID, usize>>,
    ) -> usize {
        if let Some(size) = sizes.get(&id) {
            return *size;
        }

        let tyir = self.get_tyir(id);

        let size = match tyir {
            TyIr::Ref(_, _) => 8,
            TyIr::Range(tyid, _) => self.internal_get_size(tyid, sizes),
            TyIr::Struct(def) => {
                let mut total_size = 0;
                for (_, field) in def.fields {
                    total_size += self.internal_get_size(field.ty, sizes);
                }
                total_size
            }
            TyIr::Enum(def) => {
                let mut max_size = 0;
                for (_, var) in def.variant_kinds {
                    let mut total_size = 0;
                    for (_, field) in var.field_tys {
                        total_size += self.internal_get_size(field.ty, sizes);
                    }
                    max_size = usize::max(max_size, total_size);
                }
                1 + max_size // TODO:
            }
            e => panic!("{id:?} {e:?}"),
            // TyIr::String => todo!(),
            // TyIr::Fn(fn_def) => todo!(),
            // TyIr::Array(ty_id, _) => todo!(),
            // TyIr::ArrayRef(mutable, ty_id) => todo!(),
            // TyIr::GenericParam(_) => todo!(),
        };
        sizes.insert(id, size);
        size
    }

    pub fn get_field_offset(&self, id: TyID, fid: FieldID) -> usize {
        let mut offsets = self.field_offsets.write().unwrap();
        if let Some(offset) = offsets.get(&(id, fid)) {
            return *offset;
        }
        let mut sizes = self.sizes.write().unwrap();

        let tyir = self.get_tyir(id);
        let offset = match tyir {
            TyIr::Struct(def) => {
                let mut offset = 0;
                for (_, field) in def.fields {
                    offsets.insert((id, field.id), offset);
                    debug!("{:?} {:?}", field.id, offset);
                    offset += self.internal_get_size(field.ty, &mut sizes);
                }
                *offsets.get(&(id, fid)).unwrap()
            }
            e => panic!("{id:?} {e:?}"),
            // TyIr::Ref(_, _) => 8,
            // TyIr::Range(tyid, _) => self.internal_get_size(tyid, sizes),
            // TyIr::String => todo!(),
            // TyIr::Enum(enum_def) => todo!(),
            // TyIr::Fn(fn_def) => todo!(),
            // TyIr::Array(ty_id, _) => todo!(),
            // TyIr::ArrayRef(mutable, ty_id) => todo!(),
            // TyIr::GenericParam(_) => todo!(),
        };

        return offset;
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
