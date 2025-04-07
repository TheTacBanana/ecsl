use crate::{local::LocalTyCtxt, mono::Mono, TyIr};
use bimap::{BiBTreeMap, BiHashMap};
use cfgrammar::Span;
use ecsl_diagnostics::DiagConn;
use ecsl_index::{FieldID, GlobalID, SourceFileID, TyID, VariantID};
use ecsl_parse::LexerTy;
use log::{debug, error};
use lrpar::NonStreamingLexer;
use std::{
    collections::{btree_map::Entry, BTreeMap},
    sync::{Arc, RwLock, RwLockWriteGuard},
};

pub struct TyCtxt {
    pub diag: DiagConn,
    pub monos: Arc<Mono>,

    /// Links to LocalTyCtxt's from source file ID
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,
    /// Map Global ID's to TyID
    pub mappings: RwLock<BiBTreeMap<GlobalID, TyID>>,
    /// BiMap between TyID and TyIr
    pub tyirs: RwLock<BiHashMap<TyID, TyIr>>,

    /// Next TyID
    pub cur_id: RwLock<usize>,
    /// Size Map for each TyID
    pub sizes: RwLock<BTreeMap<TyID, usize>>,
    /// Field offsets based on TyID, VariantID and FieldID
    pub field_offsets: RwLock<BTreeMap<(TyID, VariantID, FieldID), usize>>,

    /// Unused map of spans
    pub spans: RwLock<BTreeMap<TyID, (Span, SourceFileID)>>,

    /// Singular entry point for program
    pub entry_point: RwLock<Option<TyID>>,
}

impl TyCtxt {
    pub fn new(diag: DiagConn) -> Self {
        let ty_ctxt = TyCtxt {
            diag,
            sources: Default::default(),
            mappings: Default::default(),
            tyirs: Default::default(),
            monos: Default::default(),
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

    pub unsafe fn next_id(&self) -> TyID {
        let mut cur_id = self.cur_id.write().unwrap();
        let val = cur_id.clone();
        *cur_id += 1;
        return TyID::new(val);
    }

    pub unsafe fn insert_tyir(&self, id: TyID, tyir: TyIr, span: Span, fid: SourceFileID) {
        let mut defs = self.tyirs.write().unwrap();
        let mut spans = self.spans.write().unwrap();
        defs.insert(id, tyir);
        spans.insert(id, (span, fid));
    }

    pub fn get_or_create_tyid(&self, id: GlobalID) -> TyID {
        let mut lock = self.mappings.write().unwrap();
        if !lock.contains_left(&id) {
            let tyid = unsafe { self.next_id() };
            lock.insert(id, tyid);
            tyid
        } else {
            *lock.get_by_left(&id).unwrap()
        }
    }

    pub fn global_from_tyid(&self, tyid: TyID) -> Option<GlobalID> {
        let lock = self.mappings.read().unwrap();
        lock.get_by_right(&tyid).copied()
    }

    pub fn tyid_from_tyir(&self, tyir: TyIr) -> TyID {
        let mut tyirs = self.tyirs.write().unwrap();
        if !tyirs.contains_right(&tyir) {
            let next_id = unsafe { self.next_id() };
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

    pub fn get_span(&self, id: TyID) -> Option<(Span, SourceFileID)> {
        let spans = self.spans.read().unwrap();
        spans.get(&id).cloned()
    }

    pub fn get_size(&self, id: TyID) -> Option<usize> {
        let mut sizes = self.sizes.write().unwrap();
        self.internal_get_size(id, &mut sizes)
    }

    fn internal_get_size(
        &self,
        id: TyID,
        sizes: &mut RwLockWriteGuard<BTreeMap<TyID, usize>>,
    ) -> Option<usize> {
        if let Some(size) = sizes.get(&id) {
            return Some(*size);
        }

        let tyir = self.get_tyir(id);

        let size = match tyir {
            TyIr::Ref(_, _) => 8,
            TyIr::Range(tyid, _) => self.internal_get_size(tyid, sizes)?,
            TyIr::ADT(def) => {
                if def.is_struct() {
                    let mut total_size = 0;
                    for (_, field) in &def.get_struct_fields().field_tys {
                        total_size += self.internal_get_size(field.ty, sizes)?;
                    }
                    total_size
                } else {
                    let mut max_size = 0;
                    for (_, var) in &def.variant_kinds {
                        let mut total_size = 0;
                        for (_, field) in &var.field_tys {
                            total_size += self.internal_get_size(field.ty, sizes)?;
                        }
                        max_size = usize::max(max_size, total_size);
                    }
                    def.discriminant_size().unwrap() + max_size
                }
            }
            e => {
                error!("{id:?} {e:?}");
                return None;
            } // TyIr::String => todo!(),
              // TyIr::Fn(fn_def) => todo!(),
              // TyIr::Array(ty_id, _) => todo!(),
              // TyIr::ArrayRef(mutable, ty_id) => todo!(),
              // TyIr::GenericParam(_) => todo!(),
        };
        sizes.insert(id, size);
        Some(size)
    }

    pub fn get_field_offset(&self, id: TyID, vid: VariantID, fid: FieldID) -> Option<usize> {
        let mut offsets = self.field_offsets.write().unwrap();
        if let Some(offset) = offsets.get(&(id, vid, fid)) {
            return Some(*offset);
        }
        let mut sizes = self.sizes.write().unwrap();

        let tyir = self.get_tyir(id);
        let offset = match tyir {
            TyIr::ADT(def) => {
                let fields = &def.variant_kinds.get(&vid).unwrap().field_tys;
                let mut offset = 0;
                if let Some(disc_size) = def.discriminant_size() {
                    offset += disc_size;
                }
                for (_, field) in fields {
                    offsets.insert((id, vid, field.id), offset);
                    offset += self.internal_get_size(field.ty, &mut sizes)?;
                }
                *offsets.get(&(id, vid, fid)).unwrap()
            }
            e => panic!("{id:?} {e:?}"),
        };

        return Some(offset);
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

    pub fn is_reference(&self, id: TyID) -> Option<TyID> {
        match self.get_tyir(id) {
            TyIr::Ref(_, tyid) => Some(tyid),
            _ => None,
        }
    }

    pub fn format_str(&self, s: &str, lexers: &BTreeMap<SourceFileID, LexerTy>) -> Option<String> {
        let mut out = String::new();
        let mut start = 0;
        while let Some(offset) = s[start..].find("{ty:") {
            out.push_str(&s[start..(start + offset)]);
            start += offset;

            let end_offset = s[start..].find("}").unwrap();
            let tyid = TyID::new(s[(start + 4)..(start + end_offset)].parse().unwrap());
            start += end_offset + 1;

            if s[start..].find("!") == Some(0) {
                // start += 1;
                if let Some((span, fid)) = self.get_span(tyid) {
                    let ty_str = lexers.get(&fid).unwrap().span_str(span);
                    out.push_str(ty_str);
                } else {
                    out.push_str("?");
                }
                return Some(out);
            }

            let tyir = self.get_tyir(tyid);
            let fmt_string = tyir.into_fmt_string();

            if let Some(s) = self.format_str(&fmt_string, lexers) {
                out.push_str(&s);
            } else {
                out.push_str(&fmt_string);
            }
        }

        if out.is_empty() {
            return None;
        } else {
            out.push_str(&s[start..]);
            return Some(out);
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
