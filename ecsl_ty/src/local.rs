use crate::{
    ctxt::{ImportError, TyCtxt},
    def::Definition,
    import::{Import, ImportPath},
    FieldDef, GenericsScope, TyIr, TypeError,
};
use bimap::BiBTreeMap;
use cfgrammar::Span;
use ecsl_ast::ty::{Ty, TyKind};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{FieldID, GlobalID, SourceFileID, SymbolID, TyID};
use ecsl_parse::table::SymbolTable;
use log::error;
use std::{
    collections::{btree_map::Entry, BTreeMap},
    sync::{Arc, RwLock},
};

pub struct LocalTyCtxt {
    pub file: SourceFileID,
    pub table: Arc<SymbolTable>,
    pub global: Arc<TyCtxt>,
    pub diag: DiagConn,

    /// Import Mappings
    pub imported: RwLock<BTreeMap<SymbolID, Import>>,
    pub imported_resolved: RwLock<BiBTreeMap<(Option<SymbolID>, SymbolID), GlobalID>>,

    pub defined: RwLock<BTreeMap<SymbolID, Definition>>,
    pub assoc: RwLock<BTreeMap<SymbolID, BTreeMap<SymbolID, Definition>>>,
}

pub trait LocalTyCtxtExt {
    fn new_local_ctxt(
        &self,
        file: SourceFileID,
        table: Arc<SymbolTable>,
        diag: DiagConn,
    ) -> Arc<LocalTyCtxt>;
}

impl LocalTyCtxtExt for Arc<TyCtxt> {
    fn new_local_ctxt(
        &self,
        file: SourceFileID,
        table: Arc<SymbolTable>,
        diag: DiagConn,
    ) -> Arc<LocalTyCtxt> {
        let lctx = Arc::new(LocalTyCtxt {
            file,
            global: self.clone(),
            table,
            diag,
            imported: Default::default(),
            defined: Default::default(),
            assoc: Default::default(),
            imported_resolved: Default::default(),
        });
        self.sources.write().unwrap().insert(file, lctx.clone());
        lctx
    }
}

impl LocalTyCtxt {
    pub fn define_symbol(&self, def: Definition) {
        match def {
            Definition::Struct(_) | Definition::Enum(_) | Definition::Function(_) => {
                let key = def.ident();
                if self.defined.read().unwrap().contains_key(&key) {
                    let symbol = self.table.get_symbol(key).unwrap();
                    self.diag.push_error(
                        EcslError::new(
                            ErrorLevel::Error,
                            ImportError::MultipleDefinitions(&symbol.name),
                        )
                        .with_span(|_| def.span()),
                    );

                    return;
                }
                self.defined.write().unwrap().insert(key, def);
            }
            Definition::AssocFunction(gen, ty, fn_def) => {
                let scope = ty.into_scope().unwrap();

                let mut assoc = self.assoc.write().unwrap();

                match assoc.entry(scope) {
                    Entry::Vacant(vacant) => {
                        let mut new = BTreeMap::new();
                        new.insert(fn_def.ident, Definition::AssocFunction(gen, ty, fn_def));
                        vacant.insert(new);
                    }
                    Entry::Occupied(mut occupied) => {
                        if occupied.get().contains_key(&fn_def.ident) {
                            let symbol = self.table.get_symbol(fn_def.ident).unwrap();
                            self.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    ImportError::MultipleDefinitions(&symbol.name),
                                )
                                .with_span(|_| fn_def.span),
                            );
                            return;
                        }

                        occupied
                            .get_mut()
                            .insert(fn_def.ident, Definition::AssocFunction(gen, ty, fn_def));
                    }
                }
            }
        }
    }

    pub fn import_symbol(&self, import: ImportPath) {
        let symbol = import.from;
        if import.path.as_os_str().is_empty() {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Error, ImportError::SelfImport)
                    .with_span(|_| import.span),
            );
            return;
        }

        if self.defined.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    ImportError::MultipleDefinitions(&symbol.name),
                )
                .with_span(|_| import.span),
            );
            return;
        }

        if self.imported.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    ImportError::MultipleImports(&symbol.name),
                )
                .with_span(|_| import.span),
            );
            return;
        }

        {
            let mut lock = self.imported.write().unwrap();
            lock.insert(symbol, Import::Unresolved(import));
        }
    }

    pub fn exists(&self, id: SymbolID) -> bool {
        {
            let lock = self.defined.read().unwrap();
            if lock.contains_key(&id) {
                return true;
            }
        }
        {
            let lock = self.imported.read().unwrap();
            if lock.contains_key(&id) {
                return true;
            }
        }
        return false;
    }

    pub fn get_tyid(&self, ty: &Ty, scope: &GenericsScope) -> Option<TyID> {
        macro_rules! resolve_tyid {
            ($sym:ident) => {
                if let Some(index) = scope.scope_index(*$sym) {
                    self.global.tyid_from_tyir(TyIr::GenericParam(index))
                } else if let Some(gid) = self.get_global_id(None, *$sym) {
                    self.global.get_or_create_tyid(gid)
                } else {
                    self.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TypeError::UnknownType)
                            .with_span(|_| ty.span),
                    );
                    TyID::UNKNOWN
                }
            };
        }

        macro_rules! from_tyir {
            ($tyir:expr) => {
                Some(self.global.tyid_from_tyir($tyir))
            };
        }

        match &ty.kind {
            TyKind::Ident(symbol_id) => {
                let adt_base_id = resolve_tyid!(symbol_id);

                let total_generics = self
                    .global
                    .get_tyir(adt_base_id)
                    .into_adt()
                    .map(|adt| adt.total_generics)
                    .unwrap_or_default();

                let s;
                let error = match (total_generics, ty.generics.params.len()) {
                    (l, r) if l == r => None,
                    (0, n) if n > 0 => Some("Generics not needed"),
                    (n, 0) if n > 0 => Some("Type requires generics"),
                    (l @ 0.., r @ 0..) => {
                        s = format!("Mismatched generics required {} vs actual {}", l, r);
                        Some(s.as_str())
                    }
                };
                if let Some(error) = error {
                    self.diag.push_error(
                        EcslError::new(ErrorLevel::Error, error).with_span(|_| ty.generics.span),
                    );
                    return Some(TyID::UNKNOWN);
                };

                let mut params = Vec::new();
                let mut known_tys = 0;
                for g in &ty.generics.params {
                    let param_tyid = self.get_tyid(g, scope)?;
                    params.push(param_tyid);

                    let tyir = self.global.get_tyir(param_tyid);
                    match tyir {
                        TyIr::Ref(_, field_def) => match self.global.get_tyir(field_def.ty) {
                            TyIr::GenericParam(_) => (),
                            _ => known_tys += 1,
                        },
                        TyIr::GenericParam(_) => (),
                        _ => known_tys += 1,
                    }
                }

                if total_generics == 0 {
                    Some(adt_base_id)
                } else if known_tys == total_generics {
                    self.get_mono_variant(adt_base_id, &params, ty.span)
                } else {
                    self.global
                        .monos
                        .insert_mapping((adt_base_id, params), adt_base_id);
                    Some(adt_base_id)
                }
            }
            TyKind::Ref(mutable, ty) => from_tyir!(TyIr::Ref(
                *mutable,
                FieldDef {
                    id: FieldID::ZERO,
                    ty: self.get_tyid(ty, scope)?,
                    params: Vec::new()
                }
            )),
            TyKind::Ptr(mutable, ty) => from_tyir!(TyIr::Ref(
                *mutable,
                FieldDef {
                    id: FieldID::ZERO,
                    ty: self.get_tyid(ty, scope)?,
                    params: Vec::new()
                }
            )),
            TyKind::Entity(_, _) => from_tyir!(TyIr::Entity),
            TyKind::Schedule => from_tyir!(TyIr::Schedule),
            TyKind::Array(ty, span) => from_tyir!(TyIr::Array(self.get_tyid(ty, scope)?, *span)),
            TyKind::Query(_) => from_tyir!(TyIr::Query),
            TyKind::ArrayRef(_, _) => todo!(),
        }
    }

    pub fn get_mono_variant(&self, id: TyID, params: &Vec<TyID>, span: Span) -> Option<TyID> {
        fn map_tyid(s: &LocalTyCtxt, field: &mut FieldDef, params: &Vec<TyID>, span: Span) {
            let mut tyir = s.global.get_tyir(field.ty);
            field.ty = match &mut tyir {
                TyIr::GenericParam(i) => params.get(*i).copied().unwrap(),
                TyIr::ADT(adtdef) => {
                    let mut field_params = Vec::new();
                    for param_tyid in &mut field.params {
                        let mut temp = FieldDef {
                            id: FieldID::ZERO,
                            ty: *param_tyid,
                            params: Vec::new(),
                        };

                        map_tyid(s, &mut temp, params, span);

                        field_params.push(temp.ty);
                    }

                    if adtdef.total_generics != field_params.len() {
                        s.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                .with_span(|_| span),
                        );
                        TyID::UNKNOWN
                    } else {
                        s.get_mono_variant(field.ty, &field_params, span).unwrap()
                    }
                }
                TyIr::Ref(_, field_def) => {
                    map_tyid(s, field_def, params, span);
                    s.global.tyid_from_tyir(tyir.clone())
                }
                _ => field.ty,
            };
        }

        let key = (id, params.clone());
        {
            if let Some(mono) = self.global.monos.mono_map.read().unwrap().get_by_left(&key) {
                return Some(*mono);
            }
        }
        {
            let mut tyir = self.global.get_tyir(id).clone();
            let generic_count = tyir.get_generics();

            match &mut tyir {
                TyIr::ADT(adt_tyir) => {
                    // debug!("{:?}", adt_tyir);
                    if params.len() != generic_count {
                        self.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                .with_span(|_| span),
                        );
                        return Some(TyID::UNKNOWN);
                    }
                    adt_tyir.map(|f| map_tyid(self, f, params, span));
                    adt_tyir.resolved_generics = adt_tyir.total_generics;

                    let new_tyid = self.global.tyid_from_tyir(tyir);
                    self.global.monos.insert_mapping(key, new_tyid);

                    let (span, fid) = self.global.get_span(id).unwrap();
                    self.global.insert_span_file(new_tyid, span, fid);

                    Some(new_tyid)
                }
                TyIr::Fn(fn_tyir) => {
                    if params.len() != generic_count {
                        self.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                .with_span(|_| span),
                        );
                        return Some(TyID::UNKNOWN);
                    }

                    fn_tyir.map(|f| map_tyid(self, f, params, span));
                    fn_tyir.resolved_generics = fn_tyir.total_generics;

                    let new_tyid = self.global.tyid_from_tyir(tyir);
                    self.global.monos.insert_mapping(key, new_tyid);

                    Some(new_tyid)
                }
                TyIr::Ref(_, field) => {
                    map_tyid(self, field, params, span);

                    let new_tyid = self.global.tyid_from_tyir(tyir);
                    self.global.monos.insert_mapping(key, new_tyid);

                    Some(new_tyid)
                }
                TyIr::GenericParam(_)
                | TyIr::Bool
                | TyIr::Char
                | TyIr::Int
                | TyIr::Float
                | TyIr::Str
                | TyIr::Entity
                | TyIr::Bottom
                | TyIr::Query => Some(id),
                e => {
                    error!("{:?}", e);
                    None
                }
            }
        }
    }
    pub fn get_global_id(&self, scope: Option<SymbolID>, id: SymbolID) -> Option<GlobalID> {
        if let Some(scope) = scope {
            let assoc = self.assoc.read().unwrap();

            if let Some(assoc) = assoc.get(&scope) {
                if assoc.contains_key(&id) {
                    return Some(GlobalID::new(Some(scope), id, self.file));
                }
            }

            let imported = self.imported_resolved.read().unwrap();
            imported.get_by_left(&(Some(scope), id)).copied()
        } else {
            let defined = self.defined.read().unwrap();
            if defined.contains_key(&id) {
                return Some(GlobalID::new(scope, id, self.file));
            }

            let imported = self.imported_resolved.read().unwrap();
            imported.get_by_left(&(None, id)).copied()
        }
    }
}
