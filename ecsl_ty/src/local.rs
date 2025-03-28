use crate::{
    ctxt::{ImportError, TyCtxt},
    def::Definition,
    import::{Import, ImportPath},
    FieldDef, GenericsScope, TyIr, TypeError,
};
use ecsl_ast::ty::{Ty, TyKind};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{GlobalID, SourceFileID, SymbolID, TyID};
use ecsl_parse::table::SymbolTable;
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

pub struct LocalTyCtxt {
    pub file: SourceFileID,
    pub table: Arc<SymbolTable>,
    pub global: Arc<TyCtxt>,
    pub diag: DiagConn,

    /// Import Mappings
    pub imported: RwLock<BTreeMap<SymbolID, Import>>,

    pub defined: RwLock<BTreeMap<SymbolID, Definition>>,
    // pub impl_blocks: RwLock<BTreeMap<SymbolID, ImplBlock>>,
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
        });
        self.sources.write().unwrap().insert(file, lctx.clone());
        lctx
    }
}

impl LocalTyCtxt {
    pub fn define_symbol(&self, def: Definition) {
        let symbol = def.ident();
        if self.defined.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    ImportError::MultipleDefinitions(&symbol.name),
                )
                .with_span(|_| def.span()),
            );

            return;
        }
        self.defined.write().unwrap().insert(def.ident(), def);
    }

    pub fn import_symbol(&self, import: ImportPath) {
        let symbol = import.from.symbol();
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

    pub fn get_tyid(&self, ty: &Ty, scope: &GenericsScope) -> TyID {
        macro_rules! resolve_tyid {
            ($sym:ident) => {
                if let Some(index) = scope.scope_index(*$sym) {
                    self.global.tyid_from_tyir(TyIr::GenericParam(index))
                } else if let Some(gid) = self.get_global_id(*$sym) {
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
                self.global.tyid_from_tyir($tyir)
            };
        }

        match &ty.kind {
            TyKind::Ident(symbol_id) => {
                let adt_base_id = resolve_tyid!(symbol_id);
                if let Some(generics) = &ty.generics {
                    let mut params = Vec::new();
                    let mut known_tys = 0;
                    for g in &generics.params {
                        let param_tyid = self.get_tyid(g, scope);
                        let tyir = self.global.get_tyir(param_tyid);
                        params.push(param_tyid);

                        match tyir {
                            TyIr::GenericParam(_) => (),
                            _ => known_tys += 1, //TODO: Generic
                        }
                    }

                    if known_tys == 0 {
                        adt_base_id
                    } else if known_tys == params.len() {
                        self.get_mono_variant(adt_base_id, &params)
                    } else {
                        self.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TypeError::UnknownType)
                                .with_span(|_| ty.span),
                        );
                        TyID::UNKNOWN
                    }
                } else {
                    adt_base_id
                }
            }
            TyKind::Ref(mutable, ty) => from_tyir!(TyIr::Ref(*mutable, self.get_tyid(ty, scope))),
            TyKind::Array(ty, span) => from_tyir!(TyIr::Array(self.get_tyid(ty, scope), *span)),
            TyKind::Ptr(mutable, ty) => from_tyir!(TyIr::Ptr(*mutable, self.get_tyid(ty, scope))),
            e => todo!("{:?}", e),
            // TyKind::Ptr(mutable, ty) => todo!(),
            // TyKind::ArrayRef(mutable, ty) => todo!(),
            // TyKind::Entity(entity_ty) => todo!(),
            // TyKind::Schedule => todo!(),
        }
    }

    pub fn get_mono_variant(&self, id: TyID, params: &Vec<TyID>) -> TyID {
        let map_tyid = |field: &mut FieldDef| {
            let tyir = self.global.get_tyir(field.ty);
            field.ty = match &tyir {
                TyIr::GenericParam(i) => params.get(*i).copied().unwrap(),
                TyIr::ADT(adtdef) => {
                    let mut field_params = Vec::new();
                    for i in &field.params {
                        field_params.push(match self.global.get_tyir(*i) {
                            TyIr::GenericParam(index) => *params.get(index).unwrap(),
                            _ => *i,
                        })
                    }

                    if adtdef.total_generics != field_params.len() {
                        self.diag
                            .push_error(EcslError::new(ErrorLevel::Error, "Mismatched generics"));
                        TyID::UNKNOWN
                    } else {
                        self.get_mono_variant(field.ty, &field_params)
                    }
                }
                _ => field.ty,
            };
        };

        let monos = self.global.monos.mono_map.read().unwrap();
        let key = (id, params.clone());
        if let Some(mono) = monos.get_by_left(&key) {
            *mono
        } else {
            drop(monos);

            let mut tyir = self.global.get_tyir(id).clone();
            let generic_count = tyir.get_generics();

            if params.len() != generic_count {
                self.diag
                    .push_error(EcslError::new(ErrorLevel::Error, "Mismatched generics"));
                return TyID::UNKNOWN;
            }

            match &mut tyir {
                TyIr::ADT(adt_tyir) => {
                    adt_tyir.map(map_tyid);
                    adt_tyir.resolved_generics = adt_tyir.total_generics;

                    let new_tyid = self.global.tyid_from_tyir(tyir);
                    self.global.monos.insert(key, new_tyid);

                    new_tyid
                }
                TyIr::Fn(fn_tyir) => {
                    fn_tyir.map(map_tyid);
                    fn_tyir.resolved_generics = fn_tyir.total_generics;

                    let new_tyid = self.global.tyid_from_tyir(tyir);
                    self.global.monos.insert_mapping(key, new_tyid);

                    new_tyid
                }
                t => panic!("{:?} {:?} {:?}", id, t, params),
            }
        }
    }
    pub fn get_global_id(&self, id: SymbolID) -> Option<GlobalID> {
        {
            let defined = self.defined.read().unwrap();
            if defined.contains_key(&id) {
                return Some(GlobalID::new(id, self.file));
            }
        }
        {
            let imported = self.imported.read().unwrap();
            if let Some(Import::Resolved(mapped_import)) = imported.get(&id) {
                return Some(mapped_import.to);
            }
        }
        None
    }
}
