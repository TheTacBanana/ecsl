use crate::{
    ctxt::{ImportError, TyCtxt},
    def::Definition,
    import::{Import, ImportPath},
    GenericsScope, TyIr, TypeError,
};
use core::panic;
use ecsl_ast::ty::{Ty, TyKind};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{GlobalID, SourceFileID, SymbolID, TyID};
use ecsl_parse::table::SymbolTable;
use log::debug;
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
            TyKind::Ident(symbol_id) => resolve_tyid!(symbol_id),
            TyKind::Ref(mutable, ty) => from_tyir!(TyIr::Ref(*mutable, self.get_tyid(ty, scope))),
            TyKind::Array(ty, span) => from_tyir!(TyIr::Array(self.get_tyid(ty, scope), *span)),
            e => todo!("{:?}", e),
            // TyKind::Ptr(mutable, ty) => todo!(),
            // TyKind::ArrayRef(mutable, ty) => todo!(),
            // TyKind::Entity(entity_ty) => todo!(),
            // TyKind::Schedule => todo!(),
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
