use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{SourceFileID, SymbolID};
use ecsl_parse::table::SymbolTable;

use crate::{
    assoc::ImplBlock,
    def::Definition,
    import::{Import, ImportPath},
    ImportError, TyCtxt,
};

pub struct LocalTyCtxt {
    pub file: SourceFileID,
    pub table: Arc<SymbolTable>,
    pub global: Arc<TyCtxt>,
    pub diag: DiagConn,

    /// Import Mappings
    pub imported: RwLock<BTreeMap<SymbolID, Import>>,

    pub defined: RwLock<BTreeMap<SymbolID, Definition>>,
    pub impl_blocks: RwLock<BTreeMap<SymbolID, ImplBlock>>,
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
            impl_blocks: Default::default(),
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
}
