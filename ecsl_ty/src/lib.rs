use cfgrammar::Span;
use ecsl_ast::{
    data::{EnumDef, StructDef},
    parse::FnHeader,
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{SourceFileID, SymbolID};
use ecsl_parse::table::SymbolTable;
use import::ImportPath;
use std::{
    collections::BTreeMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

pub mod import;

pub struct TyCtxt {
    pub sources: RwLock<BTreeMap<SourceFileID, Arc<LocalTyCtxt>>>,
}

pub struct LocalTyCtxt {
    pub file: SourceFileID,
    pub table: Arc<SymbolTable>,
    pub global: Arc<TyCtxt>,

    pub errors: RwLock<Vec<EcslError>>,

    pub defined: RwLock<BTreeMap<SymbolID, TypeDef>>,
    pub imported: RwLock<BTreeMap<SymbolID, ImportPath>>,
}

#[derive(Debug)]
pub enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
    Function(FnHeader),
}

impl TypeDef {
    pub fn ident(&self) -> SymbolID {
        match self {
            TypeDef::Struct(s) => s.ident,
            TypeDef::Enum(e) => e.ident,
            TypeDef::Function(f) => f.ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            TypeDef::Struct(s) => s.span,
            TypeDef::Enum(e) => e.span,
            TypeDef::Function(f) => f.span,
        }
    }
}

pub enum TyCtxtError<'a> {
    // Multiple occurences
    MultipleImports(&'a str),
    MultipleDefinitions(&'a str),
    ImportedDefinedConflict(&'a str),

    // Import error
    ImportDoesntExist(&'a PathBuf, &'a str),
    SelfImport,
}

impl<'a> std::fmt::Display for TyCtxtError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TyCtxtError::MultipleImports(s) => &format!("Symbol '{}' imported multiple times", s),
            TyCtxtError::MultipleDefinitions(s) => {
                &format!("Symbol '{}' defined multiple times", s)
            }
            TyCtxtError::SelfImport => &format!("Cannot import items from same file"),
            TyCtxtError::ImportDoesntExist(p, s) => &format!(
                "Symbol '{}' cannot be imported from path '{}'",
                s,
                p.to_string_lossy()
            ),
            TyCtxtError::ImportedDefinedConflict(s) => {
                &format!("Symbol '{}' is imported and defined", s)
            }
        };
        write!(f, "{s}")
    }
}

pub trait LocalTyCtxtExt {
    fn new_local_ctxt(&self, file: SourceFileID, table: Arc<SymbolTable>) -> Arc<LocalTyCtxt>;
}

impl LocalTyCtxtExt for Arc<TyCtxt> {
    fn new_local_ctxt(&self, file: SourceFileID, table: Arc<SymbolTable>) -> Arc<LocalTyCtxt> {
        let lctx = Arc::new(LocalTyCtxt {
            file,
            global: self.clone(),
            table,
            errors: Default::default(),
            imported: Default::default(),
            defined: Default::default(),
        });
        self.sources.write().unwrap().insert(file, lctx.clone());
        lctx
    }
}

impl TyCtxt {
    pub fn new() -> Self {
        TyCtxt {
            sources: Default::default(),
        }
    }
}

impl LocalTyCtxt {
    pub fn push_error(&self, err: EcslError) {
        self.errors.write().unwrap().push(err);
    }

    pub fn define_symbol(&self, def: TypeDef) {
        let symbol = def.ident();
        if self.defined.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    TyCtxtError::MultipleDefinitions(&symbol.name),
                )
                .with_span(|_| def.span()),
            );

            return;
        }
        self.defined.write().unwrap().insert(def.ident(), def);
    }

    pub fn import_symbol(&self, import: ImportPath) {
        let symbol = import.symbol;
        if self.defined.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    TyCtxtError::MultipleDefinitions(&symbol.name),
                )
                .with_span(|_| import.span),
            );
            return;
        }

        if self.imported.read().unwrap().contains_key(&symbol) {
            let symbol = self.table.get_symbol(symbol).unwrap();
            self.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    TyCtxtError::MultipleImports(&symbol.name),
                )
                .with_span(|_| import.span),
            );
            return;
        }

        {
            let mut lock = self.imported.write().unwrap();
            lock.insert(symbol, import);
        }
    }

    pub fn use_symbol(&self, symbol: SymbolID, f: impl FnOnce(&LocalTyCtxt, &TypeDef)) {
        todo!() //TODO:

        // if let Some(def) = self.defined.get(&symbol) {
        // f(self, def)
        // }

        // {
        //     let lock = self.global.
        // }

        // if self.imported.contains(&symbol) {
        //     f(self, def)
        // }
    }
}
