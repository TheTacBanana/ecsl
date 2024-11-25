use std::{collections::BTreeSet, path::PathBuf};

use ecsl_ast::{
    item::{Item, ItemKind, UseDef, UsePath},
    visit::{walk_item, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_imports::ImportPath;
use ecsl_index::SymbolID;
use ecsl_parse::table::SymbolTable;

pub struct ImportCollector<'a> {
    pub errors: Vec<EcslError>,
    pub table: &'a SymbolTable,
    pub imported_symbols: BTreeSet<SymbolID>,
    pub imported_items: Vec<ImportPath>,
}

#[derive(Debug, Clone)]
pub enum ImportError {
    MultipleImport(String),
}

impl std::fmt::Display for ImportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ImportError::MultipleImport(s) => {
                &format!("Symbol '{}' imported multiple times", s.as_str())
            }
        };
        write!(f, "{s}")
    }
}

impl ImportCollector<'_> {
    pub fn new<'a>(table: &'a SymbolTable) -> ImportCollector<'a> {
        ImportCollector {
            errors: Vec::new(),
            table,
            imported_symbols: BTreeSet::new(),
            imported_items: Vec::new(),
        }
    }
}

impl Visitor for ImportCollector<'_> {
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Use(_) => walk_item(self, i),
            _ => VisitorCF::Continue,
        }
    }

    fn visit_use(&mut self, u: &UseDef) -> VisitorCF {
        let mut queue = vec![(PathBuf::new(), &*u.path)];
        while let Some((mut buf, path)) = queue.pop() {
            match path {
                UsePath::Super(_, next) => {
                    buf.push("..");
                    queue.push((buf, &next));
                }
                UsePath::Single(_, id, next) => {
                    let symbol = self.table.get_symbol(*id).unwrap();
                    buf.push(&symbol.name);
                    queue.push((buf, &next));
                }
                UsePath::Item(span, id) => {
                    self.imported_items.push(ImportPath {
                        relative_path: buf,
                        symbol: *id,
                        import_from: self.table.file,
                    });

                    // If already contains symbol
                    if !self.imported_symbols.insert(*id) {
                        let symbol = self.table.get_symbol(*id).unwrap();
                        self.errors.push(
                            EcslError::new(
                                ErrorLevel::Error,
                                ImportError::MultipleImport(symbol.name.to_string()),
                            )
                            .with_span(|_| *span),
                        );
                    }
                }
                UsePath::Multiple(_, next) => {
                    for n in next {
                        queue.push((buf.clone(), &n));
                    }
                }
            }
        }
        VisitorCF::Continue
    }
}
