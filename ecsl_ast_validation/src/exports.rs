use std::collections::BTreeSet;

use ecsl_ast::{
    data::StructDef,
    item::{Item, ItemKind},
    visit::{walk_item, Visitor, VisitorCF},
    // SymbolID,
};
use ecsl_error::EcslError;
use ecsl_index::SymbolID;
use ecsl_parse::table::SymbolTable;

pub struct TypeDefCollector<'a> {
    pub errors: Vec<EcslError>,
    pub table: &'a SymbolTable,
    pub found_symbols: BTreeSet<SymbolID>,
    pub defined_items: Vec,
}

impl TypeDefCollector<'_> {
    pub fn new<'a>(table: &'a SymbolTable) -> TypeDefCollector<'a> {
        TypeDefCollector {
            errors: Vec::new(),
            table,
            found_symbols: BTreeSet::new(),
            defined_items: Vec::new(),
        }
    }
}

impl Visitor for TypeDefCollector<'_> {
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Struct(_) | ItemKind::Enum(_) => walk_item(self, i),
            _ => VisitorCF::Continue,
        }
    }

    fn visit_struct_def(&mut self, s: &StructDef) -> VisitorCF {}
}
