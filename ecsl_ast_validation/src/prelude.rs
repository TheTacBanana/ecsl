use ecsl_ast::{
    item::{UseDef, UsePath},
    parse::Attribute,
    visit::{Visitor, VisitorCF},
};
use ecsl_index::SourceFileID;
use ecsl_parse::{table::SymbolTable, LexerTy};
use lrpar::NonStreamingLexer;
use std::sync::Arc;

#[derive(Debug)]
pub struct Prelude {
    pub imports: Vec<UseDef>,
    pub id: SourceFileID,
}

impl Prelude {
    pub fn new(id: SourceFileID) -> Self {
        Self {
            imports: Vec::new(),
            id,
        }
    }
}

impl Visitor for Prelude {
    fn visit_use(&mut self, u: &UseDef) -> VisitorCF {
        let Some(attr) = &u.attributes else {
            return VisitorCF::Continue;
        };

        if attr.has_attribute(&Attribute::Marker("prelude".to_string())) {
            let mut u = u.clone();
            u.attributes = None;
            self.imports.push(u);
        }

        VisitorCF::Continue
    }
}

pub fn rewrite_use_path(u: &mut UsePath, table: &Arc<SymbolTable>, lexer: &LexerTy) {
    match u {
        UsePath::Super(_, ref mut use_path) => {
            rewrite_use_path(use_path, table, lexer);
        }
        UsePath::Single(span, id, ref mut use_path) => {
            let s = lexer.span_str(*span).to_string();
            let new_id = table.create_entry(s);
            *id = new_id;
            rewrite_use_path(use_path, table, lexer);
        }
        UsePath::Multiple(_, vec) => {
            for use_path in vec {
                rewrite_use_path(use_path, table, lexer);
            }
        }
        UsePath::Item(span, id) => {
            let s = lexer.span_str(*span).to_string();
            let new_id = table.create_entry(s);
            *id = new_id;
        }
    }
}
