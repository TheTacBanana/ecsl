// use ecsl_diagnostics::Diagnostics;

use std::{cell::RefCell, rc::Rc};

use ecsl_ast::SymbolId;
use ecsl_source::SourceFile;
use lrlex::lrlex_mod;
use lrpar::{lrpar_mod, Span, NonStreamingLexer};
use table::{SymbolKind, PartialSymbolTable};

pub mod table;

lrlex_mod!("ecsl.l");
lrpar_mod!("ecsl.y");

pub fn parse_file(source: &SourceFile) {
    let lexer_def = ecsl_l::lexerdef();
    let lexer = lexer_def.lexer(&source.contents);

    let table = Rc::new(RefCell::new(PartialSymbolTable::new(&lexer)));

    let (ast, errs) = ecsl_y::parse(&lexer, table.clone());
    println!("{:#?}", ast);
    for e in errs {
        println!("{}", e)
    }
}


pub trait GenerateIdentExt {
    fn new_ident(&self, span: Span, kind: SymbolKind) -> SymbolId;
}

impl GenerateIdentExt for Rc<RefCell<PartialSymbolTable<'_, '_>>> {
    fn new_ident(&self, span: Span, kind: SymbolKind) -> SymbolId {
        let mut s = self.borrow_mut();
        let symbol_string = s.lexer.span_str(span).to_string();
        let id = s.insert_symbol(symbol_string, span, kind);
        id
    }
}
