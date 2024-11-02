use cfgrammar::Span;
use ecsl_ast::{data::DataKind, SymbolId};
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use std::collections::{hash_map::Entry, HashMap};

pub struct PartialSymbolTable<'a, 'b> {
    pub symbols: Vec<Symbol>,
    pub symbol_map: HashMap<String, SymbolId>,
    pub lexer: &'a LRNonStreamingLexer<'a, 'b, DefaultLexerTypes>,
}

pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
    pub symbol_map: HashMap<String, SymbolId>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub first: Span,
    pub used: Vec<Span>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    Local,
    Generic,
    Function,
    FunctionArg,
    Struct(DataKind),
    Enum(DataKind),
    Variant,
    FieldDef,
}

impl<'a, 'b> PartialSymbolTable<'a, 'b> {
    pub fn new(lexer: &'a LRNonStreamingLexer<'a, 'b, DefaultLexerTypes>) -> Self {
        PartialSymbolTable {
            symbols: Vec::new(),
            symbol_map: HashMap::new(),
            lexer,
        }
    }

    pub fn insert_symbol(&mut self, name: String, span: Span, kind: SymbolKind) -> SymbolId {
        let symbol_id: SymbolId;
        match self.symbol_map.entry(name) {
            Entry::Vacant(e) => {
                symbol_id = SymbolId(self.symbols.len() as u32);
                e.insert(symbol_id);
                self.symbols.push(Symbol {
                    kind,
                    first: span,
                    used: Vec::new(),
                });
            }
            Entry::Occupied(e) => {
                symbol_id = *e.get();
                let symbol = &mut self.symbols[symbol_id.inner()];
                symbol.used.push(span);
            }
        }
        symbol_id
    }

    pub fn finish(self) -> SymbolTable {
        SymbolTable {
            symbols: self.symbols,
            symbol_map: self.symbol_map,
        }
    }
}
