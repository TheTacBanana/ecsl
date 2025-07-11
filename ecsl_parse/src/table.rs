use cfgrammar::Span;
use ecsl_ast::{data::DataKind, parse::FnKind};
use ecsl_index::{SourceFileID, SymbolID};
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use std::{
    collections::{hash_map::Entry, HashMap},
    sync::RwLock,
};

#[derive(Clone)]
pub struct PartialSymbolTable<'a, 'b> {
    pub file: SourceFileID,
    pub symbols: Vec<Symbol>,
    pub symbol_map: HashMap<String, SymbolID>,
    pub lexer: &'a LRNonStreamingLexer<'a, 'b, DefaultLexerTypes>,
}

pub struct SymbolTable {
    file: SourceFileID,
    symbols: RwLock<Vec<Symbol>>,
    symbol_map: RwLock<HashMap<String, SymbolID>>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    ImportPath,
    ImportItem,

    Ty,
    Local,

    Generic,
    Function(FnKind),
    FunctionUsage,
    FunctionArg,

    EntityAttribute,
    ScheduleItem,

    Struct(DataKind),
    Enum(DataKind),
    Variant,
    VariantUsage,
    Field,
    FieldUsage,
}

impl<'a, 'b> PartialSymbolTable<'a, 'b> {
    pub fn new(
        file: SourceFileID,
        lexer: &'a LRNonStreamingLexer<'a, 'b, DefaultLexerTypes>,
    ) -> Self {
        PartialSymbolTable {
            file,
            symbols: Vec::new(),
            symbol_map: HashMap::new(),
            lexer,
        }
    }

    pub fn define_symbol(&mut self, name: String, _: Span, _: SymbolKind) -> SymbolID {
        let (id, _) = self.create_entry(name);
        // entry.add_definition(kind, span);
        id
    }

    pub fn use_symbol(&mut self, name: String, _: Span, _: SymbolKind) -> SymbolID {
        let (id, _) = self.create_entry(name);
        // entry.add_usage(kind, span);
        id
    }

    pub fn create_entry(&mut self, name: String) -> (SymbolID, &mut Symbol) {
        let symbol_id: SymbolID;
        match self.symbol_map.entry(name.clone()) {
            Entry::Vacant(e) => {
                symbol_id = SymbolID::new(self.symbols.len());
                e.insert(symbol_id);
                self.symbols.push(Symbol { name });
            }
            Entry::Occupied(e) => {
                symbol_id = *e.get();
            }
        }
        (symbol_id, self.symbols.get_mut(symbol_id.inner()).unwrap())
    }

    pub fn finish(self) -> SymbolTable {
        SymbolTable {
            file: self.file,
            symbols: RwLock::new(self.symbols),
            symbol_map: RwLock::new(self.symbol_map),
        }
    }
}

impl SymbolTable {
    pub fn file(&self) -> SourceFileID {
        self.file
    }

    pub fn get_symbol(&self, id: SymbolID) -> Option<Symbol> {
        let lock = self.symbols.read().unwrap();
        lock.get(id.inner()).cloned()
    }

    pub fn get_symbol_from_string(&self, s: &String) -> Option<SymbolID> {
        let lock = self.symbol_map.read().unwrap();
        lock.get(s).cloned()
    }

    pub fn create_entry(&self, name: String) -> SymbolID {
        let symbol_id: SymbolID;
        let mut symbols = self.symbols.write().unwrap();
        let mut symbol_map = self.symbol_map.write().unwrap();
        match symbol_map.entry(name.clone()) {
            Entry::Vacant(e) => {
                symbol_id = SymbolID::new(symbols.len());
                e.insert(symbol_id);
                symbols.push(Symbol { name });
            }
            Entry::Occupied(e) => {
                symbol_id = *e.get();
            }
        }
        symbol_id
    }
}
