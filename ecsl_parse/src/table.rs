use cfgrammar::Span;
use ecsl_ast::{data::DataKind, parse::FnKind};
use ecsl_index::{SourceFileID, SymbolID};
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Clone)]
pub struct PartialSymbolTable<'a, 'b> {
    pub file: SourceFileID,
    pub symbols: Vec<Symbol>,
    pub symbol_map: HashMap<String, SymbolID>,
    pub lexer: &'a LRNonStreamingLexer<'a, 'b, DefaultLexerTypes>,
}

#[derive(Clone)]
pub struct SymbolTable {
    pub file: SourceFileID,
    pub symbols: Vec<Symbol>,
    pub symbol_map: HashMap<String, SymbolID>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub definitions: Vec<(SymbolKind, Span)>,
    pub usages: Vec<(SymbolKind, Span)>,
}

impl Symbol {
    pub fn add_definition(&mut self, kind: SymbolKind, span: Span) {
        self.definitions.push((kind, span));
    }

    pub fn add_usage(&mut self, kind: SymbolKind, span: Span) {
        self.usages.push((kind, span));
    }
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

    pub fn define_symbol(&mut self, name: String, span: Span, kind: SymbolKind) -> SymbolID {
        let (id, entry) = self.create_entry(name);
        entry.add_usage(kind, span);
        id
    }

    pub fn use_symbol(&mut self, name: String, span: Span, kind: SymbolKind) -> SymbolID {
        let (id, entry) = self.create_entry(name);
        entry.add_usage(kind, span);
        id
    }

    pub fn create_entry(&mut self, name: String) -> (SymbolID, &mut Symbol) {
        let symbol_id: SymbolID;
        match self.symbol_map.entry(name.clone()) {
            Entry::Vacant(e) => {
                symbol_id = SymbolID(self.symbols.len() as u32);
                e.insert(symbol_id);
                self.symbols.push(Symbol {
                    name: name,
                    definitions: Vec::new(),
                    usages: Vec::new(),
                });
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
            symbols: self.symbols,
            symbol_map: self.symbol_map,
        }
    }
}

impl SymbolTable {
    pub fn get_symbol(&self, id: SymbolID) -> Option<&Symbol> {
        self.symbols.get(id.inner())
    }

    pub fn get_symbol_mut(&mut self, id: SymbolID) -> Option<&mut Symbol> {
        self.symbols.get_mut(id.inner())
    }
}
