use cfgrammar::Span;
use ecsl_ast::{
    data::{EnumDef, StructDef},
    parse::FnDef,
    ty::{Generics, Ty},
};
use ecsl_index::SymbolID;

use crate::P;

#[derive(Debug)]
pub enum Definition {
    Struct(StructDef),
    Enum(EnumDef),
    Function(FnDef),
    AssocFunction(Option<Generics>, P<Ty>, FnDef),
}

impl Definition {
    pub fn ident(&self) -> SymbolID {
        match self {
            Definition::Struct(s) => s.ident,
            Definition::Enum(e) => e.ident,
            Definition::Function(f) => f.ident,
            Definition::AssocFunction(_, _, f) => f.ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Definition::Struct(s) => s.span,
            Definition::Enum(e) => e.span,
            Definition::Function(f) => f.span,
            Definition::AssocFunction(_, _, f) => f.span,
        }
    }
}
