use cfgrammar::Span;
use ecsl_ast::{
    data::{EnumDef, StructDef},
    parse::FnHeader,
};
use ecsl_index::SymbolID;

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
