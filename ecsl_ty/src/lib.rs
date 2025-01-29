use std::collections::BTreeMap;

use ecsl_ast::{data::DataKind, parse::FnKind, ty::Mutable};
use ecsl_index::{FieldID, SymbolID, TyID, VariantID};

pub mod ctxt;
pub mod def;
pub mod import;
pub mod local;

pub type P<T> = Box<T>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyIr {
    /// Type is unknown and should be propagated as an error
    Unknown,
    // The bottom/none type
    Bottom,
    /// Intrinsic boolean type
    Bool,
    /// Intrinsic char type
    Char,
    /// Intrinsic int type
    Int,
    /// Intrinsic float type
    Float,
    /// Reference to another type
    Ref(Mutable, TyID),
    /// A struct type
    Struct(StructDef),
    /// An enum type
    Enum(EnumDef),
    /// A function type
    Fn(FnDef),
    /// A sized array type
    Array(TyID, usize),
    ArrayRef(Mutable, TyID),
    GenericParam(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub id: TyID,
    pub kind: DataKind,
    pub fields: BTreeMap<FieldID, FieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub id: TyID,
    pub kind: DataKind,
    pub variants: BTreeMap<FieldID, VariantDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Generics {
    pub generics: Vec<TyID>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDef {
    pub id: VariantID,
    pub fields: BTreeMap<FieldID, FieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef {
    pub id: FieldID,
    pub ty: TyID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub tyid: TyID,
    pub kind: FnKind,
    pub params: Vec<TyID>,
    pub ret: Option<TyID>,
}

#[derive(Debug)]
pub struct GenericsScope {
    pub scopes: Vec<ecsl_ast::ty::Generics>,
}

impl GenericsScope {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn add(&mut self, g: ecsl_ast::ty::Generics) {
        self.scopes.push(g);
    }

    pub fn add_opt(&mut self, g: Option<ecsl_ast::ty::Generics>) {
        if let Some(g) = g {
            self.scopes.push(g);
        }
    }

    pub fn scope_index(&self, id: SymbolID) -> Option<usize> {
        let mut i = 0;
        for s in &self.scopes {
            for p in &s.params {
                if p.ident == id {
                    return Some(i);
                }
                i += 1;
            }
        }
        return None;
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }
}

pub enum TypeError {
    UnknownType,
}

impl<'a> std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TypeError::UnknownType => &format!("Could not resolve type"),
        };
        write!(f, "{s}")
    }
}
