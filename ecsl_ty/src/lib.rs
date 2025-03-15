use ctxt::TyCtxt;
use ecsl_ast::{
    data::DataKind,
    expr::{Literal, RangeType},
    parse::FnKind,
    ty::Mutable,
};
use ecsl_index::{FieldID, SymbolID, TyID, VariantID};
use log::debug;
use std::{
    collections::{BTreeMap, VecDeque},
    ops::BitAnd,
    sync::Arc,
};

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
    /// Intrinsic string type
    String,
    /// Range over a numeric type
    Range(TyID, RangeType),
    /// Reference to another type
    Ref(Mutable, TyID),
    /// ADT types
    ADT(ADTDef),
    /// A function type
    Fn(FnDef),
    // /// A monomorphized adt variant
    // MonoADT(MonoADTDef),
    // /// A monomorphized function variant
    MonoFn(MonoFnDef),
    /// A sized array type
    Array(TyID, usize),
    ArrayRef(Mutable, TyID),
    GenericParam(usize),
}

impl From<Literal> for TyIr {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Int => TyIr::Int,
            Literal::Float => TyIr::Float,
            Literal::String => TyIr::String,
            Literal::Char => TyIr::Char,
            Literal::Bool => TyIr::Bool,
        }
    }
}

impl TyIr {
    pub fn get_generics(&self) -> Option<usize> {
        match self {
            TyIr::ADT(adtdef) => adtdef.generics,
            TyIr::Fn(fn_def) => fn_def.generics,
            _ => None,
        }
    }

    pub fn into_adt(self) -> Option<ADTDef> {
        match self {
            TyIr::ADT(adtdef) => Some(adtdef),
            _ => None,
        }
    }
    pub fn into_fn(self) -> Option<FnDef> {
        match self {
            TyIr::Fn(fndef) => Some(fndef),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ADTDef {
    pub id: TyID,
    pub kind: DataKind,
    pub variant_hash: BTreeMap<String, VariantID>, // TODO: Temporary solution to getting the variants of an enum, pls fix
    pub variant_kinds: BTreeMap<VariantID, VariantDef>,
    pub generics: Option<usize>,
}

impl ADTDef {
    pub fn is_struct(&self) -> bool {
        return self.variant_hash.is_empty();
    }

    pub fn is_enum(&self) -> bool {
        return !self.variant_hash.is_empty();
    }

    pub fn discriminant_size(&self) -> Option<usize> {
        if self.is_struct() {
            return None;
        }

        let byte_length = (self.variant_kinds.len().ilog2() as usize) / 8 + 1;
        return Some(byte_length);
    }

    // Will panic if not a struct
    pub fn get_struct_fields(&self) -> &VariantDef {
        if !self.is_struct() {
            panic!("Not a struct");
        }
        self.variant_kinds.get(&VariantID::ZERO).as_ref().unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Generics {
    pub generics: Vec<TyID>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDef {
    pub id: VariantID,
    pub field_hash: BTreeMap<String, FieldID>, // TODO: Temporary solution to getting the fields of a variant, pls fix
    pub field_tys: BTreeMap<FieldID, FieldDef>,
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
    pub ret: TyID,
    pub generics: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoFnDef {
    /// Original Fn
    pub tyid: TyID,
    // Generic Tys
    pub mono: BTreeMap<TyID, TyID>,
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

    pub fn add_opt(&mut self, g: Option<ecsl_ast::ty::Generics>) -> Option<usize> {
        if let Some(g) = g {
            let len = g.params.len();
            self.scopes.push(g);
            return Some(len);
        }
        return None;
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
