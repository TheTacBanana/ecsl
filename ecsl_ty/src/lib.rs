use ecsl_ast::{
    data::DataKind,
    expr::{Literal, RangeType},
    parse::{Attributes, FnKind},
    ty::Mutable,
};
use ecsl_index::{FieldID, SymbolID, TyID, VariantID};
use std::collections::BTreeMap;

pub mod ctxt;
pub mod def;
pub mod import;
pub mod local;
pub mod mono;

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
    Str,
    /// Range over a numeric type
    Range(TyID, RangeType),
    /// Reference to another type
    Ref(Mutable, FieldDef),
    /// Pointer to another type
    Ptr(Mutable, FieldDef),
    /// ADT types
    ADT(ADTDef),
    /// A function type
    Fn(FnDef),
    /// Entity Type
    Entity,
    /// Schedule Type
    Schedule,
    /// Generic param which is then monomorphised
    GenericParam(usize),
    /// A sized array type
    Array(TyID, usize),
    /// Reference to an array of type
    ArrayRef(Mutable, FieldDef),
}

impl From<Literal> for TyIr {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Int => TyIr::Int,
            Literal::Float => TyIr::Float,
            Literal::String => TyIr::Str,
            Literal::Char => TyIr::Char,
            Literal::Bool => TyIr::Bool,
        }
    }
}

impl TyIr {
    pub fn get_generics(&self) -> usize {
        match self {
            TyIr::ADT(adtdef) => adtdef.total_generics,
            TyIr::Fn(fn_def) => fn_def.total_generics,
            _ => 0,
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

    pub fn into_fmt_string(&self) -> String {
        let s = match self {
            TyIr::Unknown => "?",
            TyIr::Bottom => "()",
            TyIr::Bool => "bool",
            TyIr::Char => "char",
            TyIr::Int => "int",
            TyIr::Float => "float",
            TyIr::Str => "str",
            TyIr::Range(tyid, range_type) => &format!("{}{}{}", tyid, range_type, tyid),
            TyIr::Ref(mutable, tyid) => &format!("&{} {}", mutable, tyid),
            TyIr::Ptr(mutable, tyid) => &format!("*{} {}", mutable, tyid),
            TyIr::ADT(adtdef) => &format!("{}!", adtdef.id),
            TyIr::Fn(_) => "fn",
            TyIr::Array(tyid, n) => &format!("[{}:{}]", tyid, n),
            TyIr::ArrayRef(mutable, tyid) => &format!("&{} [{}]", mutable, tyid),
            TyIr::GenericParam(i) => &format!("T{}", i + 1),
            TyIr::Entity => "Entity",
            TyIr::Schedule => "Schedule",
        };
        s.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ADTDef {
    pub id: TyID,
    pub kind: DataKind,
    pub variant_hash: BTreeMap<String, VariantID>, // TODO: Temporary solution to getting the variants of an enum, pls fix
    pub variant_kinds: BTreeMap<VariantID, VariantDef>,

    pub resolved_generics: usize,
    pub total_generics: usize,

    pub attributes: Attributes,
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

    pub fn map(&mut self, mut f: impl FnMut(&mut FieldDef)) {
        for (_, var) in &mut self.variant_kinds {
            for (_, field) in &mut var.field_tys {
                f(field);
            }
        }
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

impl VariantDef {
    pub fn new(id: VariantID) -> Self {
        Self {
            id,
            field_hash: Default::default(),
            field_tys: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef {
    pub id: FieldID,
    pub ty: TyID,
    pub params: Vec<TyID>,
}

impl std::fmt::Display for FieldDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)?;
        if !self.params.is_empty() {
            write!(f, "<")?;
            for p in self.params.iter() {
                write!(f, "{},", p)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnParent {
    None,
    Ref(Mutable, FieldDef),
    Value(Mutable, FieldDef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub parent: FnParent,

    pub tyid: TyID,
    pub kind: FnKind,
    pub params: BTreeMap<FieldID, FieldDef>,
    pub ret: FieldDef,

    pub resolved_generics: usize,
    pub total_generics: usize,

    pub attributes: Attributes,
}

impl FnDef {
    pub fn map(&mut self, mut f: impl FnMut(&mut FieldDef)) {
        match &mut self.parent {
            FnParent::None => (),
            FnParent::Ref(_, field_def) | FnParent::Value(_, field_def) => f(field_def),
        }
        for (_, field) in &mut self.params {
            f(field);
        }
        f(&mut self.ret);
    }
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

    pub fn total(&self) -> usize {
        let mut i = 0;
        for s in &self.scopes {
            for _ in &s.params {
                i += 1;
            }
        }
        i
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
