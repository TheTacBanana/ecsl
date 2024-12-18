use std::sync::Arc;

use ecsl_ast::{
    data::{EnumDef, StructDef},
    item::{Item, ItemKind},
    parse::FnDef,
    visit::{walk_item, FnCtxt, Visitor, VisitorCF}, // SymbolID,
};
use ecsl_error::EcslError;
use ecsl_ty::{LocalTyCtxt, TypeDef};

pub struct TypeDefCollector {
    pub errors: Vec<EcslError>,
    pub ty_ctxt: Arc<LocalTyCtxt>,
}

impl TypeDefCollector {
    pub fn new<'a>(ty_ctxt: Arc<LocalTyCtxt>) -> TypeDefCollector {
        TypeDefCollector {
            errors: Vec::new(),
            ty_ctxt,
        }
    }
}

impl Visitor for TypeDefCollector {
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Fn(_) | ItemKind::Impl(_) => {
                walk_item(self, i)
            }
            _ => VisitorCF::Continue,
        }
    }

    fn visit_struct_def(&mut self, s: &StructDef) -> VisitorCF {
        self.ty_ctxt.define_symbol(TypeDef::Struct(s.clone()));
        VisitorCF::Continue
    }

    fn visit_enum_def(&mut self, e: &EnumDef) -> VisitorCF {
        self.ty_ctxt.define_symbol(TypeDef::Enum(e.clone()));
        VisitorCF::Continue
    }

    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
        match ctxt {
            FnCtxt::Free => {
                self.ty_ctxt.define_symbol(TypeDef::Function(f.to_header()));
            }
            FnCtxt::Impl => {
                todo!("Assoc Functions not yet implemented") //TODO:
            }
        }
        VisitorCF::Continue
    }
}
