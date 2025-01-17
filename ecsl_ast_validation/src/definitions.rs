use std::sync::Arc;

use ecsl_ast::{
    data::{EnumDef, StructDef},
    item::{ImplBlock, Item, ItemKind},
    parse::FnDef,
    visit::{walk_item, FnCtxt, Visitor, VisitorCF},
};
use ecsl_ty::{def::Definition, local::LocalTyCtxt};

pub struct TypeDefCollector {
    ty_ctxt: Arc<LocalTyCtxt>,
}

impl TypeDefCollector {
    pub fn new<'a>(ty_ctxt: Arc<LocalTyCtxt>) -> TypeDefCollector {
        TypeDefCollector { ty_ctxt }
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
        self.ty_ctxt.define_symbol(Definition::Struct(s.clone()));
        VisitorCF::Continue
    }

    fn visit_enum_def(&mut self, e: &EnumDef) -> VisitorCF {
        self.ty_ctxt.define_symbol(Definition::Enum(e.clone()));
        VisitorCF::Continue
    }

    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
        match ctxt {
            FnCtxt::Free => {
                self.ty_ctxt.define_symbol(Definition::Function(f.clone()));
            }
            _ => (),
        }
        VisitorCF::Continue
    }

    fn visit_impl(&mut self, i: &ImplBlock) -> VisitorCF {
        // match i.ty.kind {
        //     TyKind::Ident(symbol_id, concrete_generics) => todo!(),
        //     TyKind::Array(ty, span) => todo!(),
        //     TyKind::ArrayRef(mutable, ty) => todo!(),
        //     TyKind::Ref(mutable, ty) => todo!(),
        //     TyKind::Ptr(mutable, ty) => todo!(),
        //     TyKind::Entity(entity_ty) => todo!(),
        //     TyKind::Query => todo!(),
        //     TyKind::System => todo!(),
        //     TyKind::Schedule => todo!(),
        // }

        for f in &i.fn_defs {
            // println!("{:?}", f)
        }
        VisitorCF::Continue
    }
}
