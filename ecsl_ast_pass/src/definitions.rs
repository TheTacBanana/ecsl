use ecsl_ast::{
    data::{EnumDef, StructDef},
    item::{ImplBlock, Item, ItemKind},
    parse::FnDef,
    visit::{walk_item, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_ty::{def::Definition, local::LocalTyCtxt};
use std::sync::Arc;

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
        if i.ty.into_scope().is_none() {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, "Cannot impl for Type").with_span(|_| i.ty.span),
            );
            return VisitorCF::Continue;
        }

        for f in &i.fn_defs {
            self.ty_ctxt.define_symbol(Definition::AssocFunction(
                i.generics.clone(),
                i.ty.clone(),
                f.clone(),
            ));
        }
        VisitorCF::Continue
    }
}
