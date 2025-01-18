use ecsl_ast::{
    item::{Item, ItemKind, UseDef, UsePath},
    parse::Attribute,
    visit::{walk_item, Visitor, VisitorCF},
};
use ecsl_context::Context;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::GlobalID;
use ecsl_ty::{import::ImportPath, local::LocalTyCtxt};
use std::{collections::VecDeque, path::PathBuf, sync::Arc};

pub struct ImportCollector<'a> {
    pub ctxt: &'a Context,
    pub ty_ctxt: Arc<LocalTyCtxt>,
}

impl<'a> ImportCollector<'a> {
    pub fn new(ctxt: &'a Context, ty_ctxt: Arc<LocalTyCtxt>) -> ImportCollector<'a> {
        ImportCollector { ctxt, ty_ctxt }
    }
}

impl<'a> Visitor for ImportCollector<'a> {
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Use(_) => walk_item(self, i),
            _ => VisitorCF::Continue,
        }
    }

    fn visit_use(&mut self, u: &UseDef) -> VisitorCF {
        if let Some(attributes) = &u.attributes {
            if attributes.has_attribute(&Attribute::Marker("prelude".to_string())) {
                if self.ctxt.in_std(self.ty_ctxt.file) {
                    return VisitorCF::Continue;
                } else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(
                            ErrorLevel::Error,
                            "Cannot use prelude attribute outside of std",
                        )
                        .with_span(|_| u.span),
                    );
                }
            }
        }

        let mut queue = VecDeque::from(vec![(PathBuf::new(), &*u.path)]);
        while let Some((mut buf, path)) = queue.pop_front() {
            match path {
                UsePath::Super(_, next) => {
                    buf.push("..");
                    queue.push_back((buf, &next));
                }
                UsePath::Single(_, id, next) => {
                    let symbol = self.ty_ctxt.table.get_symbol(*id).unwrap();
                    buf.push(&symbol.name);
                    queue.push_back((buf, &next));
                }
                UsePath::Item(span, id) => {
                    self.ty_ctxt.import_symbol(ImportPath {
                        path: buf,
                        from: GlobalID::new(*id, self.ty_ctxt.file),
                        span: *span,
                    });
                }
                UsePath::Multiple(_, next) => {
                    for n in next {
                        queue.push_back((buf.clone(), &n));
                    }
                }
            }
        }
        VisitorCF::Continue
    }
}
