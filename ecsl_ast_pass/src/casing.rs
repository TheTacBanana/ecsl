use convert_case::{Boundary, Case, Casing};
use ecsl_ast::{
    data::{EnumDef, FieldDef, StructDef, VariantDef},
    parse::{AttributeMarker, Attributes, FnDef},
    stmt::{Stmt, StmtKind},
    visit::*,
};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::SymbolID;
use ecsl_parse::table::SymbolTable;
use lrpar::Span;
use std::sync::Arc;

pub struct CasingWarnings {
    diag: DiagConn,
    table: Arc<SymbolTable>,
}

impl CasingWarnings {
    pub fn new(diag: DiagConn, table: Arc<SymbolTable>) -> Self {
        Self { diag, table }
    }

    fn test_casing(
        &self,
        case: CasingWarning,
        symbol: SymbolID,
        span: Span,
        attr: Option<&Attributes>,
    ) {
        let symbol = self.table.get_symbol(symbol).unwrap();

        if attr.is_some_and(|attr| attr.get_marker(AttributeMarker::AllowCasing)) {
            return;
        }

        if symbol.name
            == symbol
                .name
                .without_boundaries(&Boundary::digits())
                .to_case(match case {
                    CasingWarning::SnakeCase => Case::Snake,
                    CasingWarning::PascalCase => Case::Pascal,
                })
        {
            return;
        }

        self.diag
            .push_error(EcslError::new(ErrorLevel::Warning, case).with_span(|_| span));
    }
}

impl Visitor for CasingWarnings {
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        self.test_casing(
            CasingWarning::SnakeCase,
            f.ident,
            f.span,
            Some(&f.attributes),
        );

        for param in f.params.iter() {
            self.test_casing(CasingWarning::SnakeCase, param.symbol(), param.span, None);
        }

        walk_fn(self, f)
    }

    fn visit_struct_def(&mut self, s: &StructDef) -> VisitorCF {
        self.test_casing(
            CasingWarning::PascalCase,
            s.ident,
            s.span,
            Some(&s.attributes),
        );
        walk_struct_def(self, s)
    }

    fn visit_enum_def(&mut self, e: &EnumDef) -> VisitorCF {
        self.test_casing(
            CasingWarning::PascalCase,
            e.ident,
            e.span,
            Some(&e.attributes),
        );
        walk_enum_def(self, e)
    }

    fn visit_variant_def(&mut self, v: &VariantDef) -> VisitorCF {
        self.test_casing(CasingWarning::PascalCase, v.ident, v.span, None);
        walk_variant_def(self, v)
    }

    fn visit_field_def(&mut self, f: &FieldDef) -> VisitorCF {
        self.test_casing(CasingWarning::SnakeCase, f.ident, f.span, None);
        VisitorCF::Continue
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        if let StmtKind::Let(_, id, span, _, _) = &s.kind {
            self.test_casing(CasingWarning::SnakeCase, *id, *span, None);
        }
        VisitorCF::Continue
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CasingWarning {
    SnakeCase,
    PascalCase,
}

impl std::fmt::Display for CasingWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CasingWarning::SnakeCase => "Casing should be snake_case",
            CasingWarning::PascalCase => "Casing should be PascalCase",
        };
        write!(f, "{}", s)
    }
}
