use std::sync::Arc;

use convert_case::{Case, Casing};
use ecsl_ast::{
    data::{EnumDef, FieldDef, StructDef, VariantDef},
    parse::{Attribute, AttributeMarker, FnDef},
    visit::{
        walk_enum_def, walk_field_def, walk_fn, walk_struct_def, walk_variant_def, FnCtxt, Visitor,
        VisitorCF,
    },
};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_parse::table::SymbolTable;

pub struct CasingWarnings {
    diag: DiagConn,
    table: Arc<SymbolTable>,
}

impl CasingWarnings {
    pub fn new(diag: DiagConn, table: Arc<SymbolTable>) -> Self {
        Self { diag, table }
    }
}

impl Visitor for CasingWarnings {
    fn visit_field_def(&mut self, f: &FieldDef) -> VisitorCF {
        let symbol = self.table.get_symbol(f.ident).unwrap();
        if !symbol.name.is_case(Case::Snake) {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Warning, CasingWarning::SnakeCase).with_span(|_| f.span),
            );
        }
        VisitorCF::Continue
    }

    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        let symbol = self.table.get_symbol(f.ident).unwrap();
        if !symbol.name.is_case(Case::Snake) {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Warning, CasingWarning::SnakeCase).with_span(|_| f.span),
            );
        }
        walk_fn(self, f)
    }

    fn visit_struct_def(&mut self, s: &StructDef) -> VisitorCF {
        let symbol = self.table.get_symbol(s.ident).unwrap();
        if !symbol.name.is_case(Case::Pascal)
            && !s
                .attributes
                .has_attribute(&Attribute::Marker(AttributeMarker::AllowCasing))
        {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Warning, CasingWarning::PascalCase)
                    .with_span(|_| s.span),
            );
        }
        walk_struct_def(self, s)
    }

    fn visit_enum_def(&mut self, e: &EnumDef) -> VisitorCF {
        let symbol = self.table.get_symbol(e.ident).unwrap();
        if !symbol.name.is_case(Case::Pascal)
            && !e
                .attributes
                .has_attribute(&Attribute::Marker(AttributeMarker::AllowCasing))
        {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Warning, CasingWarning::PascalCase)
                    .with_span(|_| e.span),
            );
        }
        walk_enum_def(self, e)
    }

    fn visit_variant_def(&mut self, v: &VariantDef) -> VisitorCF {
        let symbol = self.table.get_symbol(v.ident).unwrap();
        if !symbol.name.is_case(Case::Pascal) {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Warning, CasingWarning::PascalCase)
                    .with_span(|_| v.span),
            );
        }
        walk_variant_def(self, v)
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
