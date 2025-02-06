use std::sync::Arc;

use ecsl_ast::{
    parse::FnKind,
    visit::{FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{GlobalID, TyID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use log::debug;

pub struct EntryPoint {
    pub ty_ctxt: Arc<LocalTyCtxt>,
    pub entry_points: Vec<(TyID, EntryPointKind)>,
}

impl EntryPoint {
    pub fn new(ty_ctxt: Arc<LocalTyCtxt>) -> Self {
        Self {
            ty_ctxt,
            entry_points: Default::default(),
        }
    }
}

impl Visitor for EntryPoint {
    fn visit_fn(&mut self, f: &ecsl_ast::parse::FnDef, ctxt: FnCtxt) -> VisitorCF {
        if FnCtxt::Impl == ctxt {
            return VisitorCF::Continue;
        }

        let symbol = self.ty_ctxt.table.get_symbol(f.ident).unwrap();
        let kind = match (symbol.name.as_str(), f.kind) {
            ("main", FnKind::Fn) => Some(EntryPointKind::MainFn),
            ("main", FnKind::Sys) => Some(EntryPointKind::MainSys),
            (_, _) => None,
        };
        let Some(entry_point) = kind else {
            return VisitorCF::Continue;
        };

        let tyid = self
            .ty_ctxt
            .global
            .get_or_create_tyid(GlobalID::new(f.ident, self.ty_ctxt.file));
        let tyir = self.ty_ctxt.global.get_tyir(tyid);

        let int_tyid = self.ty_ctxt.global.tyid_from_tyir(TyIr::Int);

        match (entry_point, tyir) {
            (EntryPointKind::MainFn, TyIr::Fn(ecsl_ty::FnDef { params, ret, .. })) => {
                if params.len() != 0 {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, EntryPointError::CannotHaveArgs)
                            .with_span(|_| f.span),
                    );
                }

                if !(ret == TyID::BOTTOM || ret == int_tyid) {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, EntryPointError::WrongReturnType)
                            .with_span(|_| f.span),
                    );
                }

                self.entry_points.push((tyid, entry_point));
            }
            _ => panic!(),
        }

        VisitorCF::Continue
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPointKind {
    MainFn,
    MainSys,
}

#[derive(Debug)]
pub enum EntryPointError {
    CannotHaveArgs,
    WrongReturnType,
    MultipleEntryPoints,
    NoEntryPoints,
}

impl std::fmt::Display for EntryPointError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            EntryPointError::CannotHaveArgs => "Entry point cannot have args",
            EntryPointError::WrongReturnType => "Entry point can only return void",
            EntryPointError::MultipleEntryPoints => "Multiple entry points",
            EntryPointError::NoEntryPoints => "No entry point",
        };
        write!(f, "{}", s)
    }
}
