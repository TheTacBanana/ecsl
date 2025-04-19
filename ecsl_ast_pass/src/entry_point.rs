use ecsl_assembler::header::EntryPointKind;
use ecsl_ast::{
    parse::FnKind,
    visit::{FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{GlobalID, TyID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use std::sync::Arc;

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
            ("main", FnKind::Sys) => Some(EntryPointKind::MainSysOnce), // TODO: Consider what the default should be?
            ("main_once", FnKind::Sys) => Some(EntryPointKind::MainSysOnce),
            ("main_loop", FnKind::Sys) => Some(EntryPointKind::MainSysLoop),
            (_, _) => None,
        };
        let Some(entry_point) = kind else {
            return VisitorCF::Continue;
        };

        let tyid =
            self.ty_ctxt
                .global
                .get_or_create_tyid(GlobalID::new(None, f.ident, self.ty_ctxt.file));
        let tyir = self.ty_ctxt.global.get_tyir(tyid).into_fn().unwrap();

        let schedule_tyid = self.ty_ctxt.global.tyid_from_tyir(TyIr::Schedule);

        macro_rules! err_if {
            ($c:expr, $e:expr) => {
                if $c {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| f.span));
                }
            };
        }

        err_if!(tyir.params.len() != 0, EntryPointError::CannotHaveArgs);
        match (entry_point, tyir) {
            (EntryPointKind::MainFn, ecsl_ty::FnDef { ret, .. }) => {
                err_if!(!(ret.ty == TyID::BOTTOM), EntryPointError::WrongReturnType);

                self.entry_points.push((tyid, entry_point));
            }
            (EntryPointKind::MainSysOnce, ecsl_ty::FnDef { ret, .. }) => {
                err_if!(
                    !(ret.ty == TyID::BOTTOM || ret.ty == schedule_tyid),
                    EntryPointError::WrongReturnType
                );

                if ret.ty == schedule_tyid {
                    self.entry_points.push((tyid, EntryPointKind::MainSysOnce));
                } else {
                    self.entry_points
                        .push((tyid, EntryPointKind::MainSysUnscheduled));
                }
            }
            (EntryPointKind::MainSysLoop, ecsl_ty::FnDef { ret, .. }) => {
                err_if!(ret.ty != schedule_tyid, EntryPointError::WrongReturnType);

                todo!();
            }
            (EntryPointKind::MainSysUnscheduled, _) => unreachable!(),
            (EntryPointKind::Unknown, _) => unreachable!(),
        }

        VisitorCF::Continue
    }
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
