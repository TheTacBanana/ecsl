use ecsl_ast::{
    expr::{Expr, ExprKind},
    parse::{FnDef, FnHeader, FnKind, ParamKind},
    ty::{Generics, Ty, TyKind},
    visit::{walk_expr, walk_fn, walk_ty, FnCtxt, Visitor, VisitorCF},
    SymbolId,
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};

#[derive(Debug)]
pub enum FnValidationError {
    IncorrectSelfPosition,
    SelfUseInFreeFunction,

    EntityUsedInPlainFn,
    QueryUsedInPlainFn,
    ResourceUsedInPlainFn,
    SystemUsedInPlainFn,
    ScheduleUsedInPlainFn,
}

impl std::fmt::Display for FnValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FnValidationError::IncorrectSelfPosition => "Usage of self in incorrect position",
            FnValidationError::SelfUseInFreeFunction => "Usage of self in free function",
            FnValidationError::EntityUsedInPlainFn => "Usage of Entity in 'fn'",
            FnValidationError::QueryUsedInPlainFn => "Usage of Query in 'fn'",
            FnValidationError::ResourceUsedInPlainFn => "Usage of Resource in 'fn'",
            FnValidationError::ScheduleUsedInPlainFn => "Usage of Schedule in 'fn'",
            FnValidationError::SystemUsedInPlainFn => "Usage of System in 'fn'",
        };

        write!(f, "{}", s)
    }
}

pub struct FnValidator {
    pub errors: Vec<EcslError>,
    pub fn_headers: Vec<FnHeader>,
    pub in_expr: Option<bool>,
}

impl FnValidator {
    pub fn new() -> Self {
        FnValidator {
            errors: Vec::new(),
            fn_headers: Vec::new(),
            in_expr: None,
        }
    }
}

impl Visitor for FnValidator {
    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
        self.fn_headers.push(f.to_header());
        match ctxt {
            FnCtxt::Free => {
                for p in &f.params {
                    let err = match &p.kind {
                        ParamKind::SelfValue(_) | ParamKind::SelfReference(_) => Some(
                            EcslError::new(
                                ErrorLevel::Error,
                                FnValidationError::SelfUseInFreeFunction,
                            )
                            .with_span(|_| p.span)
                            .with_note(|_| "Move function to impl block".to_string()),
                        ),
                        _ => None,
                    };
                    if let Some(err) = err {
                        self.errors.push(err);
                    }
                }
            }
            FnCtxt::Impl => {
                let mut first = true;
                for p in &f.params {
                    let err = match (first, &p.kind) {
                        (false, ParamKind::SelfValue(_) | ParamKind::SelfReference(_)) => Some(
                            EcslError::new(
                                ErrorLevel::Error,
                                FnValidationError::IncorrectSelfPosition,
                            )
                            .with_span(|_| p.span)
                            .with_note(|_| "Self can only be first parameter".to_string()),
                        ),
                        _ => None,
                    };
                    if let Some(err) = err {
                        self.errors.push(err);
                    }
                    first = false;
                }
            }
        }
        let ret = walk_fn(self, f, ctxt);
        ret
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        let fn_header = self.fn_headers.last().unwrap();
        let err = match (&e.kind, fn_header.kind) {
            (ExprKind::Entity, FnKind::Fn) => Some(FnValidationError::EntityUsedInPlainFn),
            (ExprKind::Resource, FnKind::Fn) => Some(FnValidationError::ResourceUsedInPlainFn),
            (ExprKind::Query(_), FnKind::Fn) => Some(FnValidationError::QueryUsedInPlainFn),
            (ExprKind::Schedule(_), FnKind::Fn) => Some(FnValidationError::ScheduleUsedInPlainFn),
            _ => None,
        };
        if let Some(err) = err {
            self.errors.push(
                EcslError::new(ErrorLevel::Error, err)
                    .with_span(|_| e.span)
                    .with_note(|_| "Try convert 'fn' to 'sys'".to_string()),
            );
        }
        walk_expr(self, e)
    }

    fn visit_ty(&mut self, t: &Ty) -> VisitorCF {
        let fn_header = self.fn_headers.last().unwrap();
        let err = match (&t.kind, fn_header.kind) {
            (TyKind::Entity(_), FnKind::Fn) => Some(FnValidationError::EntityUsedInPlainFn),
            (TyKind::Query, FnKind::Fn) => Some(FnValidationError::QueryUsedInPlainFn),
            (TyKind::System, FnKind::Fn) => Some(FnValidationError::SystemUsedInPlainFn),
            (TyKind::Schedule, FnKind::Fn) => Some(FnValidationError::ScheduleUsedInPlainFn),

            _ => None,
        };
        if let Some(err) = err {
            self.errors.push(
                EcslError::new(ErrorLevel::Error, err)
                    .with_span(|_| t.span)
                    .with_note(|_| "Try convert 'fn' to 'sys'".to_string()),
            );
        }

        walk_ty(self, t)
    }
}
