use ecsl_ast::{
    expr::{Expr, ExprKind},
    item::{Item, ItemKind},
    parse::{FnDef, FnHeader, FnKind, ParamKind},
    stmt::{Block, Stmt, StmtKind},
    ty::{Ty, TyKind},
    visit::*,
};
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};

#[derive(Debug)]
pub enum FnValidationError {
    IncorrectSelfPosition,
    SelfUseInFreeFunction,
    NoSelfInAssocFunction,
    IllegalAssignmentPosition,

    EntityUsedInPlainFn,
    QueryUsedInPlainFn,
    ResourceUsedInPlainFn,
    SystemUsedInPlainFn,
    ScheduleUsedInPlainFn,
}

impl std::fmt::Display for FnValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FnValidationError::IncorrectSelfPosition => "Usage of 'self' in incorrect position",
            FnValidationError::SelfUseInFreeFunction => "Usage of 'self' in free function",
            FnValidationError::NoSelfInAssocFunction => {
                "Assoc function does not use self in parameters"
            }
            FnValidationError::IllegalAssignmentPosition => {
                "Usage of assignment in illegal position"
            }

            FnValidationError::EntityUsedInPlainFn => "Usage of Entity in 'fn'",
            FnValidationError::QueryUsedInPlainFn => "Usage of Query in 'fn'",
            FnValidationError::ResourceUsedInPlainFn => "Usage of Resource in 'fn'",
            FnValidationError::ScheduleUsedInPlainFn => "Usage of Schedule in 'fn'",
            FnValidationError::SystemUsedInPlainFn => "Usage of System in 'fn'",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentState {
    Allowed,
    Illegal,
}

pub struct FnValidator {
    pub diag: DiagConn,
    pub fn_headers: Vec<FnHeader>,
    pub assignment_state: AssignmentState,
}

impl FnValidator {
    pub fn new(diag: DiagConn) -> Self {
        FnValidator {
            diag,
            fn_headers: Vec::new(),
            assignment_state: AssignmentState::Allowed,
        }
    }
}

impl Visitor for FnValidator {
    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
        self.fn_headers.push(f.to_header());
        match ctxt {
            FnCtxt::Free => {
                for p in &f.params {
                    match &p.kind {
                        ParamKind::SelfValue(_) | ParamKind::SelfReference(_) => Some(
                            self.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    FnValidationError::SelfUseInFreeFunction,
                                )
                                .with_span(|_| f.span)
                                .with_note(|_| "Move function to impl block".to_string()),
                            ),
                        ),
                        _ => None,
                    };
                }
            }
            FnCtxt::Impl => {
                let mut first = true;
                let mut found = false;
                for p in &f.params {
                    match (first, &p.kind) {
                        (false, ParamKind::SelfValue(_) | ParamKind::SelfReference(_)) => {
                            found = true;
                            Some(
                                self.diag.push_error(
                                    EcslError::new(
                                        ErrorLevel::Error,
                                        FnValidationError::IncorrectSelfPosition,
                                    )
                                    .with_span(|_| f.span)
                                    .with_note(|_| "Self can only be first parameter".to_string()),
                                ),
                            )
                        }
                        (_, ParamKind::SelfValue(_) | ParamKind::SelfReference(_)) => {
                            found = true;
                            None
                        }
                        _ => None,
                    };

                    first = false;
                }

                if !found {
                    self.diag.push_error(
                        EcslError::new(ErrorLevel::Error, FnValidationError::NoSelfInAssocFunction)
                            .with_span(|_| f.span)
                            .with_note(|_| "Move out of impl block".to_string()),
                    );
                }
            }
        }
        walk_fn(self, f)
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        let fn_header = self.fn_headers.last().unwrap();

        // Exclude ECS Features from fn's
        let err = match (&e.kind, fn_header.kind) {
            (ExprKind::Entity, FnKind::Fn) => Some(FnValidationError::EntityUsedInPlainFn),
            (ExprKind::Resource, FnKind::Fn) => Some(FnValidationError::ResourceUsedInPlainFn),
            (ExprKind::Query(_), FnKind::Fn) => Some(FnValidationError::QueryUsedInPlainFn),
            (ExprKind::Schedule(_), FnKind::Fn) => Some(FnValidationError::ScheduleUsedInPlainFn),
            _ => None,
        };
        if let Some(err) = err {
            self.diag.push_error(
                EcslError::new(ErrorLevel::Error, err)
                    .with_span(|_| e.span)
                    .with_note(|_| "Try convert 'fn' to 'sys'".to_string()),
            );
        }

        // Exclude assignment in illegal positions
        let cur_state = self.assignment_state;
        self.assignment_state = match (&e.kind, cur_state) {
            (_, AssignmentState::Allowed) => AssignmentState::Illegal,
            (ExprKind::Assign(_, _, _), AssignmentState::Illegal) => {
                self.diag.push_error(
                    EcslError::new(
                        ErrorLevel::Error,
                        FnValidationError::IllegalAssignmentPosition,
                    )
                    .with_span(|_| e.span),
                );
                cur_state
            }
            (_, AssignmentState::Illegal) => cur_state,
        };

        walk_expr(self, e)
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        self.assignment_state = match s.kind {
            StmtKind::Expr(_) => AssignmentState::Allowed,
            _ => AssignmentState::Illegal,
        };
        walk_stmt(self, s)
    }

    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        let cur_state = self.assignment_state;
        self.assignment_state = AssignmentState::Allowed;
        let ret = walk_block(self, b);
        self.assignment_state = cur_state;
        ret
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
            self.diag.push_error(
                EcslError::new(ErrorLevel::Error, err)
                    .with_span(|_| t.span)
                    .with_note(|_| "Try convert 'fn' to 'sys'".to_string()),
            );
        }
        walk_ty(self, t)
    }

    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Fn(_) | ItemKind::Impl(_) => walk_item(self, i),
            _ => VisitorCF::Continue,
        }
    }
}
