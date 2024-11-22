use ecsl_ast::{
    parse::{FnDef, ParamKind},
    ty::Generics,
    visit::{walk_fn, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};

#[derive(Debug)]
pub enum FnValidationError {
    IncorrectSelfPosition,
    SelfUseInFreeFunction,
}

impl std::fmt::Display for FnValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FnValidationError::IncorrectSelfPosition => "Usage of self in incorrect position",
            FnValidationError::SelfUseInFreeFunction => "Usage of self in free function",
        };

        write!(f, "{}", s)
    }
}

pub struct FnValidator {
    pub errors: Vec<EcslError>,
}

impl FnValidator {
    pub fn new() -> Self {
        FnValidator { errors: Vec::new() }
    }
}

impl Visitor for FnValidator {
    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
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
        VisitorCF::Break
    }
}
