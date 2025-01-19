use ecsl_ast::{
    parse::Attributes,
    visit::{Visitor, VisitorCF},
};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::SourceFileID;

pub struct AttributeValidator<'a> {
    ctxt: &'a Context,
    diag: DiagConn,
    id: SourceFileID,
}

impl<'a> AttributeValidator<'a> {
    pub fn new(ctxt: &'a Context, diag: DiagConn, id: SourceFileID) -> AttributeValidator<'a> {
        Self { ctxt, diag, id }
    }
}

impl<'a> Visitor for AttributeValidator<'a> {
    fn visit_attributes(&mut self, a: &Attributes) -> VisitorCF {
        for attr in a.attributes() {
            if attr.is_unknown() {
                self.diag.push_error(
                    EcslError::new(ErrorLevel::Error, AttributeError::UnknownAttribute)
                        .with_span(|_| attr.span),
                );
                continue;
            }

            if attr.is_std_only() && !self.ctxt.in_std(self.id) {
                self.diag.push_error(
                    EcslError::new(ErrorLevel::Error, AttributeError::StdOnlyAttribute)
                        .with_span(|_| attr.span),
                );
                continue;
            }
        }
        VisitorCF::Continue
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AttributeError {
    UnknownAttribute,
    StdOnlyAttribute,
}

impl std::fmt::Display for AttributeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            AttributeError::UnknownAttribute => "Attribute could not be identified",
            AttributeError::StdOnlyAttribute => "Attribute can only be use in the standard library",
        };
        write!(f, "{}", s)
    }
}
