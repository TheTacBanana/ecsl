use ecsl_ast::{
    parse::{Attributes, Opcode},
    stmt::{Stmt, StmtKind},
    visit::{Visitor, VisitorCF},
};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::SourceFileID;

pub struct BytecodeValidator<'a> {
    ctxt: &'a Context,
    diag: DiagConn,
    id: SourceFileID,
}

impl<'a> BytecodeValidator<'a> {
    pub fn new(ctxt: &'a Context, diag: DiagConn, id: SourceFileID) -> BytecodeValidator<'a> {
        Self { ctxt, diag, id }
    }
}

impl<'a> Visitor for BytecodeValidator<'a> {
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        match &s.kind {
            StmtKind::ASM(asm) => {
                if !self.ctxt.in_std(self.id) {
                    self.diag.push_error(
                        EcslError::new(ErrorLevel::Error, BytecodeError::StdOnly)
                            .with_span(|_| s.span),
                    );
                }

                for bytecode in asm {
                    if let Opcode::UNDF = bytecode.ins.op {
                        self.diag.push_error(
                            EcslError::new(ErrorLevel::Error, BytecodeError::Unknown)
                                .with_span(|_| bytecode.span),
                        );
                    }
                }

                VisitorCF::Continue
            }
            _ => VisitorCF::Continue,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BytecodeError {
    StdOnly,
    Unknown,
}

impl std::fmt::Display for BytecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BytecodeError::StdOnly => "Inline bytecode only allowed in standard library",
            BytecodeError::Unknown => "Unknown opcode",
        };
        write!(f, "{}", s)
    }
}
