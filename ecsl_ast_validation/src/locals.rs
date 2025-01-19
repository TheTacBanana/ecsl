use std::collections::BTreeSet;

use ecsl_ast::{
    expr::{Expr, ExprKind},
    visit::{walk_expr, Visitor, VisitorCF},
};
use ecsl_error::{EcslError, ErrorLevel};
use ecsl_index::SymbolID;

pub struct LocalValidator {
    pub errors: Vec<EcslError>,

    pub non_local_symbols: BTreeSet<SymbolID>,

    pub stack: Vec<Vec<SymbolID>>,
    pub modifiers: Vec<ScopeModifier>,
}

pub struct Scope {}

pub enum ScopeModifier {
    Define(SymbolID),
    Use(SymbolID),
    Remove(SymbolID),
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolError {
    UseAfterFree,
}

impl std::fmt::Display for SymbolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            SymbolError::UseAfterFree => "",
        };
        write!(f, "{}", s)
    }
}

impl LocalValidator {
    pub fn define_symbol(&mut self, symbol: SymbolID) {
        self.stack.last_mut().unwrap().push(symbol);
        self.modifiers.push(ScopeModifier::Define(symbol));
    }

    pub fn use_symbol(&mut self, symbol: SymbolID) {
        self.stack.last_mut().unwrap().push(symbol);
        for m in self.modifiers.iter().rev() {
            match m {
                ScopeModifier::Define(s) if *s == symbol => {
                    break;
                }
                ScopeModifier::Remove(s) if *s == symbol => {
                    self.errors
                        .push(EcslError::new(ErrorLevel::Error, SymbolError::UseAfterFree));
                }
                _ => (),
            }
        }

        self.modifiers.push(ScopeModifier::Use(symbol));
    }
}

impl Visitor for LocalValidator {
    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        match &e.kind {
            ExprKind::Assign(symbol, expr) => {
                walk_expr(self, expr);
                self.define_symbol(*symbol);
            }

            ExprKind::Ident(symbol) => {
                self.use_symbol(*symbol);
            }

            ExprKind::BinOp(_, _, _) | ExprKind::UnOp(_, _) => {
                walk_expr(self, e);
            }

            // ExprKind::Ref(mutable, expr) => todo!(),
            // ExprKind::Array(vec) => todo!(),
            // ExprKind::MethodSelf => todo!(),
            // ExprKind::Lit(literal) => todo!(),
            // ExprKind::Struct(ty, vec) => todo!(),
            // ExprKind::Enum(ty, symbol_id, vec) => todo!(),
            // ExprKind::Range(expr, expr1, range_type) => todo!(),
            // ExprKind::Cast(expr, ty) => todo!(),
            // ExprKind::Field(expr, symbol_id) => todo!(),
            // ExprKind::Function(expr, concrete_generics, symbol_id, vec) => todo!(),
            // ExprKind::Entity => todo!(),
            // ExprKind::Resource => todo!(),
            // ExprKind::Query(query_expr) => todo!(),
            // ExprKind::Schedule(schedule) => todo!(),
            _ => todo!(),
        }
        VisitorCF::Continue
    }
}
