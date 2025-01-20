use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use ecsl_ast::{
    expr::{BinOpKind, Expr, ExprKind, Literal},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, walk_expr, walk_fn, walk_stmt, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::SymbolID;
use ecsl_ty::{local::LocalTyCtxt, ty::TyIr};
use log::{debug, error};
use lrpar::Span;

pub struct TyCheck {
    ty_ctxt: Arc<LocalTyCtxt>,

    // all_defined: BTreeSet<SymbolID>,
    stack: Vec<Scope>,
    ty_stack: Vec<TyIr>,
}

#[derive(Debug)]
pub struct Scope {
    defined: BTreeMap<SymbolID, TyIr>,
}

impl TyCheck {
    pub fn new(ty_ctxt: Arc<LocalTyCtxt>) -> Self {
        Self {
            ty_ctxt,
            stack: Vec::new(),
            ty_stack: Vec::new(),
        }
    }

    pub fn new_scope(&mut self) {
        debug!("New Scope");
        self.stack.push(Scope::new());
    }

    pub fn define_symbol(&mut self, id: SymbolID, span: Span, ty: TyIr) {
        if self.get_symbol(id).is_some() {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                    .with_span(|_| span),
            );
        }

        debug!("Define symbol {}", id);
        self.stack.last_mut().unwrap().define_symbol(id, ty);
    }

    pub fn use_symbol(&mut self, id: SymbolID, span: Span) -> Option<&TyIr> {
        if let Some(symbol) = self.get_symbol(id) {
            Some(symbol)
        } else {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::SymbolDoesntExist)
                    .with_span(|_| span),
            );
            None
        }
    }

    pub fn get_symbol(&self, id: SymbolID) -> Option<&TyIr> {
        let mut found = None;
        for scope in self.stack.iter().rev() {
            let ty = scope.get_symbol(id);
            if ty.is_some() {
                found = ty;
                break;
            }
        }
        return found;
    }

    pub fn pop_scope(&mut self) {
        let scope = self.stack.pop().unwrap();
        debug!("Popped scope {:?}", scope);
    }

    pub fn push_ty(&mut self, ty: TyIr) {
        self.ty_stack.push(ty);
    }

    pub fn pop_ty(&mut self) -> TyIr {
        self.ty_stack.pop().unwrap()
    }

    pub fn pop_double_ty(&mut self) -> (TyIr, TyIr) {
        let r = self.pop_ty();
        let l = self.pop_ty();
        (l, r)
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            defined: Default::default(),
        }
    }

    pub fn define_symbol(&mut self, id: SymbolID, ty: TyIr) {
        self.defined.insert(id, ty);
    }

    pub fn get_symbol(&self, id: SymbolID) -> Option<&TyIr> {
        self.defined.get(&id)
    }
}

impl Visitor for TyCheck {
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        self.new_scope();
        walk_fn(self, f);
        self.pop_scope();
        VisitorCF::Continue
    }

    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        self.new_scope();
        walk_block(self, b);
        self.pop_scope();
        VisitorCF::Continue
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        let size = self.ty_stack.len();
        if walk_stmt(self, s) == VisitorCF::Break {
            let _ = self.ty_stack.split_off(size);
            debug!("Restore stack to {:?}", self.ty_stack);
            return VisitorCF::Break;
        }

        match &s.kind {
            StmtKind::Let(_, symbol_id, span, _, _) => {
                let ty = self.pop_ty();
                //TODO: Match asscribed type with actual type
                self.define_symbol(*symbol_id, *span, ty);
                VisitorCF::Continue
            }
            StmtKind::If(e, _, _) | StmtKind::ElseIf(e, _, _) => {
                if self.pop_ty() != TyIr::Bool {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::ExpectedBoolean)
                            .with_span(|_| e.span),
                    );
                }
                VisitorCF::Continue
            }
            StmtKind::Else(_) => VisitorCF::Continue,
            StmtKind::Expr(_) => VisitorCF::Continue,
            e => {
                error!("Unimplemented {:?}", e);
                VisitorCF::Continue
            }
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        let size = self.ty_stack.len();
        debug!("Before {:?}", self.ty_stack);
        if walk_expr(self, e) == VisitorCF::Break {
            let _ = self.ty_stack.split_off(size);
            debug!("Restore stack to {:?}", self.ty_stack);
            return VisitorCF::Break;
        }
        debug!("After {:?}", self.ty_stack);

        match &e.kind {
            ExprKind::Assign(symbol_id, span, _) => {
                let expr_ty = self.pop_ty();
                if let Some(ident_ty) = self.use_symbol(*symbol_id, *span) {
                    if *ident_ty != expr_ty {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::AssignWrongType)
                                .with_span(|_| *span),
                        );
                    }
                    VisitorCF::Continue
                } else {
                    VisitorCF::Break
                }
            }
            ExprKind::Ident(symbol_id) => {
                if let Some(ty) = self.use_symbol(*symbol_id, e.span) {
                    let ty = ty.clone();
                    self.push_ty(ty);
                    VisitorCF::Continue
                } else {
                    self.push_ty(TyIr::Unknown);
                    return VisitorCF::Break;
                }
            }
            ExprKind::Lit(literal) => {
                let ty = match literal {
                    Literal::Int => TyIr::Int,
                    Literal::Bool => TyIr::Bool,
                    e => {
                        error!("Unimplemented {:?}", e);
                        return VisitorCF::Break;
                    }
                };
                self.push_ty(ty);
                VisitorCF::Continue
            }
            ExprKind::BinOp(op, _, _) => {
                let (l, r) = self.pop_double_ty();

                if l != r {
                    error!("Invalid bin op");
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::InvalidBinOp)
                            .with_span(|_| e.span),
                    );

                    self.push_ty(TyIr::Unknown);
                    return VisitorCF::Break;
                }

                match op {
                    BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
                        self.push_ty(l)
                    }
                    BinOpKind::And
                    | BinOpKind::Or
                    | BinOpKind::Eq
                    | BinOpKind::Neq
                    | BinOpKind::Lt
                    | BinOpKind::Leq
                    | BinOpKind::Gt
                    | BinOpKind::Geq => {
                        self.push_ty(TyIr::Bool);
                    }
                }

                return VisitorCF::Continue;
            }

            e => {
                error!("Unimplemented {:?}", e);
                VisitorCF::Continue
            } // ExprKind::Ref(mutable, expr) => todo!(),
              // ExprKind::UnOp(un_op_kind, expr) => todo!(),
              // ExprKind::Array(vec) => todo!(),
              // ExprKind::MethodSelf => todo!(),
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
        }
    }

    // fn visit_
}

#[derive(Debug, Clone, Copy)]
pub enum TyCheckError {
    SymbolDoesntExist,
    SymbolRedefined,
    ExpectedBoolean,
    InvalidBinOp,
    AssignWrongType,
}

impl std::fmt::Display for TyCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TyCheckError::SymbolDoesntExist => "Symbol could not be found in the current scope",
            TyCheckError::SymbolRedefined => "Symbol is already defined",
            TyCheckError::ExpectedBoolean => "Expected boolean expression",
            TyCheckError::InvalidBinOp => "Operation cannot be performed",
            TyCheckError::AssignWrongType => "Cannot assign incorrect type",
        };
        write!(f, "{}", s)
    }
}
