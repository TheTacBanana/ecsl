use ecsl_ast::parse::{ParamKind, RetTy};
use ecsl_ast::ty::Ty;
use ecsl_ast::SourceAST;
use ecsl_ast::{
    expr::{BinOpKind, Expr, ExprKind, Literal},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, walk_expr, walk_fn, walk_stmt, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{SymbolID, TyID};
use ecsl_ty::local::LocalTyCtxt;
use ecsl_ty::{GenericsScope, TyIr};
use log::{debug, error, trace};
use lrpar::Span;
use std::collections::BTreeMap;
use std::sync::Arc;

pub fn ty_check(ast: &SourceAST, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut ty_check = TyCheck::new(ty_ctxt);
    ty_check.visit_ast(ast);
}

pub struct TyCheck {
    ty_ctxt: Arc<LocalTyCtxt>,

    // all_defined: BTreeSet<SymbolID>,
    stack: Vec<Scope>,
    ty_stack: Vec<TyID>,
    generic_scope: GenericsScope,
}

#[derive(Debug)]
pub struct Scope {
    defined: BTreeMap<SymbolID, TyID>,
}

impl TyCheck {
    pub fn new(ty_ctxt: Arc<LocalTyCtxt>) -> Self {
        Self {
            ty_ctxt,
            stack: Vec::new(),
            ty_stack: Vec::new(),
            generic_scope: GenericsScope::new(),
        }
    }

    pub fn new_scope(&mut self) {
        debug!("New Scope");
        self.stack.push(Scope::new());
    }

    pub fn define_symbol(&mut self, id: SymbolID, span: Span, ty: TyID) {
        if self.get_symbol(id).is_some() {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                    .with_span(|_| span),
            );
        }

        debug!("Define symbol {} as {}", id, ty);
        self.stack.last_mut().unwrap().define_symbol(id, ty);
    }

    pub fn use_symbol(&mut self, id: SymbolID, span: Span) -> Option<&TyID> {
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

    pub fn get_symbol(&self, id: SymbolID) -> Option<&TyID> {
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

    pub fn get_tyid_from_ty(&self, ty: &Ty) -> TyID {
        self.ty_ctxt.get_tyid(ty, &self.generic_scope)
    }

    pub fn get_tyir_from_ty(&self, ty: &Ty) -> TyIr {
        self.ty_ctxt
            .global
            .get_tyir(self.ty_ctxt.get_tyid(ty, &self.generic_scope))
    }

    pub fn pop_scope(&mut self) {
        let scope = self.stack.pop().unwrap();
        debug!("Popped scope {:?}", scope);
    }

    pub fn push_id(&mut self, ty: TyID) {
        debug!("Pushed {:?}", ty);
        self.ty_stack.push(ty);
    }

    pub fn push_tyir(&mut self, tyir: TyIr) {
        let ty = self.ty_ctxt.global.tyid_from_tyir(tyir);
        debug!("Pushed {:?}", ty);
        self.ty_stack.push(ty);
    }

    pub fn pop_ty(&mut self) -> TyID {
        self.ty_stack.pop().unwrap()
    }

    pub fn pop_double_ty(&mut self) -> (TyID, TyID) {
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

    pub fn define_symbol(&mut self, id: SymbolID, ty: TyID) {
        self.defined.insert(id, ty);
    }

    pub fn get_symbol(&self, id: SymbolID) -> Option<&TyID> {
        self.defined.get(&id)
    }
}

impl Visitor for TyCheck {
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        debug!("Checking {:?}", f.to_header());
        self.new_scope();

        let gid = self.ty_ctxt.get_global_id(f.ident).unwrap();
        debug!("Fn GID {:?}", gid);
        let TyIr::Fn(fn_tyir) = self
            .ty_ctxt
            .global
            .get_tyir(self.ty_ctxt.global.get_or_create_tyid(gid))
        else {
            panic!("Fn {:?} has not been defined", gid);
        };
        debug!("{:?}", fn_tyir);
        debug!("Int is {:?}", self.ty_ctxt.global.tyid_from_tyir(TyIr::Int));

        match &f.ret {
            RetTy::None(_) => self.push_tyir(TyIr::Bottom),
            RetTy::Ty(ty) => self.push_id(self.get_tyid_from_ty(ty)),
        }

        for (i, param) in f.params.iter().enumerate() {
            match &param.kind {
                ParamKind::SelfValue(_) => todo!(),
                ParamKind::SelfReference(_) => todo!(),
                ParamKind::Normal(_, symbol_id, _) => {
                    self.define_symbol(*symbol_id, param.span, fn_tyir.params[i]);
                }
            }
        }

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
        if let StmtKind::For(id, ty, _, _) = &s.kind {
            self.define_symbol(*id, s.span, self.get_tyid_from_ty(&ty));
        }

        let size = self.ty_stack.len();
        if walk_stmt(self, s) == VisitorCF::Break {
            let _ = self.ty_stack.split_off(size);
            debug!("Restore stack to {:?}", self.ty_stack);
            return VisitorCF::Break;
        }

        match &s.kind {
            StmtKind::Let(_, symbol_id, span, asscribed_ty, _) => {
                let ty = self.pop_ty();
                let asscribed_ty = self.ty_ctxt.get_tyid(asscribed_ty, &self.generic_scope);
                if ty != asscribed_ty {}

                self.define_symbol(*symbol_id, *span, ty);
                VisitorCF::Continue
            }
            StmtKind::If(e, _, _) | StmtKind::ElseIf(e, _, _) => {
                if self.pop_ty() != self.ty_ctxt.global.tyid_from_tyir(TyIr::Bool) {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::ExpectedBoolean)
                            .with_span(|_| e.span),
                    );
                }
                VisitorCF::Continue
            }
            StmtKind::Else(_) => VisitorCF::Continue,
            StmtKind::Return(None) => {
                // Pop the functions return type
                if self.pop_ty() != self.ty_ctxt.global.tyid_from_tyir(TyIr::Bottom) {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
                            .with_span(|_| s.span),
                    );
                }
                VisitorCF::Continue
            }
            StmtKind::Return(Some(_)) => {
                // Pop Function Return Type and the Expressions Type
                let (l, r) = self.pop_double_ty();
                if l != r {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
                            .with_span(|_| s.span),
                    );
                }
                return VisitorCF::Continue;
            }
            StmtKind::Expr(_) => VisitorCF::Continue,
            StmtKind::Semi => VisitorCF::Continue,
            StmtKind::For(_, ty, expr, _) => {
                match &expr.kind {
                    ExprKind::Range(_, _, _) => (),
                    ExprKind::Query(_) => todo!(), //TODO:
                    _ => panic!("Non range or query found in for loop"),
                }

                let ty = self.get_tyid_from_ty(&ty);
                let over = self.pop_ty();
                if ty != over {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
                            .with_span(|_| s.span),
                    );
                }

                VisitorCF::Continue
            }

            StmtKind::While(_, _) => todo!(),
            StmtKind::Match(_, _) => todo!(),
            StmtKind::Break => todo!(),
            StmtKind::Continue => todo!(),
            // e => {
            //     error!("Unimplemented {:?}", e);
            //     VisitorCF::Continue
            // }
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        let size = self.ty_stack.len();
        if walk_expr(self, e) == VisitorCF::Break {
            let _ = self.ty_stack.split_off(size);
            debug!("Restore stack to {:?}", self.ty_stack);
            return VisitorCF::Break;
        }

        match &e.kind {
            ExprKind::Assign(symbol_id, span, _) => {
                let expr_ty = self.pop_ty();
                if let Some(ident_ty) = self.use_symbol(*symbol_id, *span) {
                    if *ident_ty != expr_ty {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::AssignWrongType)
                                .with_span(|_| *span),
                        );
                        return VisitorCF::Break;
                    }
                    VisitorCF::Continue
                } else {
                    VisitorCF::Break
                }
            }
            ExprKind::Ident(symbol_id) => {
                if let Some(ty) = self.use_symbol(*symbol_id, e.span) {
                    let ty = ty.clone();
                    self.push_id(ty);
                    VisitorCF::Continue
                } else {
                    self.push_tyir(TyIr::Unknown);
                    return VisitorCF::Break;
                }
            }
            ExprKind::Function(None, _, id, args) => {
                let arg_tys = (0..args.len())
                    .map(|_| self.pop_ty())
                    .rev()
                    .collect::<Vec<_>>();

                let Some(gid) = self.ty_ctxt.get_global_id(*id) else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionDoesntExist)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };

                let TyIr::Fn(fn_tyir) = self
                    .ty_ctxt
                    .global
                    .get_tyir(self.ty_ctxt.global.get_or_create_tyid(gid))
                else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionDoesntExist)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };

                // Compare all parameters
                for (i, (l, r)) in fn_tyir.params.iter().zip(arg_tys).enumerate() {
                    trace!("{:?}, {:?}", l, r);
                    if *l != r {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(
                                ErrorLevel::Error,
                                TyCheckError::IncorrectFunctionArgument,
                            )
                            .with_span(|_| args[i].span),
                        );
                    }
                }

                VisitorCF::Continue
            }
            ExprKind::Function(Some(_), _, _, _) => {
                error!("Assoc functions not implemented");
                VisitorCF::Break
            }
            ExprKind::Lit(literal) => {
                let tyir = match literal {
                    Literal::Int => TyIr::Int,
                    Literal::Bool => TyIr::Bool,
                    Literal::Float => TyIr::Float,
                    Literal::Char => TyIr::Char,

                    Literal::String => todo!(), //TODO:
                };
                self.push_tyir(tyir);
                VisitorCF::Continue
            }
            ExprKind::BinOp(op, _, _) => {
                let (l, r) = self.pop_double_ty();
                if l != r {
                    error!("Invalid bin op {:?}", op);
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::InvalidBinOp)
                            .with_span(|_| e.span),
                    );

                    self.push_id(self.ty_ctxt.global.unknown_ty());
                    return VisitorCF::Break;
                }

                match op {
                    BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
                        self.push_id(l)
                    }
                    BinOpKind::And
                    | BinOpKind::Or
                    | BinOpKind::Eq
                    | BinOpKind::Neq
                    | BinOpKind::Lt
                    | BinOpKind::Leq
                    | BinOpKind::Gt
                    | BinOpKind::Geq => {
                        self.push_tyir(TyIr::Bool);
                    }
                }

                return VisitorCF::Continue;
            }
            ExprKind::Range(_, _, _) => {
                let (l, r) = self.pop_double_ty();
                if l != r {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::RangeMustEqual)
                            .with_span(|_| e.span),
                    );
                    self.push_tyir(TyIr::Unknown);
                    return VisitorCF::Break;
                }
                self.push_id(l);
                VisitorCF::Continue
            }

            e => {
                error!("Unimplemented {:?}", e);
                VisitorCF::Break
            } // ExprKind::Ref(mutable, expr) => todo!(),
              // ExprKind::UnOp(un_op_kind, expr) => todo!(),
              // ExprKind::Array(vec) => todo!(),
              // ExprKind::MethodSelf => todo!(),
              // ExprKind::Struct(ty, vec) => todo!(),
              // ExprKind::Enum(ty, symbol_id, vec) => todo!(),
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
    FunctionDoesntExist,
    IncorrectFunctionArgument,
    SymbolIsNotFunction,
    SymbolRedefined,
    ExpectedBoolean,
    InvalidBinOp,
    AssignWrongType,
    FunctionReturnType,
    ForLoopIterator,
    RangeMustEqual,
}

impl std::fmt::Display for TyCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TyCheckError::SymbolDoesntExist => "Symbol could not be found in the current scope",
            TyCheckError::SymbolRedefined => "Symbol is already defined",
            TyCheckError::ExpectedBoolean => "Expected boolean expression",
            TyCheckError::InvalidBinOp => "Operation cannot be performed",
            TyCheckError::AssignWrongType => "Cannot assign incorrect type",
            TyCheckError::FunctionReturnType => "Return type for function does not match",
            TyCheckError::ForLoopIterator => "Iterator has mismatched type",
            TyCheckError::RangeMustEqual => "lhs and rhs of range expression must be the same",
            TyCheckError::FunctionDoesntExist => "Could not find defined or imported ",
            TyCheckError::SymbolIsNotFunction => "Symbol is not a function",
            TyCheckError::IncorrectFunctionArgument => {
                "Type of argument does not match Type of signature"
            }
        };
        write!(f, "{}", s)
    }
}
