use ecsl_ast::expr::UnOpKind;
use ecsl_ast::parse::ParamKind;
use ecsl_ast::ty::Mutable;
use ecsl_ast::SourceAST;
use ecsl_ast::{
    expr::{Expr, ExprKind},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::cons::Constant;
use ecsl_gir::expr::Operand;
use ecsl_gir::term::{SwitchCase, Terminator, TerminatorKind};
use ecsl_gir::{Local, GIR};
use ecsl_index::{BlockID, ConstID, LocalID, SymbolID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use ecsl_ty::local::LocalTyCtxt;
use ecsl_ty::{GenericsScope, TyIr};
use ext::IntoTyID;
use log::debug;
use std::collections::BTreeMap;
use std::sync::Arc;

#[allow(unused)]
mod gir {
    pub use ecsl_gir::expr::*;
    pub use ecsl_gir::stmt::*;
    pub use ecsl_gir::term::*;
    pub use ecsl_gir::*;
}

pub mod ext;

pub fn ty_check(ast: &SourceAST, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut ty_check = TyCheck::new(ty_ctxt);
    ty_check.visit_ast(ast);
}

pub struct TyCheck {
    ty_ctxt: Arc<LocalTyCtxt>,
    generic_scope: GenericsScope,

    cur_gir: Option<TyID>,
    gir: BTreeMap<TyID, GIR>,

    block_stack: Vec<BlockID>,
    symbols: Vec<BTreeMap<SymbolID, LocalID>>,
    stack: Vec<(TyID, Operand)>,
}

impl TyCheck {
    pub fn new(ty_ctxt: Arc<LocalTyCtxt>) -> Self {
        Self {
            ty_ctxt,
            generic_scope: GenericsScope::new(),
            cur_gir: None,
            gir: Default::default(),
            block_stack: Default::default(),
            symbols: Default::default(),
            stack: Default::default(),
        }
    }

    pub fn get_tyir(&self, t: impl IntoTyID) -> TyIr {
        self.ty_ctxt.global.get_tyir(t.into_tyid(&self.ty_ctxt))
    }

    pub fn get_tyid(&self, t: impl IntoTyID) -> TyID {
        t.into_tyid(&self.ty_ctxt)
    }

    pub fn cur_gir(&self) -> &GIR {
        self.gir.get(&self.cur_gir.unwrap()).unwrap()
    }

    pub fn new_block(&mut self) -> BlockID {
        let block = self
            .gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .new_block();
        self.block_stack.push(block);
        self.symbols.push(Default::default());
        block
    }

    pub fn new_block_without_stack(&mut self) -> BlockID {
        self.gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .new_block()
    }

    pub fn push_block_stack(&mut self, block: BlockID) {
        self.block_stack.push(block);
        self.symbols.push(Default::default());
    }

    pub fn pop_block(&mut self) -> BlockID {
        self.symbols.pop();
        self.block_stack.pop().unwrap()
    }

    pub fn new_local(&mut self, local: Local) -> LocalID {
        self.gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .new_local(local)
    }

    pub fn new_constant(&mut self, cons: Constant) -> ConstID {
        self.gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .new_constant(cons)
    }

    pub fn push_stmt_to_cur_block(&mut self, stmt: ecsl_gir::stmt::Stmt) {
        let block_id = self.cur_block();
        self.gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .get_block_mut(block_id)
            .unwrap()
            .push(stmt);
    }

    pub fn cur_block(&self) -> BlockID {
        *self.block_stack.last().unwrap()
    }

    pub fn block_mut(&mut self, block: BlockID) -> &mut gir::Block {
        self.gir
            .get_mut(&self.cur_gir.unwrap())
            .unwrap()
            .get_block_mut(block)
            .unwrap()
    }

    pub fn push(&mut self, tyid: TyID, operand: Operand) {
        self.stack.push((tyid, operand));
    }

    pub fn pop(&mut self) -> (TyID, Operand) {
        self.stack.pop().unwrap()
    }

    pub fn pop_double(&mut self) -> ((TyID, Operand), (TyID, Operand)) {
        let r = self.pop();
        let l = self.pop();
        (l, r)
    }

    pub fn global(&self) -> &TyCtxt {
        &self.ty_ctxt.global
    }
}

impl Visitor for TyCheck {
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        // Create GIR for Fn
        let tyid = self.get_tyid(f.ident);
        self.gir.insert(tyid, GIR::new(tyid));
        self.cur_gir = Some(tyid);

        // Get tyir
        let TyIr::Fn(fn_tyir) = self.get_tyir(f.ident) else {
            panic!("Fn {:?} has not been defined", tyid);
        };

        // Insert Return Type as Local
        self.new_local(Local::new(f.ret_span(), Mutable::Mut, fn_tyir.ret));

        // Create symbols for function
        self.symbols.push(Default::default());

        // Insert Params as locals
        for (i, param) in f.params.iter().enumerate() {
            match &param.kind {
                ParamKind::SelfValue(_) => todo!(),
                ParamKind::SelfReference(_) => todo!(),
                ParamKind::Normal(mutable, symbol_id, _) => {
                    let local_id =
                        self.new_local(Local::new(param.span, *mutable, fn_tyir.params[i]));

                    if let Some(_) = self
                        .symbols
                        .last_mut()
                        .unwrap()
                        .insert(*symbol_id, local_id)
                    {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                                .with_span(|_| param.span),
                        );
                        return VisitorCF::Break;
                    }
                }
            }
        }

        // TODO: Currently not walking generics

        // Make block and iter
        self.new_block();
        walk_block(self, &f.block)?;

        let block = self.block_mut(self.cur_block());
        if !block.terminated() && fn_tyir.ret == TyID::BOTTOM {
            block.terminate(Terminator {
                kind: TerminatorKind::Return,
            });
        }

        self.pop_block();

        debug!("{}", self.cur_gir());

        // Remove symbols for function
        self.symbols.pop().unwrap();
        VisitorCF::Continue
    }

    fn visit_block(&mut self, _: &Block) -> VisitorCF {
        panic!("Dont use this function")
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        macro_rules! unify {
            ($l:expr, $r:expr, $err:expr) => {
                debug!("Stmt Unify {} {}", $l, $r);
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| s.span));
                    return VisitorCF::Break;
                }
            };
            ($l:expr, $r:expr, $err:expr, $span:expr) => {
                debug!("Stmt Unify {} {}", $l, $r);
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| $span));
                    return VisitorCF::Break;
                }
            };
        }

        match &s.kind {
            StmtKind::Let(mutable, symbol_id, span, ty, expr) => {
                // Visit expression
                self.visit_expr(expr)?;

                // Unify Types
                let lhs = self.get_tyid((ty.as_ref(), &self.generic_scope));
                let (rhs_ty, rhs_op) = self.pop();
                unify!(lhs, rhs_ty, TyCheckError::AssignWrongType);

                // Create local ID
                let local_id = self.new_local(Local::new(*span, *mutable, rhs_ty));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: *span,
                    kind: gir::StmtKind::Assign(
                        local_id,
                        Box::new(gir::Expr {
                            span: *span, //TODO: Replace Span
                            kind: gir::ExprKind::Value(rhs_op),
                        }),
                    ),
                });

                // Insert Ident into local Mapping
                if let Some(_) = self
                    .symbols
                    .last_mut()
                    .unwrap()
                    .insert(*symbol_id, local_id)
                {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                            .with_span(|_| s.span),
                    );
                    return VisitorCF::Break;
                }
            }
            StmtKind::ElseIf(_, _, _) | StmtKind::Else(_) => panic!("If statement in wrong place"),
            StmtKind::If(expr, block, stmt) => {
                enum IfStmt<'a> {
                    If(&'a Expr, &'a Block),
                    ElseIf(&'a Expr, &'a Block),
                    Else(&'a Block),
                }

                // Collect all if stmts into enum
                let mut stmts = Vec::new();
                stmts.push(IfStmt::If(&expr, &block));
                let mut next = stmt.as_ref();
                while let Some(stmt) = next.take() {
                    match &stmt.kind {
                        StmtKind::ElseIf(expr, block, stmt) => {
                            stmts.push(IfStmt::ElseIf(&expr, &block));
                            next = stmt.as_ref();
                        }
                        StmtKind::Else(block) => {
                            stmts.push(IfStmt::Else(&block));
                        }
                        _ => panic!("If Statement in wrong place"),
                    }
                }

                // Store bool ty for later
                let bool_ty = self.get_tyid(TyIr::Bool);

                let mut blocks_to_terminate = Vec::new();
                // let mut ends_with_else = false;
                for if_stmt in &stmts {
                    match if_stmt {
                        IfStmt::If(expr, block) | IfStmt::ElseIf(expr, block) => {
                            let cur_block = self.cur_block();

                            // Visit expression
                            self.visit_expr(expr)?;

                            // Unify Condition
                            let (cond_ty, cond_op) = self.pop();
                            unify!(cond_ty, bool_ty, TyCheckError::ExpectedBoolean, expr.span);

                            // Walk if block
                            let start_of_block = self.new_block();
                            walk_block(self, block)?;
                            let end_of_block = self.pop_block();

                            // Add Block to termination list
                            blocks_to_terminate.push(end_of_block);

                            let to_next = self.new_block_without_stack();
                            self.block_mut(cur_block).terminate(Terminator {
                                kind: TerminatorKind::Switch(
                                    cond_op,
                                    vec![
                                        SwitchCase::Value(0, to_next),
                                        SwitchCase::Default(start_of_block),
                                    ],
                                ),
                            });
                            self.push_block_stack(to_next);
                        }
                        IfStmt::Else(block) => {
                            // Walk else block
                            walk_block(self, block)?;
                            let end_of_block = self.pop_block();

                            // Add Block to termination list
                            blocks_to_terminate.push(end_of_block);

                            // Create trailing block
                            self.new_block();
                        }
                    }
                }

                let cur_block = self.cur_block();
                for block in &blocks_to_terminate {
                    self.block_mut(*block).terminate_no_replace(Terminator {
                        kind: TerminatorKind::Jump(cur_block),
                    });
                }
            }
            StmtKind::Expr(expr) => {
                self.visit_expr(expr)?;
            }
            StmtKind::Return(expr) => {
                let return_ty = self.cur_gir().get_local(LocalID::ZERO).tyid;

                if let Some(expr) = expr {
                    self.visit_expr(expr)?;
                    let (ty, op) = self.pop();

                    unify!(ty, return_ty, TyCheckError::FunctionReturnType);

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: s.span,
                        kind: gir::StmtKind::Assign(
                            LocalID::ZERO,
                            Box::new(gir::Expr {
                                span: expr.span,
                                kind: gir::ExprKind::Value(op),
                            }),
                        ),
                    })
                } else {
                    unify!(TyID::BOTTOM, return_ty, TyCheckError::FunctionReturnType);
                }

                let cur_block = self.cur_block();
                self.block_mut(cur_block).terminate(Terminator {
                    kind: TerminatorKind::Return,
                });
            }

            e => todo!("{e:?}"),
            // StmtKind::For(symbol_id, ty, expr, block) => todo!(),
            // StmtKind::While(expr, block) => todo!(),
            // StmtKind::Match(expr, match_arms) => todo!(),
            // StmtKind::Break => todo!(),
            // StmtKind::Continue => todo!(),
            // StmtKind::Return(expr) => todo!(),
            // StmtKind::Semi => todo!(),
        }

        VisitorCF::Continue
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        macro_rules! unify {
            ($l:expr, $r:expr, $err:expr) => {
                debug!("Expr Unify {} {}", $l, $r);
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| e.span));
                    return VisitorCF::Break;
                }
            };
            ($l:expr, $r:expr, $err:expr, $span:expr) => {
                debug!("Expr Unify {} {}", $l, $r);
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| $span));
                    return VisitorCF::Break;
                }
            };
        }

        let ret_ty = match &e.kind {
            ExprKind::Ident(symbol_id) => {
                // Search all symbols
                let mut found = None;
                for symbols in self.symbols.iter().rev() {
                    if let Some(local) = symbols.get(symbol_id) {
                        found = Some(*local);
                        break;
                    }
                }

                // Throw error if not found
                if found.is_none() {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::SymbolDoesntExist)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                }

                let local = self.cur_gir().get_local(found.unwrap());

                Some((local.tyid, Operand::Move(found.unwrap())))
            }
            ExprKind::Lit(literal) => {
                let const_id = self.new_constant(Constant::from_literal(*literal, e.span));

                let tyid = self.get_tyid(TyIr::from(*literal));

                Some((tyid, Operand::Constant(const_id)))
            }
            ExprKind::Function(None, _, symbol_id, exprs) => { //TODO: Generics
                // Iter over all expressions
                let mut exprs_tys = Vec::new();
                for expr in exprs {
                    self.visit_expr(expr)?;
                    let (ty, op) = self.pop();
                    exprs_tys.push((ty, op, expr.span))
                }

                // Get TyIr
                let TyIr::Fn(fn_tyir) = self.get_tyir(*symbol_id) else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::FunctionDoesntExist)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };

                let mut operands = Vec::new();
                for ((l, op, span), r) in exprs_tys.iter().zip(&fn_tyir.params) {
                    unify!(*l, *r, TyCheckError::IncorrectFunctionArgument, *span);
                    operands.push(*op);
                }

                let local_id = self.new_local(Local::new(e.span, Mutable::Imm, fn_tyir.ret));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        local_id,
                        Box::new(gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::Call(fn_tyir.tyid, operands),
                        }),
                    ),
                });

                Some((fn_tyir.ret, Operand::Move(local_id)))
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // Visit LHS
                self.visit_expr(lhs)?;
                let (lhs_ty, lhs_op) = self.pop();

                // Visit RHS
                self.visit_expr(rhs)?;
                let (rhs_ty, rhs_op) = self.pop();

                // Unify Types
                unify!(lhs_ty, rhs_ty, TyCheckError::LHSMatchRHS);

                let numeric = self.global().is_numeric(lhs_ty);
                let boolean = self.get_tyid(TyIr::Bool) == lhs_ty;

                let tyid = if op.operation() && numeric {
                    lhs_ty
                } else if op.comparsion() && (numeric || boolean) {
                    self.get_tyid(TyIr::Bool)
                } else if op.boolean_logic() && boolean {
                    self.get_tyid(TyIr::Bool)
                } else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::InvalidOperation)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };

                let local_id = self.new_local(Local::new(e.span, Mutable::Imm, tyid));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        local_id,
                        Box::new(gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::BinOp(*op, lhs_op, rhs_op),
                        }),
                    ),
                });

                Some((tyid, Operand::Move(local_id)))
            }
            ExprKind::UnOp(op, expr) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, e_op) = self.pop();

                let bool_ty = self.get_tyid(TyIr::Bool);

                let mapped_ty = match op {
                    UnOpKind::Not if e_ty == bool_ty => e_ty,
                    UnOpKind::Neg if self.global().is_numeric(e_ty) => e_ty,
                    UnOpKind::Deref => todo!(),
                    _ => {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::InvalidOperation)
                                .with_span(|_| e.span),
                        );
                        return VisitorCF::Break;
                    }
                };

                let local_id = self.new_local(Local::new(e.span, Mutable::Imm, mapped_ty));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        local_id,
                        Box::new(gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::UnOp(*op, e_op),
                        }),
                    ),
                });

                Some((mapped_ty, Operand::Move(local_id)))
            }
            ,

            // ExprKind::Cast(expr, ty) => {

            // },
            _ => todo!(),
            // ExprKind::Function(Some(expr), concrete_generics, symbol_id, exprs) => todo!(),
            // ExprKind::Assign(symbol_id, span, expr) => todo!(),
            // ExprKind::Ref(mutable, expr) => todo!(),
            // ExprKind::Array(exprs) => todo!(),
            // ExprKind::MethodSelf => todo!(),
            // ExprKind::Struct(ty, field_exprs) => todo!(),
            // ExprKind::Enum(ty, symbol_id, field_exprs) => todo!(),
            // ExprKind::Range(expr, expr1, range_type) => todo!(),
            // ExprKind::Field(expr, symbol_id) => todo!(),
            // ExprKind::Entity => todo!(),
            // ExprKind::Resource => todo!(),
            // ExprKind::Query(query_expr) => todo!(),
            // ExprKind::Schedule(schedule) => todo!(),
        };

        if let Some((ty, op)) = ret_ty {
            self.push(ty, op);
            VisitorCF::Continue
        } else {
            self.push(TyID::UNKNOWN, Operand::Unknown);
            VisitorCF::Break
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyCheckError {
    SymbolDoesntExist,
    FunctionDoesntExist,
    IncorrectFunctionArgument,
    SymbolIsNotFunction,
    SymbolRedefined,
    ExpectedBoolean,
    InvalidOperation,
    AssignWrongType,
    FunctionReturnType,
    ForLoopIterator,
    RangeMustEqual,
    LHSMatchRHS,
}

impl std::fmt::Display for TyCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TyCheckError::SymbolDoesntExist => "Symbol could not be found in the current scope",
            TyCheckError::SymbolRedefined => "Symbol is already defined",
            TyCheckError::ExpectedBoolean => "Expected boolean expression",
            TyCheckError::InvalidOperation => "Operation cannot be performed",
            TyCheckError::LHSMatchRHS => "LHS type must match RHS type of expresion",
            TyCheckError::AssignWrongType => "Cannot assign incorrect type",
            TyCheckError::FunctionReturnType => "Return type for function does not match",
            TyCheckError::ForLoopIterator => "Iterator has mismatched type",
            TyCheckError::RangeMustEqual => "LHS and RHS of range expression must be the same",
            TyCheckError::FunctionDoesntExist => "Could not find defined or imported ",
            TyCheckError::SymbolIsNotFunction => "Symbol is not a function",
            TyCheckError::IncorrectFunctionArgument => {
                "Type of argument does not match Type of signature"
            }
        };
        write!(f, "{}", s)
    }
}
