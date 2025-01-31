use ecsl_ast::expr::UnOpKind;
use ecsl_ast::parse::{ParamKind, RetTy};
use ecsl_ast::ty::{Mutable, Ty};
use ecsl_ast::{
    expr::{BinOpKind, Expr, ExprKind, Literal},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, walk_expr, walk_fn, walk_stmt, FnCtxt, Visitor, VisitorCF},
};
use ecsl_ast::{SourceAST, P};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::cons::Constant;
use ecsl_gir::expr::Operand;
use ecsl_gir::{Local, GIR};
use ecsl_index::{BlockID, ConstID, LocalID, SymbolID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use ecsl_ty::local::{self, LocalTyCtxt};
use ecsl_ty::{GenericsScope, TyIr};
use ext::IntoTyID;
use log::{debug, error, trace};
use lrpar::Span;
use std::collections::BTreeMap;
use std::sync::Arc;

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

    pub fn push(&mut self, tyid: TyID, operand: Operand) {
        debug!("{:?} {:?}", tyid, operand);
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

    // pub fn define_symbol(&mut self, id: SymbolID, span: Span, ty: TyID) {
    //     if self.get_symbol(id).is_some() {
    //         self.ty_ctxt.diag.push_error(
    //             EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
    //                 .with_span(|_| span),
    //         );
    //     }

    //     debug!("Define symbol {} as {}", id, ty);
    //     self.stack.last_mut().unwrap().define_symbol(id, ty);
    // }

    // pub fn use_symbol(&mut self, id: SymbolID, span: Span) -> Option<&TyID> {
    //     if let Some(symbol) = self.get_symbol(id) {
    //         Some(symbol)
    //     } else {
    //         self.ty_ctxt.diag.push_error(
    //             EcslError::new(ErrorLevel::Error, TyCheckError::SymbolDoesntExist)
    //                 .with_span(|_| span),
    //         );
    //         None
    //     }
    // }

    // pub fn get_symbol(&self, id: SymbolID) -> Option<&TyID> {
    //     let mut found = None;
    //     for scope in self.stack.iter().rev() {
    //         let ty = scope.get_symbol(id);
    //         if ty.is_some() {
    //             found = ty;
    //             break;
    //         }
    //     }
    //     return found;
    // }

    // pub fn get_tyid_from_ty(&self, ty: &Ty) -> TyID {
    //     self.ty_ctxt.get_tyid(ty, &self.generic_scope)
    // }

    // pub fn get_tyir_from_ty(&self, ty: &Ty) -> TyIr {
    //     self.ty_ctxt
    //         .global
    //         .get_tyir(self.ty_ctxt.get_tyid(ty, &self.generic_scope))
    // }

    // pub fn pop_scope(&mut self) {
    //     let scope = self.stack.pop().unwrap();
    //     debug!("Popped scope {:?}", scope);
    // }

    // pub fn push_id(&mut self, ty: TyID) {
    //     debug!("Pushed {:?}", ty);
    //     self.ty_stack.push(ty);
    // }

    // pub fn push_tyir(&mut self, tyir: TyIr) {
    //     let ty = self.ty_ctxt.global.tyid_from_tyir(tyir);
    //     debug!("Pushed {:?}", ty);
    //     self.ty_stack.push(ty);
    // }
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
        _ = match &f.ret {
            RetTy::None(span) => self.new_local(Local::new(*span, Mutable::Imm, TyID::BOTTOM)),
            RetTy::Ty(ty) => {
                self.new_local(Local::new(ty.span, Mutable::Mut, fn_tyir.ret.unwrap()))
            }
        };

        // Insert Params as locals
        for (i, param) in f.params.iter().enumerate() {
            match &param.kind {
                ParamKind::SelfValue(_) => todo!(),
                ParamKind::SelfReference(_) => todo!(),
                ParamKind::Normal(mutable, _, _) => {
                    self.new_local(Local::new(param.span, *mutable, fn_tyir.params[i]));
                }
            }
        }

        // TODO: Currently not walking generics

        self.visit_block(&f.block);

        debug!("{}", self.cur_gir());

        VisitorCF::Continue
    }

    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        let id = self.new_block();

        let cf = walk_block(self, b);

        self.block_stack.pop();
        cf
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        mod gir {
            pub use ecsl_gir::expr::*;
            pub use ecsl_gir::stmt::*;
            pub use ecsl_gir::term::*;
        }

        macro_rules! unify {
            ($l:ident, $r:ident, $err:expr) => {
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| s.span));
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

            _ => todo!(),
            // StmtKind::If(expr, block, stmt) => todo!(),
            // StmtKind::ElseIf(expr, block, stmt) => todo!(),
            // StmtKind::Else(block) => todo!(),
            // StmtKind::For(symbol_id, ty, expr, block) => todo!(),
            // StmtKind::While(expr, block) => todo!(),
            // StmtKind::Match(expr, match_arms) => todo!(),
            // StmtKind::Break => todo!(),
            // StmtKind::Continue => todo!(),
            // StmtKind::Return(expr) => todo!(),
            // StmtKind::Expr(expr) => todo!(),
            // StmtKind::Semi => todo!(),
        }

        VisitorCF::Continue
    }

    #[must_use]
    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        mod gir {
            pub use ecsl_gir::expr::*;
            pub use ecsl_gir::stmt::*;
            pub use ecsl_gir::term::*;
        }

        macro_rules! unify {
            ($l:ident, $r:ident, $err:expr) => {
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $err).with_span(|_| e.span));
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

                Some((lhs_ty, Operand::Move(local_id)))
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

            _ => todo!(),
            // ExprKind::Assign(symbol_id, span, expr) => todo!(),
            // ExprKind::Ref(mutable, expr) => todo!(),
            // ExprKind::Array(exprs) => todo!(),
            // ExprKind::MethodSelf => todo!(),
            // ExprKind::Struct(ty, field_exprs) => todo!(),
            // ExprKind::Enum(ty, symbol_id, field_exprs) => todo!(),
            // ExprKind::Range(expr, expr1, range_type) => todo!(),
            // ExprKind::Cast(expr, ty) => todo!(),
            // ExprKind::Field(expr, symbol_id) => todo!(),
            // ExprKind::Function(expr, concrete_generics, symbol_id, exprs) => todo!(),
            // ExprKind::Entity => todo!(),
            // ExprKind::Resource => todo!(),
            // ExprKind::Query(query_expr) => todo!(),
            // ExprKind::Schedule(schedule) => todo!(),
        };

        debug!("{:?}", ret_ty);

        if let Some((ty, op)) = ret_ty {
            self.push(ty, op);
            VisitorCF::Continue
        } else {
            self.push(TyID::UNKNOWN, Operand::Unknown);
            VisitorCF::Break
        }
    }
}

// impl Visitor for TyCheck {
//     fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
//         // debug!("Checking {:?}", f.to_header());
//         // self.new_scope();

//         let gid = self.ty_ctxt.get_global_id(f.ident).unwrap();
//         debug!("Fn GID {:?}", gid);
//         let TyIr::Fn(fn_tyir) = self
//             .ty_ctxt
//             .global
//             .get_tyir(self.ty_ctxt.global.get_or_create_tyid(gid))
//         else {
//             panic!("Fn {:?} has not been defined", gid);
//         };
//         debug!("{:?}", fn_tyir);
//         debug!("Int is {:?}", self.ty_ctxt.global.tyid_from_tyir(TyIr::Int));

//         // match &f.ret {
//         //     RetTy::None(_) => self.push_tyir(TyIr::Bottom),
//         //     RetTy::Ty(ty) => self.push_id(self.get_tyid_from_ty(ty)),
//         // }

//         // for (i, param) in f.params.iter().enumerate() {
//         //     match &param.kind {
//         //         ParamKind::SelfValue(_) => todo!(),
//         //         ParamKind::SelfReference(_) => todo!(),
//         //         ParamKind::Normal(_, symbol_id, _) => {
//         //             self.define_symbol(*symbol_id, param.span, fn_tyir.params[i]);
//         //         }
//         //     }
//         // }

//         walk_fn(self, f);

//         self.pop_scope();
//         VisitorCF::Continue
//     }

//     fn visit_block(&mut self, b: &Block) -> VisitorCF {
//         self.new_scope();
//         walk_block(self, b);
//         self.pop_scope();
//         VisitorCF::Continue
//     }

//     fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
//         if let StmtKind::For(id, ty, _, _) = &s.kind {
//             self.define_symbol(*id, s.span, self.get_tyid_from_ty(&ty));
//         }

//         let size = self.ty_stack.len();
//         if walk_stmt(self, s) == VisitorCF::Break {
//             let _ = self.ty_stack.split_off(size);
//             debug!("Restore stack to {:?}", self.ty_stack);
//             return VisitorCF::Break;
//         }

//         match &s.kind {
//             StmtKind::Let(_, symbol_id, span, asscribed_ty, _) => {
//                 let ty = self.pop_ty();
//                 let asscribed_ty = self.ty_ctxt.get_tyid(asscribed_ty, &self.generic_scope);
//                 if ty != asscribed_ty {}

//                 self.define_symbol(*symbol_id, *span, ty);
//                 VisitorCF::Continue
//             }
//             StmtKind::If(e, _, _) | StmtKind::ElseIf(e, _, _) => {
//                 if self.pop_ty() != self.ty_ctxt.global.tyid_from_tyir(TyIr::Bool) {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::ExpectedBoolean)
//                             .with_span(|_| e.span),
//                     );
//                 }
//                 VisitorCF::Continue
//             }
//             StmtKind::Else(_) => VisitorCF::Continue,
//             StmtKind::Return(None) => {
//                 // Pop the functions return type
//                 if self.pop_ty() != self.ty_ctxt.global.tyid_from_tyir(TyIr::Bottom) {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
//                             .with_span(|_| s.span),
//                     );
//                 }
//                 VisitorCF::Continue
//             }
//             StmtKind::Return(Some(_)) => {
//                 // Pop Function Return Type and the Expressions Type
//                 let (l, r) = self.pop_double_ty();
//                 if l != r {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
//                             .with_span(|_| s.span),
//                     );
//                 }
//                 return VisitorCF::Continue;
//             }
//             StmtKind::Expr(_) => VisitorCF::Continue,
//             StmtKind::Semi => VisitorCF::Continue,
//             StmtKind::For(_, ty, expr, _) => {
//                 match &expr.kind {
//                     ExprKind::Range(_, _, _) => (),
//                     ExprKind::Query(_) => todo!(), //TODO:
//                     _ => panic!("Non range or query found in for loop"),
//                 }

//                 let ty = self.get_tyid_from_ty(&ty);
//                 let over = self.pop_ty();
//                 if ty != over {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::FunctionReturnType)
//                             .with_span(|_| s.span),
//                     );
//                 }

//                 VisitorCF::Continue
//             }

//             StmtKind::While(_, _) => todo!(),
//             StmtKind::Match(_, _) => todo!(),
//             StmtKind::Break => todo!(),
//             StmtKind::Continue => todo!(),
//             // e => {
//             //     error!("Unimplemented {:?}", e);
//             //     VisitorCF::Continue
//             // }
//         }
//     }

//     fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
//         let size = self.ty_stack.len();
//         if walk_expr(self, e) == VisitorCF::Break {
//             let _ = self.ty_stack.split_off(size);
//             debug!("Restore stack to {:?}", self.ty_stack);
//             return VisitorCF::Break;
//         }

//         match &e.kind {
//             ExprKind::Assign(symbol_id, span, _) => {
//                 let expr_ty = self.pop_ty();
//                 if let Some(ident_ty) = self.use_symbol(*symbol_id, *span) {
//                     if *ident_ty != expr_ty {
//                         self.ty_ctxt.diag.push_error(
//                             EcslError::new(ErrorLevel::Error, TyCheckError::AssignWrongType)
//                                 .with_span(|_| *span),
//                         );
//                         return VisitorCF::Break;
//                     }
//                     VisitorCF::Continue
//                 } else {
//                     VisitorCF::Break
//                 }
//             }
//             ExprKind::Ident(symbol_id) => {
//                 if let Some(ty) = self.use_symbol(*symbol_id, e.span) {
//                     let ty = ty.clone();
//                     self.push_id(ty);
//                     VisitorCF::Continue
//                 } else {
//                     self.push_tyir(TyIr::Unknown);
//                     return VisitorCF::Break;
//                 }
//             }
//             ExprKind::Function(None, _, id, args) => {
//                 let arg_tys = (0..args.len())
//                     .map(|_| self.pop_ty())
//                     .rev()
//                     .collect::<Vec<_>>();

//                 let Some(gid) = self.ty_ctxt.get_global_id(*id) else {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::FunctionDoesntExist)
//                             .with_span(|_| e.span),
//                     );
//                     return VisitorCF::Break;
//                 };

//                 let TyIr::Fn(fn_tyir) = self
//                     .ty_ctxt
//                     .global
//                     .get_tyir(self.ty_ctxt.global.get_or_create_tyid(gid))
//                 else {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::FunctionDoesntExist)
//                             .with_span(|_| e.span),
//                     );
//                     return VisitorCF::Break;
//                 };

//                 // Compare all parameters
//                 for (i, (l, r)) in fn_tyir.params.iter().zip(arg_tys).enumerate() {
//                     trace!("{:?}, {:?}", l, r);
//                     if *l != r {
//                         self.ty_ctxt.diag.push_error(
//                             EcslError::new(
//                                 ErrorLevel::Error,
//                                 TyCheckError::IncorrectFunctionArgument,
//                             )
//                             .with_span(|_| args[i].span),
//                         );
//                     }
//                 }

//                 VisitorCF::Continue
//             }
//             ExprKind::Function(Some(_), _, _, _) => {
//                 error!("Assoc functions not implemented");
//                 VisitorCF::Break
//             }
//             ExprKind::Lit(literal) => {
//                 let tyir = match literal {
//                     Literal::Int => TyIr::Int,
//                     Literal::Bool => TyIr::Bool,
//                     Literal::Float => TyIr::Float,
//                     Literal::Char => TyIr::Char,

//                     Literal::String => todo!(), //TODO:
//                 };
//                 self.push_tyir(tyir);
//                 VisitorCF::Continue
//             }
//             ExprKind::BinOp(op, _, _) => {
//                 let (l, r) = self.pop_double_ty();
//                 if l != r {
//                     error!("Invalid bin op {:?}", op);
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::InvalidBinOp)
//                             .with_span(|_| e.span),
//                     );

//                     self.push_id(self.ty_ctxt.global.unknown_ty());
//                     return VisitorCF::Break;
//                 }

//                 match op {
//                     BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
//                         self.push_id(l)
//                     }
//                     BinOpKind::And
//                     | BinOpKind::Or
//                     | BinOpKind::Eq
//                     | BinOpKind::Neq
//                     | BinOpKind::Lt
//                     | BinOpKind::Leq
//                     | BinOpKind::Gt
//                     | BinOpKind::Geq => {
//                         self.push_tyir(TyIr::Bool);
//                     }
//                 }

//                 return VisitorCF::Continue;
//             }
//             ExprKind::Range(_, _, _) => {
//                 let (l, r) = self.pop_double_ty();
//                 if l != r {
//                     self.ty_ctxt.diag.push_error(
//                         EcslError::new(ErrorLevel::Error, TyCheckError::RangeMustEqual)
//                             .with_span(|_| e.span),
//                     );
//                     self.push_tyir(TyIr::Unknown);
//                     return VisitorCF::Break;
//                 }
//                 self.push_id(l);
//                 VisitorCF::Continue
//             }

//             e => {
//                 error!("Unimplemented {:?}", e);
//                 VisitorCF::Break
//             } // ExprKind::Ref(mutable, expr) => todo!(),
//               // ExprKind::UnOp(un_op_kind, expr) => todo!(),
//               // ExprKind::Array(vec) => todo!(),
//               // ExprKind::MethodSelf => todo!(),
//               // ExprKind::Struct(ty, vec) => todo!(),
//               // ExprKind::Enum(ty, symbol_id, vec) => todo!(),
//               // ExprKind::Cast(expr, ty) => todo!(),
//               // ExprKind::Field(expr, symbol_id) => todo!(),
//               // ExprKind::Function(expr, concrete_generics, symbol_id, vec) => todo!(),
//               // ExprKind::Entity => todo!(),
//               // ExprKind::Resource => todo!(),
//               // ExprKind::Query(query_expr) => todo!(),
//               // ExprKind::Schedule(schedule) => todo!(),
//         }
//     }

//     // fn visit_
// }

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
