use ecsl_ast::expr::{BinOpKind, RangeType, UnOpKind};
use ecsl_ast::parse::{Immediate, ParamKind};
use ecsl_ast::stmt::InlineBytecode;
use ecsl_ast::ty::{Mutable, TyKind};
use ecsl_ast::SourceAST;
use ecsl_ast::{
    expr::{Expr, ExprKind},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, FnCtxt, Visitor, VisitorCF},
};
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::cons::Constant;
use ecsl_gir::expr::{BinOp, Operand, OperandKind, UnOp};
use ecsl_gir::term::{SwitchCase, Terminator, TerminatorKind};
use ecsl_gir::{Local, LocalKind, Place, Projection, GIR};
use ecsl_gir_pass::linker::FunctionLinker;
use ecsl_index::{BlockID, ConstID, LocalID, SymbolID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use ecsl_ty::local::LocalTyCtxt;
use ecsl_ty::{GenericsScope, MonoFnDef, TyIr};
use ext::IntoTyID;
use log::debug;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

#[allow(unused)]
mod gir {
    pub use ecsl_gir::expr::*;
    pub use ecsl_gir::stmt::*;
    pub use ecsl_gir::term::*;
    pub use ecsl_gir::*;
}

pub mod ext;

pub fn ty_check(
    ast: &SourceAST,
    ty_ctxt: Arc<LocalTyCtxt>,
    linker: FunctionLinker,
) -> FunctionLinker {
    let mut ty_check = TyCheck::new(ty_ctxt, linker);
    ty_check.visit_ast(ast);
    ty_check.linker
}

pub struct TyCheck {
    ty_ctxt: Arc<LocalTyCtxt>,
    linker: FunctionLinker,
    generic_scope: GenericsScope,

    cur_gir: Option<GIR>,
    generated_gir: Vec<TyID>,

    block_stack: Vec<BlockID>,
    symbols: Vec<BTreeMap<SymbolID, Place>>,
    stack: Vec<(TyID, Operand)>,
}

#[allow(unused)]
enum TerminationKind {
    Lower,
    Equal,
    Higher,
}

impl TyCheck {
    pub fn new(ty_ctxt: Arc<LocalTyCtxt>, linker: FunctionLinker) -> Self {
        Self {
            ty_ctxt,
            linker,
            generic_scope: GenericsScope::new(),
            cur_gir: None,
            generated_gir: Default::default(),
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
        self.cur_gir.as_ref().unwrap()
    }

    pub fn cur_gir_mut(&mut self) -> &mut GIR {
        self.cur_gir.as_mut().unwrap()
    }

    pub fn new_block(&mut self) -> BlockID {
        let block = self.cur_gir.as_mut().unwrap().new_block();
        self.block_stack.push(block);
        self.symbols.push(Default::default());
        block
    }

    pub fn new_block_without_stack(&mut self) -> BlockID {
        self.cur_gir.as_mut().unwrap().new_block()
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
        self.cur_gir.as_mut().unwrap().new_local(local)
    }

    pub fn get_local_mut(&mut self, local: LocalID) -> &mut Local {
        self.cur_gir_mut().get_local_mut(local)
    }

    pub fn new_constant(&mut self, cons: Constant) -> ConstID {
        self.cur_gir.as_mut().unwrap().new_constant(cons)
    }

    pub fn push_stmt_to_cur_block(&mut self, stmt: ecsl_gir::stmt::Stmt) {
        let block_id = self.cur_block();
        self.cur_gir
            .as_mut()
            .unwrap()
            .get_block_mut(block_id)
            .unwrap()
            .push(stmt);
    }

    pub fn cur_block(&self) -> BlockID {
        *self.block_stack.last().unwrap()
    }

    pub fn block_mut(&mut self, block: BlockID) -> &mut gir::Block {
        self.cur_gir.as_mut().unwrap().get_block_mut(block).unwrap()
    }

    fn terminate_with_new_block(
        &mut self,
        term_kind: TerminationKind,
        f: impl FnOnce(&mut ecsl_gir::Block, BlockID),
    ) -> BlockID {
        let new_block = self.new_block_without_stack();
        self.terminate_with_existing_block(term_kind, new_block, f)
    }

    fn terminate_with_existing_block(
        &mut self,
        term_kind: TerminationKind,
        existing_block: BlockID,
        f: impl FnOnce(&mut ecsl_gir::Block, BlockID),
    ) -> BlockID {
        let orig_block = self.cur_block();
        f(self.block_mut(orig_block), existing_block);
        match term_kind {
            TerminationKind::Lower => {
                let _ = self.symbols.pop().unwrap();
                self.block_stack.pop().unwrap();
                self.block_stack.push(existing_block);
            }
            TerminationKind::Equal => {
                self.block_stack.pop().unwrap();
                self.block_stack.push(existing_block);
            }
            TerminationKind::Higher => {
                self.symbols.push(Default::default());
                self.block_stack.push(existing_block);
            }
        }
        existing_block
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

    pub fn find_symbol(&self, symbol_id: SymbolID) -> Option<Place> {
        let mut found = None;
        for symbols in self.symbols.iter().rev() {
            if let Some(local) = symbols.get(&symbol_id) {
                found = Some(local.clone());
                break;
            }
        }
        return found;
    }
}

impl Visitor for TyCheck {
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        self.generic_scope.add_opt(f.generics.clone());

        // Create GIR for Fn
        let tyid = self.get_tyid(f.ident);
        self.cur_gir = Some(GIR::new(tyid));

        // Get tyir
        let TyIr::Fn(fn_tyir) = self.get_tyir(f.ident) else {
            panic!("Fn {:?} has not been defined", tyid);
        };

        // Insert Return Type as Local
        self.new_local(Local::new(
            f.ret_span(),
            Mutable::Mut,
            fn_tyir.ret,
            LocalKind::Ret,
        ));

        // Create symbols for function
        self.symbols.push(Default::default());

        // Insert Params as locals
        for (i, param) in f.params.iter().enumerate() {
            match &param.kind {
                ParamKind::SelfValue(_) => todo!(),
                ParamKind::SelfReference(_) => todo!(),
                ParamKind::Normal(mutable, symbol_id, _) => {
                    let local_id = self.new_local(Local::new(
                        param.span,
                        *mutable,
                        fn_tyir.params[i],
                        LocalKind::Arg,
                    ));

                    if let Some(_) = self
                        .symbols
                        .last_mut()
                        .unwrap()
                        .insert(*symbol_id, Place::from_local(local_id))
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

        // Remove symbols for function
        self.symbols.pop().unwrap();

        debug!("{}", self.cur_gir());

        self.linker.add_gir(tyid, self.cur_gir.take().unwrap());
        self.generated_gir.push(tyid);

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

        macro_rules! err_if {
            ($c:expr, $e:expr, $s:expr) => {
                if $c {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| $s));
                    return VisitorCF::Break;
                }
            };
        }

        macro_rules! err {
            ($e:expr,$s:expr) => {
                self.ty_ctxt
                    .diag
                    .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| $s));
                return VisitorCF::Break;
            };
        }
        match &s.kind {
            StmtKind::Let(mutable, symbol_id, span, ty, expr) => {
                // Visit expression
                self.visit_expr(expr)?;
                let (rhs_ty, rhs_op) = self.pop();

                // Unify Types
                if let Some(ty) = ty {
                    let let_ty = self.get_tyid((ty.as_ref(), &self.generic_scope));
                    unify!(let_ty, rhs_ty, TyCheckError::AssignWrongType);
                }

                let local_id = match &rhs_op {
                    Operand::Copy(place) | Operand::Move(place) => {
                        let local = self.get_local_mut(place.local);

                        if local.kind == LocalKind::Temp {
                            local.kind = LocalKind::Let;
                            local.mutable = *mutable;
                            local.span = *span;

                            place.local
                        } else {
                            let local_id =
                                self.new_local(Local::new(*span, *mutable, rhs_ty, LocalKind::Let));

                            // Create Assignment Stmt
                            self.push_stmt_to_cur_block(gir::Stmt {
                                span: *span,
                                kind: gir::StmtKind::Assign(
                                    Place::from_local(local_id),
                                    gir::Expr {
                                        span: *span,
                                        kind: gir::ExprKind::Value(rhs_op),
                                    },
                                ),
                            });

                            local_id
                        }
                    }
                    Operand::Constant(_) => {
                        // Create local ID
                        let local_id =
                            self.new_local(Local::new(*span, *mutable, rhs_ty, LocalKind::Let));

                        // Create Assignment Stmt
                        self.push_stmt_to_cur_block(gir::Stmt {
                            span: *span,
                            kind: gir::StmtKind::Assign(
                                Place::from_local(local_id),
                                gir::Expr {
                                    span: *span,
                                    kind: gir::ExprKind::Value(rhs_op),
                                },
                            ),
                        });

                        // Return local
                        local_id
                    }
                };

                // Insert Ident into local Mapping
                if let Some(_) = self
                    .symbols
                    .last_mut()
                    .unwrap()
                    .insert(*symbol_id, Place::from_local(local_id))
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
                #[derive(Debug)]
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

                let symbols_len = self.symbols.len();

                let mut blocks_to_terminate = Vec::new();
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

                            // TODO: Check soundness of terminating if statements
                            let to_next = self.new_block_without_stack();
                            self.block_mut(cur_block).terminate(Terminator {
                                kind: TerminatorKind::Switch(
                                    cond_op,
                                    vec![
                                        SwitchCase::Value(Immediate::Bool(true), start_of_block),
                                        SwitchCase::Default(to_next),
                                    ],
                                ),
                            });
                            self.block_stack.push(to_next);
                        }
                        IfStmt::Else(block) => {
                            // Walk else block
                            walk_block(self, block)?;

                            // Add Block to termination list
                            blocks_to_terminate.push(self.cur_block());

                            // Create trailing block
                            let trailing_block = self.new_block_without_stack();
                            self.block_stack.push(trailing_block);
                        }
                    }
                }

                let cur_block = self.cur_block();
                for block in &blocks_to_terminate {
                    self.block_mut(*block).terminate_no_replace(Terminator {
                        kind: TerminatorKind::Jump(cur_block),
                    });
                }

                assert_eq!(
                    symbols_len,
                    self.symbols.len(),
                    "Internal Compiler Error: symbol table incorrect after if statement"
                );
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
                            Place::return_location(),
                            gir::Expr {
                                span: expr.span,
                                kind: gir::ExprKind::Value(op),
                            },
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
            StmtKind::BYT(bytecode) => {
                for InlineBytecode { span, ins } in bytecode {
                    let mut ins = ins.clone();
                    for i in ins.operand.iter_mut() {
                        if let Immediate::SymbolOf(symbol_id) = i {
                            let found = self.find_symbol(*symbol_id);

                            // Throw error if not found
                            if found.is_none() {
                                self.ty_ctxt.diag.push_error(
                                    EcslError::new(
                                        ErrorLevel::Error,
                                        TyCheckError::SymbolDoesntExist,
                                    )
                                    .with_span(|_| *span),
                                );
                                return VisitorCF::Break;
                            }

                            *i = Immediate::LocalOf(found.unwrap().local);
                        }
                    }

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: *span,
                        kind: gir::StmtKind::BYT(ins.clone()),
                    })
                }
            }
            StmtKind::For(symbol_id, ty, expr, block) => {
                let ExprKind::Range(lhs, rhs, range_type) = &expr.kind else {
                    panic!()
                };

                // Visit LHS
                self.visit_expr(lhs.as_ref())?;
                let (lhs_ty, lhs_op) = self.pop();

                // Visit RHS
                self.visit_expr(rhs.as_ref())?;
                let (rhs_ty, rhs_op) = self.pop();

                // Get For loop iterator Ty
                let iterator_tyid = self.get_tyid((ty.as_ref(), &self.generic_scope));

                // Unify Types
                unify!(lhs_ty, rhs_ty, TyCheckError::RangeMustEqual);
                unify!(iterator_tyid, lhs_ty, TyCheckError::ForLoopIterator);

                let symbols_len = self.symbols.len();

                // Create local ID
                let iterator_local_id = self.new_local(Local::new(
                    s.span,
                    Mutable::Mut,
                    iterator_tyid,
                    LocalKind::Let,
                ));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: s.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(iterator_local_id),
                        gir::Expr {
                            span: expr.span, //TODO: Replace Span
                            kind: gir::ExprKind::Value(lhs_op),
                        },
                    ),
                });

                let internal_max = self.new_local(Local::new(
                    rhs.span,
                    Mutable::Imm,
                    rhs_ty,
                    LocalKind::Internal,
                ));

                // Create Max Value
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: s.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(internal_max),
                        gir::Expr {
                            span: expr.span, //TODO: Replace Span
                            kind: gir::ExprKind::Value(rhs_op),
                        },
                    ),
                });

                // Jump to next block
                let cond_block =
                    self.terminate_with_new_block(TerminationKind::Higher, |block, next| {
                        block.terminate(Terminator {
                            kind: TerminatorKind::Jump(next),
                        })
                    });

                // Insert Ident into local Mapping
                if let Some(_) = self
                    .symbols
                    .last_mut()
                    .unwrap()
                    .insert(*symbol_id, Place::from_local(iterator_local_id))
                {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                            .with_span(|_| s.span),
                    );
                    return VisitorCF::Break;
                }

                let comparison =
                    self.new_local(Local::new(expr.span, Mutable::Imm, lhs_ty, LocalKind::Temp));

                self.push_stmt_to_cur_block(gir::Stmt {
                    span: s.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(comparison),
                        gir::Expr {
                            span: expr.span,
                            kind: gir::ExprKind::BinOp(
                                BinOp(
                                    OperandKind::Int,
                                    match range_type {
                                        RangeType::Exclusive => BinOpKind::Lt,
                                        RangeType::Inclusive => BinOpKind::Leq,
                                    },
                                ),
                                Operand::Copy(Place::from_local(iterator_local_id)),
                                Operand::Copy(Place::from_local(internal_max)),
                            ),
                        },
                    ),
                });

                let leave_block = self.new_block_without_stack();

                let _start_of_block =
                    self.terminate_with_new_block(TerminationKind::Higher, |block, next| {
                        block.terminate(Terminator {
                            kind: TerminatorKind::Switch(
                                Operand::Move(Place::from_local(comparison)),
                                vec![
                                    SwitchCase::Value(Immediate::Bool(true), next),
                                    SwitchCase::Default(leave_block),
                                ],
                            ),
                        });
                    });

                walk_block(self, block)?;

                let _increment_block =
                    self.terminate_with_new_block(TerminationKind::Lower, |block, next| {
                        block.terminate_no_replace(Terminator {
                            kind: TerminatorKind::Jump(next),
                        });
                    });

                let one_const = self.new_constant(Constant::Internal {
                    imm: Immediate::Int(1),
                });
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: s.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(iterator_local_id),
                        gir::Expr {
                            span: expr.span,
                            kind: gir::ExprKind::BinOp(
                                BinOp(OperandKind::Int, BinOpKind::Add),
                                Operand::Copy(Place::from_local(iterator_local_id)),
                                Operand::Constant(one_const),
                            ),
                        },
                    ),
                });

                self.terminate_with_existing_block(
                    TerminationKind::Lower,
                    cond_block,
                    |block, next| {
                        block.terminate(Terminator {
                            kind: TerminatorKind::Jump(next),
                        });
                    },
                );
                self.block_stack.push(leave_block);

                assert_eq!(
                    symbols_len,
                    self.symbols.len(),
                    "Internal Compiler Error: symbol table incorrect after for loop"
                );
            }
            StmtKind::Match(expr, match_arms) => {
                self.visit_expr(expr.as_ref())?;
                let (ty, op) = self.pop();

                let TyIr::ADT(enum_tyir) = self.get_tyir(ty) else {
                    err!(TyCheckError::TypeCannotBeMatched, expr.span);
                };
                err_if!(
                    !enum_tyir.is_enum(),
                    TyCheckError::TypeCannotBeMatched,
                    expr.span
                );

                let local_id = match op {
                    Operand::Copy(place) | Operand::Move(place) => {
                        let local = self.get_local_mut(place.local);
                        local.kind = LocalKind::Internal;
                        place.local
                    }
                    Operand::Constant(_) => {
                        // Create local ID
                        let local_id = self.new_local(Local::new(
                            expr.span,
                            Mutable::Imm,
                            ty,
                            LocalKind::Internal,
                        ));

                        // Create Assignment Stmt
                        self.push_stmt_to_cur_block(gir::Stmt {
                            span: expr.span,
                            kind: gir::StmtKind::Assign(
                                Place::from_local(local_id),
                                gir::Expr {
                                    span: expr.span,
                                    kind: gir::ExprKind::Value(op),
                                },
                            ),
                        });

                        // Return local
                        local_id
                    }
                };

                let current_block = self.cur_block();
                let out_block = self.new_block_without_stack();

                let mut arms = BTreeMap::new();
                let mut default_arm = None;

                for arm in match_arms {
                    if default_arm.is_some() {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Warning, TyCheckError::MatchArmUnreachable)
                                .with_span(|_| (arm.span)),
                        );
                        continue;
                    }

                    // Make block for arm
                    let arm_block = self.new_block();
                    if let Some(case) = arm.ident {
                        let symbol = self.ty_ctxt.table.get_symbol(case).unwrap();
                        let Some(var_id) = enum_tyir.variant_hash.get(&symbol.name) else {
                            err!(TyCheckError::NoMemberWithName, arm.span);
                        };
                        err_if!(
                            arms.insert(var_id, arm_block).is_some(),
                            TyCheckError::MatchArmDuplicate,
                            arm.span
                        );
                        let var = enum_tyir.variant_kinds.get(var_id).unwrap();

                        let mut fields = BTreeSet::new();
                        for field in &arm.fields {
                            let symbol = self.ty_ctxt.table.get_symbol(field.ident).unwrap();
                            let Some(field_id) = var.field_hash.get(&symbol.name) else {
                                err!(TyCheckError::NoMemberWithName, arm.span);
                            };

                            err_if!(
                                !fields.insert(field_id),
                                TyCheckError::DuplicateMember,
                                arm.span
                            );
                            let field_def = var.field_tys.get(field_id).unwrap();

                            // Insert Ident into local Mapping

                            err_if!(
                                self.symbols
                                    .last_mut()
                                    .unwrap()
                                    .insert(
                                        field.ident,
                                        Place::from_local(local_id).with_projection(
                                            Projection::Field {
                                                ty,
                                                vid: *var_id,
                                                fid: *field_id,
                                                new_ty: field_def.ty
                                            }
                                        )
                                    )
                                    .is_some(),
                                TyCheckError::DuplicateMember,
                                arm.span
                            );
                        }

                        err_if!(
                            fields.len() != var.field_tys.len(),
                            TyCheckError::MissingFields,
                            arm.span
                        );
                    } else {
                        default_arm = Some(arm_block);
                    }

                    walk_block(self, &arm.block)?;
                    self.terminate_with_existing_block(
                        TerminationKind::Lower,
                        out_block,
                        |block, out_block| {
                            block.terminate_no_replace(Terminator {
                                kind: TerminatorKind::Jump(out_block),
                            });
                        },
                    );
                }

                err_if!(
                    default_arm.is_none() && arms.len() != enum_tyir.variant_kinds.len(),
                    TyCheckError::MatchNotExhaustive,
                    expr.span
                );

                let mut cases = Vec::new();
                for (vid, block_id) in arms.iter() {
                    cases.push(SwitchCase::Value(
                        Immediate::UByte(vid.inner() as u8),
                        *block_id,
                    ));
                }
                if let Some(default) = default_arm {
                    cases.push(SwitchCase::Default(default));
                }

                self.block_mut(current_block).terminate(Terminator {
                    kind: TerminatorKind::Switch(
                        Operand::Copy(
                            Place::from_local(local_id)
                                .with_projection(Projection::Discriminant { tyid: ty }),
                        ),
                        cases,
                    ),
                });
            }
            e => todo!("{e:?}"),
            // StmtKind::While(expr, block) => todo!(),
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

        macro_rules! err_if {
            ($c:expr, $e:expr, $s:expr) => {
                if $c {
                    self.ty_ctxt
                        .diag
                        .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| $s));
                    return VisitorCF::Break;
                }
            };
        }

        macro_rules! err {
            ($e:expr,$s:expr) => {
                self.ty_ctxt
                    .diag
                    .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| $s));
                return VisitorCF::Break;
            };
        }

        let ret_ty = match &e.kind {
            ExprKind::Assign(lhs, span, rhs) => {
                // Visit lhs
                let temp_block = self.new_block();
                self.visit_expr(lhs)?;
                let (lhs_ty, lhs_op) = self.pop();
                self.pop_block();
                self.cur_gir_mut().remove_block(temp_block);

                let lhs_place = match lhs_op {
                    Operand::Copy(place) | Operand::Move(place) => place,
                    Operand::Constant(_) => {
                        err!(TyCheckError::CannotAssignToLHS, lhs.span);
                    }
                };

                let local = self.cur_gir().get_local(lhs_place.local);
                let can_assign = match local.kind {
                    LocalKind::Temp => false,
                    LocalKind::Let => true,
                    LocalKind::Arg => true,
                    LocalKind::Internal => false,
                    LocalKind::Ret => panic!("Internal Compiler Error"),
                };
                err_if!(!can_assign, TyCheckError::CannotAssignToLHS, lhs.span);

                // Visit rhs
                self.visit_expr(rhs)?;
                let (rhs_ty, rhs_op) = self.pop();

                let local_tyid = lhs_place.projected_tyid(self.cur_gir());

                unify!(rhs_ty, local_tyid, TyCheckError::AssignWrongType, *span);

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        lhs_place,
                        gir::Expr {
                            span: *span,
                            kind: gir::ExprKind::Value(rhs_op),
                        },
                    ),
                });

                return VisitorCF::Continue;
            }
            ExprKind::Ident(symbol_id) => {
                // Search all symbols
                let found = self.find_symbol(*symbol_id);

                // Throw error if not found
                if found.is_none() {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::SymbolDoesntExist)
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };
                let found = found.unwrap();

                Some((found.projected_tyid(self.cur_gir()), Operand::Copy(found)))
            }
            ExprKind::Lit(literal) => {
                let tyid = self.get_tyid(TyIr::from(*literal));
                let const_id = self.new_constant(Constant::External {
                    span: e.span,
                    tyid,
                    kind: *literal,
                });

                Some((tyid, Operand::Constant(const_id)))
            }
            ExprKind::Function(None, generics, symbol_id, exprs) => {
                // Get TyIr
                let TyIr::Fn(fn_tyir) = self.get_tyir(*symbol_id) else {
                    err!(TyCheckError::FunctionDoesntExist, e.span);
                };

                let mut concrete_generics = BTreeMap::new();
                if fn_tyir.generics.is_some() && generics.is_some() {
                    let generic_count = fn_tyir.generics.unwrap();
                    let generics = generics.as_ref().unwrap();
                    err_if!(
                        generic_count != generics.params.len(),
                        TyCheckError::InvalidGenericTypes,
                        generics.span
                    );

                    for (i, p) in generics.params.iter().enumerate() {
                        concrete_generics.insert(
                            self.get_tyid(TyIr::GenericParam(i)),
                            self.get_tyid((p, &self.generic_scope)),
                        );
                    }
                }

                let ret_ty = concrete_generics
                    .get(&fn_tyir.ret)
                    .cloned()
                    .unwrap_or(fn_tyir.ret);

                // Preallocate the size of the return value on the stack
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::AllocReturn(ret_ty),
                });

                // Iter over all expressions
                let mut exprs_tys = Vec::new();
                for expr in exprs {
                    self.visit_expr(expr)?;
                    let (ty, op) = self.pop();
                    exprs_tys.push((ty, op, expr.span))
                }

                err_if!(
                    exprs_tys.len() != fn_tyir.params.len(),
                    TyCheckError::IncorrectFunctionArguments,
                    e.span
                );

                let mut operands = Vec::new();
                for ((l, op, span), r) in exprs_tys.iter().zip(&fn_tyir.params) {
                    let r = concrete_generics.get(r).cloned().unwrap_or(*r);

                    unify!(*l, r, TyCheckError::IncorrectFunctionArguments, *span);

                    operands.push(op.clone());
                }

                let mut tyid = fn_tyir.tyid;

                if concrete_generics.len() > 0 {
                    tyid = self.ty_ctxt.global.tyid_from_tyir(TyIr::MonoFn(MonoFnDef {
                        tyid: fn_tyir.tyid,
                        mono: concrete_generics,
                    }));

                    self.linker.mono.insert(fn_tyir.tyid, tyid);
                }

                let local_id =
                    self.new_local(Local::new(e.span, Mutable::Imm, ret_ty, LocalKind::Temp));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::Call(tyid, operands),
                        },
                    ),
                });

                Some((ret_ty, Operand::Move(Place::from_local(local_id))))
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

                let op_kind = match self.get_tyir(lhs_ty) {
                    TyIr::Bool => OperandKind::Bool,
                    TyIr::Int => OperandKind::Int,
                    TyIr::Float => OperandKind::Float,
                    _ => panic!(),
                };

                let local_id =
                    self.new_local(Local::new(e.span, Mutable::Imm, tyid, LocalKind::Temp));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::BinOp(BinOp(op_kind, *op), lhs_op, rhs_op),
                        },
                    ),
                });

                Some((tyid, Operand::Move(Place::from_local(local_id))))
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

                let op_kind = match self.get_tyir(e_ty) {
                    TyIr::Bool => OperandKind::Bool,
                    TyIr::Int => OperandKind::Int,
                    TyIr::Float => OperandKind::Float,
                    _ => panic!(),
                };

                let local_id =
                    self.new_local(Local::new(e.span, Mutable::Imm, mapped_ty, LocalKind::Temp));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::UnOp(UnOp(op_kind, *op), e_op),
                        },
                    ),
                });

                Some((mapped_ty, Operand::Move(Place::from_local(local_id))))
            }
            ExprKind::Cast(expr, ty) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, e_op) = self.pop();

                let from = self.get_tyir(e_ty);
                let to_tyid = self.get_tyid((ty.as_ref(), &self.generic_scope));
                let to_tyir = self.get_tyir(to_tyid);

                use ecsl_gir::expr::{Expr, ExprKind};
                let expr = match (from, to_tyir) {
                    (TyIr::Int, TyIr::Float) => {
                        ExprKind::Cast(e_op, OperandKind::Int, OperandKind::Float)
                    }
                    (TyIr::Float, TyIr::Int) => {
                        ExprKind::Cast(e_op, OperandKind::Float, OperandKind::Int)
                    }
                    (l, r) if l == r => {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::RedundantCast)
                                .with_span(|_| e.span),
                        );
                        return VisitorCF::Break;
                    }
                    (_, _) => {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::InvalidCast)
                                .with_span(|_| e.span),
                        );
                        return VisitorCF::Break;
                    }
                };

                let local_id =
                    self.new_local(Local::new(e.span, Mutable::Imm, to_tyid, LocalKind::Temp));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id),
                        Expr {
                            span: e.span,
                            kind: expr,
                        },
                    ),
                });

                Some((to_tyid, Operand::Move(Place::from_local(local_id))))
            }
            ExprKind::Range(_, _, _) => {
                panic!()
            }
            ExprKind::Struct(ty, fields) => {
                let mut struct_tyid = self.get_tyid((ty.as_ref(), &self.generic_scope));
                let TyIr::ADT(mut struct_tyir) = self.get_tyir(struct_tyid) else {
                    err!(TyCheckError::TypeIsNotAStruct, e.span);
                };
                err_if!(
                    !struct_tyir.is_struct(),
                    TyCheckError::TypeIsNotAStruct,
                    e.span
                );

                let generics = ty.generics.as_ref();
                if generics.map(|ty| ty.params.len()) != struct_tyir.generics {
                    err!(TyCheckError::InvalidGenericTypes, e.span);
                }

                if let Some(generics) = generics {
                    let mut tyids = Vec::new();
                    for p in &generics.params {
                        tyids.push(self.get_tyid((p, &self.generic_scope)));
                    }

                    struct_tyid = self.ty_ctxt.global.get_mono_variant(struct_tyid, &tyids);
                    struct_tyir = self.get_tyir(struct_tyid).into_adt().unwrap()
                }

                let local_id = self.new_local(Local::new(
                    e.span,
                    Mutable::Imm,
                    struct_tyid,
                    LocalKind::Internal,
                ));

                let variant = struct_tyir.get_struct_fields();

                let mut field_set = BTreeSet::new();
                for field in fields {
                    let symbol = self.ty_ctxt.table.get_symbol(field.ident).unwrap();

                    let Some(field_id) = variant.field_hash.get(&symbol.name) else {
                        err!(TyCheckError::NoMemberWithName, field.span);
                    };

                    // Insert field into set
                    err_if!(
                        !field_set.insert(field_id),
                        TyCheckError::DuplicateMember,
                        field.span
                    );

                    self.visit_expr(&field.expr)?;
                    let (expr_tyid, expr_op) = self.pop();

                    let field_def = variant.field_tys.get(field_id).unwrap();
                    unify!(
                        expr_tyid,
                        field_def.ty,
                        TyCheckError::LHSMatchRHS,
                        field.span
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: field.span,
                        kind: gir::StmtKind::Assign(
                            Place::from_local(local_id).with_projection(gir::Projection::Field {
                                ty: struct_tyid,
                                vid: variant.id,
                                fid: *field_id,
                                new_ty: field_def.ty,
                            }),
                            gir::Expr {
                                span: e.span,
                                kind: gir::ExprKind::Value(expr_op),
                            },
                        ),
                    });
                }

                // Check all fields have been defined
                err_if!(
                    field_set.len() != variant.field_tys.len(),
                    TyCheckError::MissingFields,
                    e.span
                );

                Some((struct_tyid, Operand::Move(Place::from_local(local_id))))
            }
            ExprKind::Field(expr, symbol_id) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, mut e_op) = self.pop();

                // Check not a literal

                err_if!(
                    matches!(e_op, Operand::Constant(_)),
                    TyCheckError::DotSyntaxOnConst,
                    e.span
                );

                // Get struct TyIR
                let TyIr::ADT(struct_tyir) = self.get_tyir(e_ty) else {
                    err!(TyCheckError::TypeIsNotAStruct, e.span);
                };
                err_if!(
                    !struct_tyir.is_struct(),
                    TyCheckError::TypeIsNotAStruct,
                    e.span
                );

                // Get the structs fields
                let variant = struct_tyir.get_struct_fields();

                // Get the field name and id
                let symbol = self.ty_ctxt.table.get_symbol(*symbol_id).unwrap();
                let Some(field_id) = variant.field_hash.get(&symbol.name) else {
                    err!(TyCheckError::NoMemberWithName, e.span);
                };

                // Get the field definition and create a projection to it
                let field_def = variant.field_tys.get(field_id).unwrap();
                match &mut e_op {
                    Operand::Copy(place) | Operand::Move(place) => {
                        place.with_projection_ref(gir::Projection::Field {
                            ty: e_ty,
                            fid: *field_id,
                            vid: variant.id,
                            new_ty: field_def.ty,
                        });
                    }
                    _ => unreachable!(),
                }

                Some((field_def.ty, e_op))
            }
            ExprKind::Enum(ty, variant, fields) => {
                let mut enum_tyid = self.get_tyid((ty.as_ref(), &self.generic_scope));
                let TyIr::ADT(mut enum_tyir) = self.get_tyir(enum_tyid) else {
                    err!(TyCheckError::TypeIsNotAnEnum, e.span);
                };
                err_if!(!enum_tyir.is_enum(), TyCheckError::TypeIsNotAnEnum, e.span);

                let generics = ty.generics.as_ref();
                if generics.map(|ty| ty.params.len()) != enum_tyir.generics {
                    err!(TyCheckError::InvalidGenericTypes, e.span);
                }

                if let Some(generics) = generics {
                    let mut tyids = Vec::new();
                    for p in &generics.params {
                        tyids.push(self.get_tyid((p, &self.generic_scope)));
                    }

                    enum_tyid = self.ty_ctxt.global.get_mono_variant(enum_tyid, &tyids);
                    enum_tyir = self.get_tyir(enum_tyid).into_adt().unwrap()
                }

                let local_id = self.new_local(Local::new(
                    e.span,
                    Mutable::Imm,
                    enum_tyid,
                    LocalKind::Internal,
                ));

                // Get variant name and id and get the definition
                let symbol = self.ty_ctxt.table.get_symbol(*variant).unwrap();
                let Some(var_id) = enum_tyir.variant_hash.get(&symbol.name) else {
                    err!(TyCheckError::NoVariantWithName, e.span);
                };
                let variant_def = enum_tyir.variant_kinds.get(var_id).unwrap();

                // Store the discriminant
                let discriminant = enum_tyir.discriminant_size().unwrap();
                let disc_const = match discriminant {
                    1 => Immediate::UByte(var_id.inner() as u8),
                    _ => {
                        err!(TyCheckError::TooManyVariants, e.span);
                    }
                };
                let disc_const = self.new_constant(Constant::Internal { imm: disc_const });

                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id)
                            .with_projection(Projection::Discriminant { tyid: enum_tyid }),
                        gir::Expr {
                            span: e.span,
                            kind: gir::ExprKind::Value(Operand::Constant(disc_const)),
                        },
                    ),
                });

                let mut field_set = BTreeSet::new();
                for field in fields {
                    let symbol = self.ty_ctxt.table.get_symbol(field.ident).unwrap();
                    let Some(field_id) = variant_def.field_hash.get(&symbol.name) else {
                        err!(TyCheckError::NoMemberWithName, field.span);
                    };

                    // Insert field into set
                    err_if!(
                        !field_set.insert(field_id),
                        TyCheckError::DuplicateMember,
                        field.span
                    );

                    self.visit_expr(&field.expr)?;
                    let (expr_tyid, expr_op) = self.pop();

                    let field_def = variant_def.field_tys.get(field_id).unwrap();
                    unify!(
                        expr_tyid,
                        field_def.ty,
                        TyCheckError::LHSMatchRHS,
                        field.span
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: field.span,
                        kind: gir::StmtKind::Assign(
                            Place::from_local(local_id).with_projection(gir::Projection::Field {
                                ty: enum_tyid,
                                vid: *var_id,
                                fid: *field_id,
                                new_ty: field_def.ty,
                            }),
                            gir::Expr {
                                span: e.span,
                                kind: gir::ExprKind::Value(expr_op),
                            },
                        ),
                    });
                }

                // Check all fields have been defined
                err_if!(
                    field_set.len() != variant_def.field_tys.len(),
                    TyCheckError::MissingFields,
                    e.span
                );

                Some((enum_tyid, Operand::Move(Place::from_local(local_id))))
            }
            e => panic!("{:?}", e),
            // ExprKind::Ref(mutable, expr) => todo!(),
            // ExprKind::Array(exprs) => todo!(),
            // ExprKind::MethodSelf => todo!(),
            // ExprKind::Entity => todo!(),
            // ExprKind::Resource => todo!(),
            // ExprKind::Query(query_expr) => todo!(),
            // ExprKind::Schedule(schedule) => todo!(),
        };

        if let Some((ty, op)) = ret_ty {
            self.push(ty, op);
            VisitorCF::Continue
        } else {
            VisitorCF::Break
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyCheckError {
    SymbolDoesntExistBytecode,
    SymbolDoesntExist,
    FunctionDoesntExist,
    IncorrectFunctionArguments,
    SymbolIsNotFunction,
    SymbolRedefined,
    ExpectedBoolean,
    InvalidOperation,
    InvalidCast,
    TypeIsNotAStruct,
    TypeIsNotAnEnum,
    RedundantCast,
    AssignWrongType,
    CannotAssignToLHS,
    FunctionReturnType,
    ForLoopIterator,
    RangeMustEqual,
    LHSMatchRHS,
    NoMemberWithName,
    DuplicateMember,
    MissingFields,
    DotSyntaxOnConst,
    NoVariantWithName,
    TooManyVariants,
    TypeCannotBeMatched,
    MatchNotExhaustive,
    MatchArmDuplicate,
    MatchArmUnreachable,

    InvalidGenericTypes,
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
            TyCheckError::IncorrectFunctionArguments => "Arguments do not match function signature",
            TyCheckError::SymbolDoesntExistBytecode => {
                "Symbol specified in bytecode does not exist"
            }
            TyCheckError::InvalidCast => "Cannot perform cast between types",
            TyCheckError::RedundantCast => "Cast is redundant",
            TyCheckError::TypeIsNotAStruct => "Type is not a struct",
            TyCheckError::TypeIsNotAnEnum => "Type is not an enum",
            TyCheckError::NoMemberWithName => "Struct does not have a member with name",
            TyCheckError::DuplicateMember => "Member has already been defined",
            TyCheckError::MissingFields => "Not all fields have been defined",
            TyCheckError::DotSyntaxOnConst => "Dot syntax on literal not allowed",
            TyCheckError::NoVariantWithName => "Could not find variant with name",
            TyCheckError::TooManyVariants => "Variant count unsupported",
            TyCheckError::TypeCannotBeMatched => "Type cannot be matched upon",
            TyCheckError::MatchNotExhaustive => "Match is not exhaustive",
            TyCheckError::MatchArmUnreachable => "Default case makes arm unreachable",
            TyCheckError::MatchArmDuplicate => "Variant already exists",
            TyCheckError::CannotAssignToLHS => "Cannot assign to LHS",
            TyCheckError::InvalidGenericTypes => {
                "Generics of caller do not match generics of callee"
            }
        };
        write!(f, "{}", s)
    }
}
