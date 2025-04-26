#![feature(if_let_guard)]
#![feature(macro_metavar_expr_concat)]
#![feature(array_windows)]
use ecsl_ast::ecs::{FilterKind, Schedule, ScheduleKind};
use ecsl_ast::expr::{BinOpKind, UnOpKind};
use ecsl_ast::item::{ImplBlock, Item, ItemKind};
use ecsl_ast::parse::{AttributeMarker, FnKind, Immediate, ParamKind};
use ecsl_ast::stmt::InlineBytecode;
use ecsl_ast::ty::Mutable;
use ecsl_ast::SourceAST;
use ecsl_ast::{
    expr::{Expr, ExprKind},
    parse::FnDef,
    stmt::{Block, Stmt, StmtKind},
    visit::{walk_block, Visitor, VisitorCF},
};
use ecsl_bytecode::BuiltinOp;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::cons::Constant;
use ecsl_gir::expr::{BinOp, Operand, OperandKind, UnOp};
use ecsl_gir::term::{SwitchCase, Terminator, TerminatorKind};
use ecsl_gir::{Local, LocalKind, Place, Projection, GIR};
use ecsl_gir_pass::linker::FunctionLinker;
use ecsl_index::{BlockID, ConstID, FieldID, LocalID, SymbolID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use ecsl_ty::local::LocalTyCtxt;
use ecsl_ty::{ADTDef, FieldDef, FnParent, GenericsScope, TyIr};
use ext::IntoTyID;
use for_loop::ForLoopKind;
use log::debug;
use lrpar::Span;
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
pub mod for_loop;

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

    break_continue_stack: Vec<BreakContinue>,
}

#[allow(unused)]
enum TerminationKind {
    /// Decrease depth
    Lower,
    Equal,
    /// Increase depth aka more indentation
    Higher,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakContinue {
    pub br: BlockID,
    pub co: Option<BlockID>,
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
            break_continue_stack: Default::default(),
        }
    }

    pub fn get_tyir(&self, t: impl IntoTyID) -> TyIr {
        self.ty_ctxt
            .global
            .get_tyir(t.into_tyid(&self.ty_ctxt).unwrap_or(TyID::UNKNOWN))
    }

    pub fn get_tyid(&self, t: impl IntoTyID) -> TyID {
        t.into_tyid(&self.ty_ctxt).unwrap_or(TyID::UNKNOWN)
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

    pub fn block(&mut self, block: BlockID) -> &gir::Block {
        self.cur_gir.as_ref().unwrap().get_block(block).unwrap()
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

    pub fn insert_symbol(&mut self, symbol_id: SymbolID, place: Place, span: Span) -> VisitorCF {
        if let Some(_) = self.symbols.last_mut().unwrap().insert(symbol_id, place) {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::SymbolRedefined)
                    .with_span(|_| span),
            );
            return VisitorCF::Break;
        }
        return VisitorCF::Continue;
    }

    pub fn add_break(&mut self, br: BreakContinue) {
        self.break_continue_stack.push(br);
    }

    /// Get the current break point
    pub fn break_point(&self) -> Option<BlockID> {
        self.break_continue_stack.last().map(|s| s.br)
    }

    /// Get the current continue point
    pub fn continue_point(&self) -> Option<BlockID> {
        for br_co in self.break_continue_stack.iter().rev() {
            if let Some(co) = br_co.co {
                return Some(co);
            }
        }
        return None;
    }

    fn process_fn(&mut self, f: &FnDef, scope: Option<SymbolID>) -> VisitorCF {
        let pop_generics = self.generic_scope.add_opt(f.generics.clone());

        // Create GIR for Fn
        let tyid = self.get_tyid((scope, f.ident));
        debug!("Fn {:?}", tyid);

        // Get tyir
        let TyIr::Fn(fn_tyir) = self.get_tyir((scope, f.ident)) else {
            panic!("Internal Compiler Error: Fn {} has not been defined", tyid);
        };

        self.cur_gir = Some(GIR::new(tyid, fn_tyir.kind, self.ty_ctxt.file, f.span));

        // Insert Return Type as Local
        self.new_local(Local::new(
            f.ret_span(),
            Mutable::Mut,
            self.ty_ctxt
                .get_mono_variant(fn_tyir.ret.ty, &fn_tyir.ret.params, f.span)
                .unwrap(),
            LocalKind::Ret,
        ));

        // Create symbols for function
        self.symbols.push(Default::default());

        // Insert Params as locals
        for (i, param) in f.params.iter().enumerate() {
            let (ident, mutable) = match &param.kind {
                ParamKind::SelfValue(m, s)
                | ParamKind::SelfReference(m, s)
                | ParamKind::Normal(m, s, _) => (*s, *m),
            };

            let fid = FieldID::new(i);
            let tyir_param = &fn_tyir.params[&fid];
            let local_id = self.new_local(Local::new(
                param.span,
                mutable,
                self.ty_ctxt
                    .get_mono_variant(tyir_param.ty, &tyir_param.params, param.span)
                    .unwrap(),
                LocalKind::Arg,
            ));

            let place = Place::from_local(local_id, param.span);

            self.insert_symbol(ident, place, param.span);
        }

        // Make block and iter
        self.new_block();
        walk_block(self, &f.block)?;

        // TODO: If block is terminated in bytecode then 2 RET instructions inserted
        let block = self.block_mut(self.cur_block());
        if !block.terminated() && fn_tyir.ret.ty == TyID::BOTTOM {
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

        if pop_generics {
            self.generic_scope.pop();
        }

        VisitorCF::Continue
    }

    /// Make a reference to an expression
    pub fn reference_expr(
        &mut self,
        mutable: Mutable,
        ty: TyID,
        op: Operand,
        span: Span,
    ) -> Option<(TyID, Operand)> {
        // Get the place we are referencing
        let place_to_reference = match op {
            Operand::Copy(place) | Operand::Move(place) => place,
            Operand::Constant(_) => {
                self.ty_ctxt.diag.push_error(
                    EcslError::new(ErrorLevel::Error, TyCheckError::CannotReference)
                        .with_span(|_| span),
                );
                return None;
            }
        };

        // Project the Tyir to ensure we dont have multiple layers of references
        let projected_tyir = self.get_tyir(place_to_reference.projected_tyid(self.cur_gir()));
        if let TyIr::Ref(_, _) = projected_tyir {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::CannotDoubleReference)
                    .with_span(|_| span),
            );
        }

        // Test that the local can be referenced
        // Aka the local is not temp
        let local = self.cur_gir().get_local(place_to_reference.local);
        if !local.kind.can_reference() {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::CannotReference)
                    .with_span(|_| span),
            );
            return None;
        }

        // Get the new TyID for the ref type
        let ref_tyid = self.ty_ctxt.global.tyid_from_tyir(TyIr::Ref(
            mutable,
            FieldDef {
                id: FieldID::ZERO,
                ty,
                params: Vec::new(),
            },
        ));

        // Create a local with the tyid
        let local = self.new_local(Local::new(
            span,
            Mutable::Imm,
            ref_tyid,
            LocalKind::Internal,
        ));

        // Create a place from the local and emit a gir stmt to create the refrence
        let place_that_the_reference_is = Place::from_local(local, span);
        self.push_stmt_to_cur_block(gir::Stmt {
            span,
            kind: gir::StmtKind::Assign(
                place_that_the_reference_is.clone(),
                gir::Expr {
                    span,
                    kind: gir::ExprKind::Reference(mutable, place_to_reference),
                },
            ),
        });

        Some((ref_tyid, Operand::Copy(place_that_the_reference_is.clone())))
    }
}

impl Visitor for TyCheck {
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        match &i.kind {
            ItemKind::Fn(fn_def) => self.process_fn(fn_def, None)?,
            ItemKind::Impl(impl_def) => self.visit_impl(&impl_def)?,
            _ => (),
        }
        VisitorCF::Continue
    }

    fn visit_impl(&mut self, i: &ImplBlock) -> VisitorCF {
        debug!("START OF IMPL");
        let pop_generics = self.generic_scope.add_opt(i.generics.clone());

        let scope = i.ty.into_scope().unwrap();

        for f in i.fn_defs.iter() {
            _ = self.process_fn(f, Some(scope));
        }

        if pop_generics {
            self.generic_scope.pop();
        }

        VisitorCF::Continue
    }

    fn visit_block(&mut self, _: &Block) -> VisitorCF {
        panic!("Dont use this function")
    }

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        err_macros!(self.ty_ctxt, s.span);

        if self.block(self.cur_block()).terminated() {
            self.ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, TyCheckError::DeadCode).with_span(|_| s.span),
            );
            return VisitorCF::Break;
        }
        let span = s.span;
        match &s.kind {
            StmtKind::Semi => {
                self.ty_ctxt.diag.push_error(
                    EcslError::new(ErrorLevel::Warning, TyCheckError::TrailingSemi)
                        .with_span(|_| s.span),
                );
            }
            StmtKind::Let(mutable, symbol_id, span, ty, expr) => {
                // Visit expression
                self.visit_expr(expr)?;
                let (rhs_ty, rhs_op) = self.pop();
                catch_unknown!(rhs_ty, TyCheckError::UnknownTy);

                // Unify Types
                if let Some(ty) = ty {
                    debug!("{:?} {:?}", ty, self.generic_scope);
                    let let_ty = catch_unknown!(
                        self.get_tyid((ty.as_ref(), &self.generic_scope)),
                        TyCheckError::UnknownTy
                    );
                    unify!(
                        let_ty,
                        rhs_ty,
                        TyCheckError::AssignWrongType {
                            from: rhs_ty,
                            to: let_ty
                        }
                    );
                }

                let local_id = match &rhs_op {
                    Operand::Copy(place) | Operand::Move(place) => {
                        let local = self.get_local_mut(place.local);

                        if local.kind.promote_to_let() {
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
                                    Place::from_local(local_id, *span),
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
                                Place::from_local(local_id, *span),
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

                // Insert Symbol
                self.insert_symbol(*symbol_id, Place::from_local(local_id, *span), s.span)?;
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
                            unify!(
                                cond_ty,
                                bool_ty,
                                TyCheckError::ExpectedBoolean(cond_ty),
                                expr.span
                            );

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

                    unify!(
                        ty,
                        return_ty,
                        TyCheckError::FunctionReturnType {
                            from: ty,
                            to: return_ty
                        }
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: s.span,
                        kind: gir::StmtKind::Assign(
                            Place::return_location(span),
                            gir::Expr {
                                span: expr.span,
                                kind: gir::ExprKind::Value(op),
                            },
                        ),
                    })
                } else {
                    unify!(
                        TyID::BOTTOM,
                        return_ty,
                        TyCheckError::FunctionReturnType {
                            from: TyID::BOTTOM,
                            to: return_ty
                        }
                    );
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
                        } else if let Immediate::Builtin(op, symbol) = i {
                            use ecsl_ast::ty::*;
                            let tyid = catch_unknown!(
                                self.get_tyid((
                                    &Ty::new(
                                        *span,
                                        TyKind::Ident(*symbol),
                                        ConcreteGenerics::empty(*span)
                                    ),
                                    &self.generic_scope
                                )),
                                TyCheckError::UnknownTy
                            );

                            *i = match op {
                                BuiltinOp::CID => Immediate::ComponentOf(tyid),
                                BuiltinOp::Size => Immediate::SizeOf(tyid, 0),
                                BuiltinOp::SizeAdd1 => Immediate::SizeOf(tyid, 1),
                                _ => {
                                    self.ty_ctxt.diag.push_error(
                                        EcslError::new(
                                            ErrorLevel::Error,
                                            TyCheckError::UnknownBuiltinOp,
                                        )
                                        .with_span(|_| *span),
                                    );
                                    return VisitorCF::Break;
                                }
                            }
                        }
                    }

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: *span,
                        kind: gir::StmtKind::BYT(ins.clone()),
                    })
                }
            }
            StmtKind::For(symbol_id, ty, expr, block) => {
                // Create for loop kind
                let mut for_loop_kind_opt = None;
                ForLoopKind::from_expr(self, expr, &mut for_loop_kind_opt)?;
                let mut for_loop_kind = for_loop_kind_opt.unwrap();

                // Get For loop iterator Ty
                if let Some(ty) = ty {
                    let iterator_tyid = catch_unknown!(
                        self.get_tyid((ty.as_ref(), &self.generic_scope)),
                        TyCheckError::UnknownTy
                    );
                    unify!(
                        iterator_tyid,
                        for_loop_kind.get_iterator_type(),
                        TyCheckError::ForLoopIterator {
                            index: iterator_tyid,
                            range: for_loop_kind.get_iterator_type()
                        }
                    );
                }

                // Store symbols len for assert
                let symbols_len = self.symbols.len();

                // Setup initial things
                let iterator_local_id = for_loop_kind.create_iterator_local(self, s.span);

                // Jump to next block
                let condition_block =
                    self.terminate_with_new_block(TerminationKind::Higher, |block, next| {
                        block.terminate(Terminator {
                            kind: TerminatorKind::Jump(next),
                        })
                    });

                // Insert Symbol
                self.insert_symbol(
                    *symbol_id,
                    Place::from_local(iterator_local_id, span),
                    s.span,
                )?;

                // Create the condition block returning the leave block
                let leave_block = for_loop_kind.create_comparison(self, iterator_local_id, s.span);

                // Creat the increment block
                let increment_block = self.new_block_without_stack();

                self.add_break(BreakContinue {
                    br: leave_block,
                    co: Some(increment_block),
                });

                // Walk the loop block and terminate with the increment block
                walk_block(self, block)?;
                self.terminate_with_existing_block(
                    TerminationKind::Lower,
                    increment_block,
                    |block, next| {
                        block.terminate_no_replace(Terminator {
                            kind: TerminatorKind::Jump(next),
                        });
                    },
                );

                // Fill out the increment block
                for_loop_kind.create_increment(self, iterator_local_id, condition_block, span);

                self.block_stack.push(leave_block);

                self.break_continue_stack.pop();

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
                    err!(TyCheckError::TypeCannotBeMatched(ty), expr.span);
                };
                err_if!(
                    !enum_tyir.is_enum(),
                    TyCheckError::TypeCannotBeMatched(ty),
                    expr.span
                );

                let place = match op {
                    Operand::Copy(place) | Operand::Move(place) => {
                        let local = self.get_local_mut(place.local);
                        local.kind.promote_from_temp(LocalKind::Internal);
                        place
                    }
                    _ => panic!("Interal Compiler Error: Enum cannot be const"),
                };

                let current_block = self.cur_block();
                let out_block = self.new_block_without_stack();

                self.break_continue_stack.push(BreakContinue {
                    br: out_block,
                    co: None,
                });

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
                            err!(TyCheckError::NoMemberWithName(ty), arm.span);
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
                                err!(TyCheckError::NoMemberWithName(ty), arm.span);
                            };

                            err_if!(
                                !fields.insert(field_id),
                                TyCheckError::DuplicateMember,
                                arm.span
                            );
                            let field_def = var.field_tys.get(field_id).unwrap();

                            // Insert Ident into local Mapping
                            self.insert_symbol(
                                field.ident,
                                place.clone().with_projection(Projection::Field {
                                    ty,
                                    vid: *var_id,
                                    fid: *field_id,
                                    new_ty: field_def.ty,
                                }),
                                arm.span,
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
                    TyCheckError::MatchNotExhaustive(ty),
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
                        Operand::Copy(place.with_projection(Projection::Discriminant { tyid: ty })),
                        cases,
                    ),
                });

                self.break_continue_stack.pop();
            }
            StmtKind::While(expr, block) => {
                // Terminate block with block for expr
                let expr_block =
                    self.terminate_with_new_block(TerminationKind::Higher, |block, id| {
                        block.terminate(Terminator {
                            kind: TerminatorKind::Jump(id),
                        });
                    });

                // Unify Condition
                self.visit_expr(expr.as_ref())?;
                let (cond_ty, cond_op) = self.pop();
                unify!(
                    cond_ty,
                    self.get_tyid(TyIr::Bool),
                    TyCheckError::ExpectedBoolean(cond_ty),
                    expr.span
                );

                // Create the next block
                let to_next = self.new_block_without_stack();

                // Push break continue to stack
                self.break_continue_stack.push(BreakContinue {
                    br: to_next,
                    co: Some(expr_block),
                });

                // Walk if block
                let start_of_block = self.new_block();
                walk_block(self, block)?;
                let end_of_block = self.pop_block();

                // Terminate expr block with jump
                self.block_mut(expr_block).terminate(Terminator {
                    kind: TerminatorKind::Switch(
                        cond_op,
                        vec![
                            SwitchCase::Value(Immediate::Bool(true), start_of_block),
                            SwitchCase::Default(to_next),
                        ],
                    ),
                });

                // Jump back to expr block
                self.block_mut(end_of_block)
                    .terminate_no_replace(Terminator {
                        kind: TerminatorKind::Jump(expr_block),
                    });

                self.push_block_stack(to_next);

                // Pop break continue stack
                self.break_continue_stack.pop();
            }
            StmtKind::Break => {
                if let Some(br) = self.break_point() {
                    self.block_mut(self.cur_block()).terminate(Terminator {
                        kind: TerminatorKind::Jump(br),
                    });
                } else {
                    err!(TyCheckError::NoBreak, s.span);
                }
            }
            StmtKind::Continue => {
                if let Some(co) = self.continue_point() {
                    self.block_mut(self.cur_block()).terminate(Terminator {
                        kind: TerminatorKind::Jump(co),
                    });
                } else {
                    err!(TyCheckError::NoBreak, s.span);
                }
            }
        }

        VisitorCF::Continue
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        err_macros!(self.ty_ctxt, e.span);

        let span = e.span;
        let ret_ty = match &e.kind {
            ExprKind::Assign(lhs, span, rhs) => {
                // Visit lhs
                self.visit_expr(lhs)?;
                let (_, lhs_op) = self.pop();

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
                    LocalKind::Internal => true,
                    LocalKind::Ret => panic!("Internal Compiler Error"),
                };
                err_if!(!can_assign, TyCheckError::CannotAssignToLHS, lhs.span);

                // Visit rhs
                self.visit_expr(rhs)?;
                let (rhs_ty, rhs_op) = self.pop();

                let local_tyid = lhs_place.projected_tyid(self.cur_gir());

                unify!(
                    rhs_ty,
                    local_tyid,
                    TyCheckError::AssignWrongType {
                        from: rhs_ty,
                        to: local_tyid
                    },
                    *span
                );

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
            ExprKind::Ident(ident) | ExprKind::MethodSelf(ident) => {
                // Search all symbols
                let found = self.find_symbol(*ident);

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
                let tyid =
                    catch_unknown!(self.get_tyid(TyIr::from(*literal)), TyCheckError::UnknownTy);
                let const_id = self.new_constant(Constant::External {
                    span: e.span,
                    tyid,
                    kind: *literal,
                });

                Some((tyid, Operand::Constant(const_id)))
            }
            ExprKind::StaticFunction(scope, symbol_id, generics, exprs) => {
                let mut tyid = catch_unknown!(
                    self.get_tyid((Some(*scope), *symbol_id)),
                    TyCheckError::FunctionDoesntExist
                );

                let Some(fn_tyir) = self.get_tyir(tyid).into_fn() else {
                    err!(TyCheckError::FunctionDoesntExist, e.span);
                };

                let mut exprs_tys = Vec::new();

                let fn_tyir = if fn_tyir.total_generics > 0 {
                    let params = generics
                        .params
                        .iter()
                        .map(|ty| self.get_tyid((ty, &self.generic_scope)))
                        .collect::<Vec<_>>();

                    // If the fn is marked as requiring comp generics then enforce that
                    if fn_tyir
                        .attributes
                        .get_marker(AttributeMarker::RequireCompGenerics)
                    {
                        for tyid in params.iter() {
                            err_if!(
                                !self.ty_ctxt.global.is_comp(*tyid),
                                TyCheckError::RequiresComp(*tyid),
                                e.span
                            );

                            self.cur_gir_mut().require_component(*tyid);
                        }
                    }

                    for p in params.iter() {
                        _ = catch_unknown!(*p, TyCheckError::UnknownTy);
                    }

                    tyid = catch_unknown!(
                        self.ty_ctxt
                            .get_mono_variant(tyid, &params, generics.span)
                            .unwrap(),
                        TyCheckError::FunctionDoesntExist
                    );
                    self.get_tyir(tyid).into_fn().unwrap()
                } else {
                    fn_tyir
                };

                let ret_ty = fn_tyir.ret.ty;
                // Preallocate the size of the return value on the stack
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::AllocReturn(ret_ty),
                });

                // Iter over all expressions
                for expr in exprs {
                    self.visit_expr(expr)?;
                    let (ty, op) = self.pop();
                    exprs_tys.push((ty, op, expr.span))
                }

                err_if!(
                    exprs_tys.len() != fn_tyir.params.len(),
                    TyCheckError::NoFunctionWithNArguments(exprs_tys.len()),
                    e.span
                );

                let mut operands = Vec::new();
                for ((l, op, span), (_, r)) in exprs_tys.iter().zip(&fn_tyir.params) {
                    unify!(
                        *l,
                        r.ty,
                        TyCheckError::IncorrectFunctionArguments { from: *l, to: r.ty },
                        *span
                    );
                    operands.push(op.clone());
                }

                let local_id = self.new_local(Local::new(
                    e.span,
                    Mutable::Imm,
                    ret_ty,
                    LocalKind::Internal,
                ));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id, span),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::Call(tyid, operands),
                        },
                    ),
                });

                Some((ret_ty, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::Function(parent, generics, symbol_id, exprs) => {
                let parent = match parent {
                    Some(expr) => {
                        self.visit_expr(expr)?;
                        let (ty, op) = self.pop();
                        Some((ty, op, expr.span))
                    }
                    _ => None,
                };

                // Convert from ref type or actual type and get any associated function with them
                let mut fn_tyid = match parent {
                    Some((tyid, _, span)) => match self.get_tyir(tyid) {
                        TyIr::Ref(_, field_def) => {
                            let mono_tyid = self
                                .ty_ctxt
                                .get_mono_variant(field_def.ty, &field_def.params, span)
                                .unwrap();

                            self.get_tyid((mono_tyid, *symbol_id))
                        }
                        TyIr::ADT(adt) => self.get_tyid((adt.id, *symbol_id)),
                        _ => self.get_tyid((tyid, *symbol_id)),
                    },
                    None => self.get_tyid(*symbol_id),
                };

                // Get the Tyir for the function
                let Some(fn_tyir) = self.get_tyir(fn_tyid).into_fn() else {
                    err!(TyCheckError::FunctionDoesntExist, e.span);
                };

                let fn_tyir = if fn_tyir.total_generics > 0 {
                    let mut params = Vec::new();

                    // If the parent exists and it has existing generics put them at
                    // the start of the params
                    if let Some((parent, _, _)) = &parent {
                        if let Some((_, existing_generics)) = self
                            .ty_ctxt
                            .global
                            .monos
                            .mono_map
                            .read()
                            .unwrap()
                            .get_by_right(parent)
                        {
                            params.extend(existing_generics.clone());
                        }
                    }

                    // Convert the params to tyid
                    generics
                        .params
                        .iter()
                        .map(|ty| self.get_tyid((ty, &self.generic_scope)))
                        .for_each(|ty| params.push(ty));

                    // If the fn is marked as requiring comp generics then enforce that
                    if fn_tyir
                        .attributes
                        .get_marker(AttributeMarker::RequireCompGenerics)
                    {
                        for tyid in params.iter() {
                            err_if!(
                                !self.ty_ctxt.global.is_comp(*tyid),
                                TyCheckError::RequiresComp(*tyid),
                                e.span
                            );

                            self.cur_gir_mut().require_component(*tyid);
                        }
                    }

                    // Catch unknown params
                    for p in params.iter() {
                        _ = catch_unknown!(*p, TyCheckError::UnknownTy);
                    }

                    debug!("{:?} {:?}", fn_tyid, self.ty_ctxt.global.get_tyir(fn_tyid));

                    fn_tyid = catch_unknown!(
                        self.ty_ctxt
                            .get_mono_variant(fn_tyid, &params, generics.span)
                            .unwrap(),
                        TyCheckError::FunctionDoesntExist
                    );
                    let tyir = self.get_tyir(fn_tyid).into_fn().unwrap();

                    tyir
                } else {
                    fn_tyir
                };

                let mut exprs_tys = Vec::new();
                match (parent, &fn_tyir.parent) {
                    (Some((ty, op, span)), FnParent::Value(_, ref parent)) => {
                        err_if!(
                            ty != parent.ty,
                            TyCheckError::MismatchedFunction(ty, parent.ty),
                            span
                        );
                        match &op {
                            Operand::Copy(place) | Operand::Move(place) => {
                                let local = self.get_local_mut(place.local);
                                local.kind.promote_from_temp(LocalKind::Internal);
                                place
                            }
                            _ => err!(TyCheckError::DotSyntaxOnConst, e.span),
                        };
                        exprs_tys.push((ty, op, span))
                    }
                    (Some((ty, op, span)), FnParent::Ref(mutable, parent)) => {
                        let ref_tyid = self.get_tyid(TyIr::Ref(*mutable, parent.clone()));
                        if ty == parent.ty {
                            match &op {
                                Operand::Copy(place) | Operand::Move(place) => {
                                    let local = self.get_local_mut(place.local);
                                    local.kind.promote_from_temp(LocalKind::Internal);
                                    place
                                }
                                _ => err!(TyCheckError::DotSyntaxOnConst, e.span),
                            };

                            let Some((ty, op)) = self.reference_expr(*mutable, ty, op, span) else {
                                return VisitorCF::Break;
                            };

                            exprs_tys.push((ty, op, span));
                        } else if ty == ref_tyid {
                            exprs_tys.push((ty, op, span))
                        } else {
                            err!(TyCheckError::MismatchedFunction(ty, fn_tyid), span);
                        }
                    }
                    (None, FnParent::None) => (),
                    (None, FnParent::Ref(_, ref ty) | FnParent::Value(_, ref ty)) => {
                        err!(TyCheckError::NotAFreeFunction(ty.ty), e.span);
                    }
                    (Some((ty, _, _)), FnParent::None) => {
                        err!(TyCheckError::NotAMemberFunction(ty), e.span);
                    }
                }

                let ret_ty = fn_tyir.ret.ty;
                // Preallocate the size of the return value on the stack
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::AllocReturn(ret_ty),
                });

                // Iter over all expressions
                for expr in exprs {
                    self.visit_expr(expr)?;
                    let (ty, op) = self.pop();
                    exprs_tys.push((ty, op, expr.span))
                }

                err_if!(
                    exprs_tys.len() != fn_tyir.params.len(),
                    TyCheckError::NoFunctionWithNArguments(exprs_tys.len()),
                    e.span
                );

                let mut operands = Vec::new();
                for ((l, op, span), (_, r)) in exprs_tys.iter().zip(&fn_tyir.params) {
                    unify!(
                        *l,
                        r.ty,
                        TyCheckError::IncorrectFunctionArguments { from: *l, to: r.ty },
                        *span
                    );
                    operands.push(op.clone());
                }

                let local_id = self.new_local(Local::new(
                    e.span,
                    Mutable::Imm,
                    ret_ty,
                    LocalKind::Internal,
                ));

                // Create Assignment Stmt
                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id, span),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::Call(fn_tyid, operands),
                        },
                    ),
                });

                Some((ret_ty, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // Visit LHS
                self.visit_expr(lhs)?;
                let (lhs_ty, lhs_op) = self.pop();

                // Visit RHS
                self.visit_expr(rhs)?;
                let (rhs_ty, rhs_op) = self.pop();

                // Unify Types
                unify!(lhs_ty, rhs_ty, TyCheckError::LHSMatchRHS(lhs_ty, rhs_ty));

                let int = self.get_tyid(TyIr::Int) == lhs_ty;
                let float = self.get_tyid(TyIr::Float) == lhs_ty;
                let boolean = self.get_tyid(TyIr::Bool) == lhs_ty;

                let tyid = if (op.int_operation() && int) || (op.float_operation() && float) {
                    lhs_ty
                } else if op.comparsion() && (int || float || boolean) {
                    self.get_tyid(TyIr::Bool)
                } else if op.boolean_logic() && boolean {
                    self.get_tyid(TyIr::Bool)
                } else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(
                            ErrorLevel::Error,
                            TyCheckError::InvalidBinOp(*op, lhs_ty, rhs_ty),
                        )
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
                        Place::from_local(local_id, span),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::BinOp(BinOp(op_kind, *op), lhs_op, rhs_op),
                        },
                    ),
                });

                Some((tyid, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::UnOp(op @ (UnOpKind::Neg | UnOpKind::Not), expr) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, e_op) = self.pop();

                let bool_ty = self.get_tyid(TyIr::Bool);
                let mapped_ty = match op {
                    UnOpKind::Not if e_ty == bool_ty => e_ty,
                    UnOpKind::Neg if self.global().is_numeric(e_ty) => e_ty,
                    _ => {
                        self.ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, TyCheckError::InvalidUnOp(*op, e_ty))
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
                        Place::from_local(local_id, span),
                        gir::Expr {
                            span: e.span, //TODO: Replace Span
                            kind: gir::ExprKind::UnOp(UnOp(op_kind, *op), e_op),
                        },
                    ),
                });

                Some((mapped_ty, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::UnOp(UnOpKind::Deref, expr) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, e_op) = self.pop();

                let tyir = self.get_tyir(e_ty);
                let deref_tyid = if let TyIr::Ref(_, ty_id) = tyir {
                    ty_id
                } else {
                    self.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, TyCheckError::CannotDeref(e_ty))
                            .with_span(|_| e.span),
                    );
                    return VisitorCF::Break;
                };

                let place = match e_op {
                    Operand::Copy(place) | Operand::Move(place) => place,
                    Operand::Constant(_) => panic!(),
                };

                Some((
                    deref_tyid.ty,
                    Operand::Copy(place.with_projection(Projection::Deref {
                        new_ty: deref_tyid.ty,
                    })),
                ))
            }
            ExprKind::Cast(expr, ty) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (e_ty, e_op) = self.pop();

                let from = self.get_tyir(e_ty);
                let to_tyid = catch_unknown!(
                    self.get_tyid((ty.as_ref(), &self.generic_scope)),
                    TyCheckError::UnknownTy
                );
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
                            EcslError::new(
                                ErrorLevel::Error,
                                TyCheckError::InvalidCast {
                                    from: e_ty,
                                    to: to_tyid,
                                },
                            )
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
                        Place::from_local(local_id, span),
                        Expr {
                            span: e.span,
                            kind: expr,
                        },
                    ),
                });

                Some((to_tyid, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::Range(_, _, _) => {
                panic!("Internal Compiler Error: Range Expr Ty Check")
            }
            ExprKind::Struct(ty, fields) => {
                let struct_tyid = catch_unknown!(
                    self.get_tyid((ty.as_ref(), &self.generic_scope)),
                    TyCheckError::UnknownTy
                );
                let TyIr::ADT(struct_tyir) = self.get_tyir(struct_tyid) else {
                    err!(TyCheckError::TypeIsNotAStruct, e.span);
                };
                err_if!(
                    !struct_tyir.is_struct(),
                    TyCheckError::TypeIsNotAStruct,
                    e.span
                );

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
                        err!(TyCheckError::NoMemberWithName(struct_tyid), field.span);
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
                        TyCheckError::LHSMatchRHS(expr_tyid, field_def.ty),
                        field.span
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: field.span,
                        kind: gir::StmtKind::Assign(
                            Place::from_local(local_id, span).with_projection(
                                gir::Projection::Field {
                                    ty: struct_tyid,
                                    vid: variant.id,
                                    fid: *field_id,
                                    new_ty: field_def.ty,
                                },
                            ),
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

                Some((
                    struct_tyid,
                    Operand::Move(Place::from_local(local_id, span)),
                ))
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

                let get_struct_tyir = |s: &TyCheck, tyid: TyID, tyir: TyIr| -> Option<ADTDef> {
                    match tyir {
                        TyIr::ADT(adt) if adt.is_struct() => Some(adt),
                        TyIr::Entity => todo!("Entity Fields"),
                        _ => {
                            s.ty_ctxt.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    TyCheckError::TypeHasNoFields(tyid),
                                )
                                .with_span(|_| expr.span),
                            );
                            None
                        }
                    }
                };

                let place = e_op.place_mut().unwrap();
                let tyir = self.get_tyir(e_ty);

                if let TyIr::Array(_, len) = tyir {
                    let symbol = self.ty_ctxt.table.get_symbol(*symbol_id).unwrap();
                    if symbol.name == "len" {
                        Some((
                            self.get_tyid(TyIr::Int),
                            Operand::Constant(self.new_constant(Constant::Internal {
                                span,
                                imm: Immediate::Int(len as i32),
                            })),
                        ))
                    } else {
                        err!(TyCheckError::NoMemberWithName(e_ty), e.span);
                    }
                } else {
                    let adtdef = match tyir {
                        TyIr::Ref(_, def) => {
                            place.with_projection_ref(Projection::Deref { new_ty: def.ty });
                            get_struct_tyir(self, def.ty, self.get_tyir(def.ty))
                        }
                        tyir => get_struct_tyir(self, e_ty, tyir),
                    };

                    let Some(struct_tyir) = adtdef else {
                        return VisitorCF::Break;
                    };

                    // Get the structs fields
                    let variant = struct_tyir.get_struct_fields();

                    // Get the field name and id
                    let symbol = self.ty_ctxt.table.get_symbol(*symbol_id).unwrap();
                    let Some(field_id) = variant.field_hash.get(&symbol.name) else {
                        err!(TyCheckError::NoMemberWithName(e_ty), e.span);
                    };

                    // Get the field definition and create a projection to it
                    let field_def = variant.field_tys.get(field_id).unwrap();
                    match &mut e_op {
                        Operand::Copy(place) | Operand::Move(place) => {
                            place.with_projection_ref(gir::Projection::Field {
                                ty: struct_tyir.id,
                                fid: *field_id,
                                vid: variant.id,
                                new_ty: field_def.ty,
                            });
                        }
                        _ => unreachable!(),
                    }

                    Some((field_def.ty, e_op))
                }
            }
            ExprKind::Enum(ty, variant, fields) => {
                let enum_tyid = catch_unknown!(
                    self.get_tyid((ty.as_ref(), &self.generic_scope)),
                    TyCheckError::UnknownTy
                );
                let TyIr::ADT(enum_tyir) = self.get_tyir(enum_tyid) else {
                    err!(TyCheckError::TypeIsNotAnEnum, e.span);
                };

                err_if!(!enum_tyir.is_enum(), TyCheckError::TypeIsNotAnEnum, e.span);

                let local_id = self.new_local(Local::new(
                    e.span,
                    Mutable::Imm,
                    enum_tyid,
                    LocalKind::Internal,
                ));

                // Get variant name and id and get the definition
                let symbol = self.ty_ctxt.table.get_symbol(*variant).unwrap();
                let Some(var_id) = enum_tyir.variant_hash.get(&symbol.name) else {
                    err!(TyCheckError::NoVariantWithName(enum_tyid), e.span);
                };
                let variant_def = enum_tyir.variant_kinds.get(var_id).unwrap();

                // Store the discriminant
                let discriminant = enum_tyir.discriminant_size().unwrap();
                let disc_const = match discriminant {
                    1 => Immediate::UByte(var_id.inner() as u8),
                    _ => {
                        err!(TyCheckError::TooManyVariants(enum_tyid), e.span);
                    }
                };
                let disc_const = self.new_constant(Constant::Internal {
                    span: ty.span,
                    imm: disc_const,
                });

                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        Place::from_local(local_id, span)
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
                        err!(TyCheckError::NoMemberWithName(enum_tyid), field.span);
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
                        TyCheckError::LHSMatchRHS(expr_tyid, field_def.ty),
                        field.span
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span: field.span,
                        kind: gir::StmtKind::Assign(
                            Place::from_local(local_id, span).with_projection(
                                gir::Projection::Field {
                                    ty: enum_tyid,
                                    vid: *var_id,
                                    fid: *field_id,
                                    new_ty: field_def.ty,
                                },
                            ),
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

                Some((enum_tyid, Operand::Move(Place::from_local(local_id, span))))
            }
            ExprKind::Ref(mutable, expr) => {
                // Visit expr
                self.visit_expr(expr)?;
                let (ty, op) = self.pop();
                catch_unknown!(ty, TyCheckError::UnknownTy);

                let out = self.reference_expr(*mutable, ty, op, span);
                if out.is_none() {
                    return VisitorCF::Break;
                }

                out
            }
            ExprKind::Query(query) => {
                let mut with = BTreeSet::new();
                let mut without = BTreeSet::new();

                for filter in query.filters.iter() {
                    for ty in filter.items.iter() {
                        let filter_tyid = catch_unknown!(
                            self.get_tyid((ty, &self.generic_scope)),
                            TyCheckError::UnknownTy
                        );

                        err_if!(
                            !self.ty_ctxt.global.is_comp(filter_tyid),
                            TyCheckError::RequiresComp(filter_tyid),
                            e.span
                        );

                        self.cur_gir_mut().require_component(filter_tyid);

                        match filter.kind {
                            FilterKind::With => {
                                err_if!(
                                    !with.insert(filter_tyid),
                                    TyCheckError::DuplicateFilter(filter_tyid),
                                    filter.span
                                );
                                err_if!(
                                    without.contains(&filter_tyid),
                                    TyCheckError::ConflictingFilter(filter_tyid),
                                    filter.span
                                )
                            }
                            FilterKind::Without => {
                                err_if!(
                                    !without.insert(filter_tyid),
                                    TyCheckError::DuplicateFilter(filter_tyid),
                                    filter.span
                                );
                                err_if!(
                                    with.contains(&filter_tyid),
                                    TyCheckError::ConflictingFilter(filter_tyid),
                                    filter.span
                                )
                            }
                            _ => err!(TyCheckError::UnsupportedFilterKind, filter.span),
                        }
                    }
                }

                let query_tyid = self.get_tyid(TyIr::Query);

                let const_id = self.new_constant(Constant::Query {
                    span: e.span,
                    query: gir::Query {
                        with: with.into_iter().collect(),
                        without: without.into_iter().collect(),
                    },
                });

                let local = self.new_local(Local::new(
                    span,
                    Mutable::Imm,
                    query_tyid,
                    LocalKind::Internal,
                ));

                let place = Place::from_local(local, span);

                self.push_stmt_to_cur_block(gir::Stmt {
                    span: e.span,
                    kind: gir::StmtKind::Assign(
                        place.clone(),
                        gir::Expr {
                            span: e.span,
                            kind: gir::ExprKind::Value(Operand::Constant(const_id)),
                        },
                    ),
                });

                Some((query_tyid, Operand::Copy(place)))
            }
            ExprKind::Schedule(schedule) => {
                fn process_schedule(
                    schedule: &Schedule,
                    ty_check: &mut TyCheck,
                    out: &mut Option<ConstID>,
                ) -> VisitorCF {
                    err_macros!(ty_check.ty_ctxt, schedule.span, sch_);

                    match &schedule.kind {
                        ScheduleKind::Sys(scope, symbol) => {
                            let fn_tyid = sch_catch_unknown!(
                                ty_check.get_tyid((*scope, *symbol)),
                                TyCheckError::FunctionDoesntExist
                            );

                            let Some(fn_tyir) = ty_check.get_tyir(fn_tyid).into_fn() else {
                                sch_err!(TyCheckError::FunctionDoesntExist, schedule.span);
                            };

                            sch_err_if!(fn_tyir.kind == FnKind::Fn, TyCheckError::FnInSchedule);
                            sch_err_if!(
                                fn_tyir.params.len() > 0,
                                TyCheckError::ScheduleSysNoArguments
                            );
                            sch_err_if!(
                                fn_tyir.ret.ty != TyID::BOTTOM,
                                TyCheckError::ScheduleSysNoArguments
                            );

                            let ptr = ty_check.new_constant(Constant::Internal {
                                span: schedule.span,
                                imm: Immediate::AddressOf(fn_tyid),
                            });
                            *out = Some(ptr);
                        }
                        ScheduleKind::Ordered(schedules) => {
                            sch_err_if!(schedules.is_empty(), TyCheckError::EmptySchedule);

                            let mut ordered_out =
                                schedules.iter().map(|_| None).collect::<Vec<_>>();
                            for (i, s) in schedules.iter().enumerate() {
                                process_schedule(s, ty_check, &mut ordered_out[i])?;
                            }

                            let ptr = ty_check.new_constant(Constant::Schedule {
                                kind: gir::ScheduleKind::Ordered,
                                contents: ordered_out.drain(..).map(|opt| opt.unwrap()).collect(),
                            });

                            *out = Some(ptr);
                        }
                        ScheduleKind::Unordered(schedules) => {
                            sch_err_if!(schedules.is_empty(), TyCheckError::EmptySchedule);

                            let mut unordered_out =
                                schedules.iter().map(|_| None).collect::<Vec<_>>();
                            for (i, s) in schedules.iter().enumerate() {
                                process_schedule(s, ty_check, &mut unordered_out[i])?;
                            }

                            let ptr = ty_check.new_constant(Constant::Schedule {
                                kind: gir::ScheduleKind::Unordered,
                                contents: unordered_out.drain(..).map(|opt| opt.unwrap()).collect(),
                            });

                            *out = Some(ptr);
                        }
                    }
                    VisitorCF::Continue
                }

                let mut out = None;
                process_schedule(schedule, self, &mut out)?;

                let schedule_tyid = self.get_tyid(TyIr::Schedule);

                Some((schedule_tyid, Operand::Constant(out.unwrap())))
            }
            ExprKind::Bundle(exprs) => {
                let mut bundle = BTreeMap::new();
                for e in exprs {
                    self.visit_expr(e)?;
                    let (ty, op) = self.pop();

                    err_if!(
                        !self.ty_ctxt.global.is_comp(ty),
                        TyCheckError::RequiresComp(ty),
                        e.span
                    );

                    self.cur_gir_mut().require_component(ty);

                    let out = bundle.insert(ty, (ty, op));
                    err_if!(out.is_some(), TyCheckError::DuplicateInBundle(ty))
                }

                let entity_tyid = self.get_tyid(TyIr::Entity);
                let local = self.new_local(Local::new(
                    span,
                    Mutable::Imm,
                    entity_tyid,
                    LocalKind::Internal,
                ));
                let place = Place::from_local(local, span);

                self.push_stmt_to_cur_block(gir::Stmt {
                    span,
                    kind: gir::StmtKind::Assign(
                        place.clone(),
                        gir::Expr {
                            span,
                            kind: gir::ExprKind::Bundle(bundle.into_values().collect()),
                        },
                    ),
                });

                Some((entity_tyid, Operand::Copy(place)))
            }
            ExprKind::Array(exprs) => {
                let mut out = Vec::new();
                for e in exprs {
                    self.visit_expr(e)?;
                    out.push(self.pop());
                }

                err_if!(out.len() == 0, TyCheckError::EmptyArray);
                for [(l, _), (r, _)] in out.array_windows::<2>() {
                    err_if!(l != r, TyCheckError::LHSMatchRHS(*l, *r));
                }

                let array_element = out[0].0;
                let array_ty = self.get_tyid(TyIr::Array(array_element, out.len()));
                let local = self.new_local(Local::new(
                    span,
                    Mutable::Imm,
                    array_ty,
                    LocalKind::Internal,
                ));

                for (i, (_, op)) in out.into_iter().enumerate() {
                    let place = Place::from_local(local, span).with_projection(
                        Projection::ConstArrayIndex {
                            array_element,
                            index: i,
                        },
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span,
                        kind: gir::StmtKind::Assign(
                            place,
                            gir::Expr {
                                span,
                                kind: gir::ExprKind::Value(op),
                            },
                        ),
                    });
                }

                Some((array_ty, Operand::Move(Place::from_local(local, span))))
            }
            ExprKind::ArrayRepeat(expr, len) => {
                self.visit_expr(expr)?;
                let (array_element, op) = self.pop();

                let array_ty = self.get_tyid(TyIr::Array(array_element, *len));
                let local = self.new_local(Local::new(
                    span,
                    Mutable::Imm,
                    array_ty,
                    LocalKind::Internal,
                ));

                for i in 0..*len {
                    let place = Place::from_local(local, span).with_projection(
                        Projection::ConstArrayIndex {
                            array_element,
                            index: i,
                        },
                    );

                    self.push_stmt_to_cur_block(gir::Stmt {
                        span,
                        kind: gir::StmtKind::Assign(
                            place,
                            gir::Expr {
                                span,
                                kind: gir::ExprKind::Value(op.clone()),
                            },
                        ),
                    });
                }

                Some((array_ty, Operand::Move(Place::from_local(local, span))))
            }
            ExprKind::ArrayIndex(arr, index) => {
                self.visit_expr(arr)?;
                let (arr_ty, arr_op) = self.pop();

                self.visit_expr(index)?;
                let (index_ty, index_op) = self.pop();

                let TyIr::Array(element, _) = self.get_tyir(arr_ty) else {
                    err!(TyCheckError::CannotIndex(arr_ty), arr.span)
                };
                err_if!(
                    index_ty != self.get_tyid(TyIr::Int),
                    TyCheckError::CannotIndexWith(index_ty)
                );

                let index_place = match index_op {
                    Operand::Copy(place) | Operand::Move(place) => place,
                    Operand::Constant(_) => {
                        // Create local ID
                        let local_id = self.new_local(Local::new(
                            span,
                            Mutable::Imm,
                            element,
                            LocalKind::Internal,
                        ));

                        // Create Assignment Stmt
                        self.push_stmt_to_cur_block(gir::Stmt {
                            span,
                            kind: gir::StmtKind::Assign(
                                Place::from_local(local_id, span),
                                gir::Expr {
                                    span,
                                    kind: gir::ExprKind::Value(index_op),
                                },
                            ),
                        });

                        Place::from_local(local_id, span)
                    }
                };

                let indexed =
                    self.new_local(Local::new(span, Mutable::Imm, element, LocalKind::Temp));
                let place = Place::from_local(indexed, span);

                self.push_stmt_to_cur_block(gir::Stmt {
                    span,
                    kind: gir::StmtKind::Assign(
                        place.clone(),
                        gir::Expr {
                            span,
                            kind: gir::ExprKind::Value(Operand::Copy(
                                arr_op.place().unwrap().with_projection(Projection::Index {
                                    array_element: element,
                                    with: index_place,
                                }),
                            )),
                        },
                    ),
                });

                Some((element, Operand::Copy(place)))
            }
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
    /// Type is Unknown
    /// Map be thrown in multiple contexts or as a side effort
    UnknownTy,

    /// LHS of expression does not match RHS of expression
    LHSMatchRHS(TyID, TyID),
    /// Assignment of incorect type
    AssignWrongType {
        from: TyID,
        to: TyID,
    },
    CannotAssignToLHS,
    /// For Loop iterating over incorrect type
    ForLoopIterator {
        range: TyID,
        index: TyID,
    },
    CannotReference,
    CannotDoubleReference,
    CannotDeref(TyID),

    ExpectedBoolean(TyID),

    InvalidUnOp(UnOpKind, TyID),
    InvalidBinOp(BinOpKind, TyID, TyID),

    InvalidCast {
        from: TyID,
        to: TyID,
    },
    RedundantCast,

    SymbolDoesntExistBytecode,
    SymbolDoesntExist,
    FunctionDoesntExist,
    NoFunctionWithNArguments(usize),
    IncorrectFunctionArguments {
        from: TyID,
        to: TyID,
    },
    SymbolIsNotFunction,
    SymbolRedefined,
    FunctionReturnType {
        from: TyID,
        to: TyID,
    },
    NoMemberWithName(TyID),
    DuplicateMember,
    MissingFields,

    DotSyntaxOnConst,

    RequiresComp(TyID),
    RequiresQuery(TyID),

    // Enum related errors
    TypeCannotBeMatched(TyID),
    NoVariantWithName(TyID),
    MatchNotExhaustive(TyID),
    MatchArmDuplicate,
    MatchArmUnreachable,
    TooManyVariants(TyID),

    TypeIsNotAStruct,
    TypeIsNotAnEnum,
    DeadCode,
    TypeHasNoFields(TyID),

    NotAMemberFunction(TyID),
    NotAFreeFunction(TyID),
    MismatchedFunction(TyID, TyID),
    TrailingSemi,

    NoBreak,
    NoContinue,
    UnknownBuiltinOp,

    EmptyArray,
    CannotIndex(TyID),
    CannotIndexWith(TyID),

    UnsupportedFilterKind,
    DuplicateFilter(TyID),
    ConflictingFilter(TyID),

    EmptySchedule,
    FnInSchedule,
    ScheduleSysNoArguments,
    DuplicateInBundle(TyID),
}

impl std::fmt::Display for TyCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TyCheckError::*;
        let s = match self {
            UnknownTy => "Unknown Type",
            LHSMatchRHS(lhs, rhs) => &format!("Type '{}' must match Type '{}'", lhs, rhs),
            AssignWrongType { from, to } => {
                &format!("Cannot assign Type '{}' to Type '{}'", from, to)
            }
            CannotAssignToLHS => "Cannot assign to LHS",
            SymbolDoesntExist => "Symbol could not be found in the current scope",
            SymbolRedefined => "Symbol is already defined",
            ExpectedBoolean(tyid) => &format!("Expected boolean expression found Type '{}'", tyid),
            InvalidUnOp(op, tyid) => {
                &format!("Operation '{}' is not valid for Type '{}'", op, tyid)
            }
            InvalidBinOp(op, lhs, rhs) => &format!(
                "Operation '{}' is not valid for Type '{}' and Type '{}'",
                op, lhs, rhs
            ),
            FunctionReturnType { from, to } => {
                &format!("Cannot return Type '{}' from function Type '{}'", from, to)
            }
            ForLoopIterator { range, index } => {
                &format!("Cannot iterate Type '{}' over Type '{}'", index, range)
            }
            FunctionDoesntExist => "Could not find defined or imported ",
            SymbolIsNotFunction => "Symbol is not a function",
            NoFunctionWithNArguments(n) => &format!("No function name with {} arguments", n),
            IncorrectFunctionArguments { from, to } => &format!(
                "Arguments Type '{}' does not match argument Type '{}'",
                from, to
            ),
            SymbolDoesntExistBytecode => "Symbol specified in bytecode does not exist",
            InvalidCast { from, to } => {
                &format!("Cannot perform cast from Type '{}' to Type '{}'", from, to)
            }
            RedundantCast => "Cast is redundant",
            NoMemberWithName(tyid) => {
                &format!("Struct '{}' does not have a member with name", tyid)
            }
            DuplicateMember => "Member has already been defined",
            MissingFields => "Not all fields have been defined",
            DotSyntaxOnConst => "Dot syntax on literal not allowed",
            MatchNotExhaustive(tyid) => &format!("Match on type '{}' is not exhaustive", tyid),
            TooManyVariants(tyid) => &format!("Type '{}' has more than 128 variants", tyid),
            NoVariantWithName(tyid) => &format!("Type '{}' has no variant with name", tyid),
            TypeCannotBeMatched(tyid) => &format!("Type '{}' cannot be matched upon", tyid),
            MatchArmUnreachable => "Default case makes arm unreachable",
            MatchArmDuplicate => "Variant is already matched upon",
            DeadCode => "Stmts after terminator",
            TypeIsNotAStruct => "Type is not a struct",
            TypeIsNotAnEnum => "Type is not an enum",
            CannotReference => "Cannot reference temporary expression",
            CannotDoubleReference => "Cannot create references to reference types",
            CannotDeref(tyid) => &format!("Cannot deref Type '{}'", tyid),
            NotAMemberFunction(tyid) => {
                &format!("Function cannot be called as a member of Type '{}'", tyid)
            }
            NotAFreeFunction(tyid) => &format!("Function is a member of Type '{}'", tyid),
            MismatchedFunction(lhs, rhs) => &format!(
                "Expr with Type '{}' and function requires Type '{}'",
                lhs, rhs
            ),
            TrailingSemi => "Trailing semicolon",
            TypeHasNoFields(tyid) => &format!("Type '{}' has no fields", tyid),
            NoBreak => "Break outside of breakable context",
            NoContinue => "Continue outside of continuable context",
            RequiresComp(tyid) => &format!("Type '{}' is not a component", tyid),
            UnknownBuiltinOp => "Unknown builtin bytecode op",
            UnsupportedFilterKind => "Unsupported filter kind",
            DuplicateFilter(tyid) => &format!("Filter for Type '{}' already present", tyid),
            ConflictingFilter(tyid) => &format!("Conflicting for Type '{}' already present", tyid),
            RequiresQuery(tyid) => &format!("Expected Query found Type '{}'", tyid),
            EmptySchedule => "Schedule is empty",
            FnInSchedule => "Fn must be Sys to be used in schedule",
            ScheduleSysNoArguments => "System in a schedule must have no arguments",
            DuplicateInBundle(tyid) => &format!("Duplicate component of Type '{}' in bundle", tyid),
            EmptyArray => "Cannot create empty array",
            CannotIndex(tyid) => &format!("Cannot index non array Type '{}'", tyid),
            CannotIndexWith(tyid) => &format!("Cannot index array with Type '{}'", tyid),
        };
        write!(f, "{}", s)
    }
}
