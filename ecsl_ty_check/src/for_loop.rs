use crate::TerminationKind;
use crate::TyCheck;
use crate::TyCheckError;
use ecsl_ast::expr::BinOpKind;
use ecsl_ast::expr::Expr as ExprAST;
use ecsl_ast::expr::ExprKind as ExprKindAST;
use ecsl_ast::visit::Visitor;
use ecsl_ast::visit::VisitorCF;
use ecsl_ast::{expr::RangeType, ty::Mutable};
use ecsl_bytecode::Immediate;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_gir::cons::Constant;
use ecsl_gir::expr::BinOp;
use ecsl_gir::expr::Expr;
use ecsl_gir::expr::ExprKind;
use ecsl_gir::expr::OperandKind;
use ecsl_gir::expr::QueryOpKind;
use ecsl_gir::stmt::Stmt;
use ecsl_gir::stmt::StmtKind;
use ecsl_gir::term::SwitchCase;
use ecsl_gir::term::Terminator;
use ecsl_gir::term::TerminatorKind;
use ecsl_gir::LocalKind;
use ecsl_gir::Place;
use ecsl_gir::{expr::Operand, Local};
use ecsl_index::BlockID;
use ecsl_index::LocalID;
use ecsl_index::TyID;
use ecsl_ty::TyIr;
use lrpar::Span;

pub enum ForLoopKind {
    Range {
        int_ty: TyID,
        lhs: Operand,
        rhs: Operand,
        range_type: RangeType,
        span: Span,
        internal_max: Option<LocalID>,
    },
    Query {
        entity_tyid: TyID,
        query: Operand,
        span: Span,
        active_query_id: Option<LocalID>,
    },
}

impl ForLoopKind {
    pub fn from_expr(
        ty_check: &mut TyCheck,
        expr: &ExprAST,
        out: &mut Option<ForLoopKind>,
    ) -> VisitorCF {
        macro_rules! unify {
            ($l:expr, $r:expr, $err:expr) => {
                if $l == TyID::UNKNOWN || $r == TyID::UNKNOWN || $l != $r {
                    ty_check.ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, $err).with_span(|_| expr.span),
                    );
                    return VisitorCF::Break;
                }
            };
        }

        macro_rules! err_if {
            ($c:expr, $e:expr, $s:expr) => {
                if $c {
                    err!($e, $s)
                }
            };
        }

        macro_rules! err {
            ($e:expr,$s:expr) => {{
                ty_check
                    .ty_ctxt
                    .diag
                    .push_error(EcslError::new(ErrorLevel::Error, $e).with_span(|_| $s));
                return VisitorCF::Break;
            }};
        }

        match &expr.kind {
            ExprKindAST::Range(lhs, rhs, range_type) => {
                // Visit LHS
                ty_check.visit_expr(lhs)?;
                let (lhs_ty, lhs_op) = ty_check.pop();

                // Visit RHS
                ty_check.visit_expr(rhs.as_ref())?;
                let (rhs_ty, rhs_op) = ty_check.pop();

                // Unify Types
                unify!(lhs_ty, rhs_ty, TyCheckError::LHSMatchRHS(lhs_ty, rhs_ty));

                *out = Some(ForLoopKind::Range {
                    int_ty: lhs_ty,
                    lhs: lhs_op,
                    rhs: rhs_op,
                    range_type: *range_type,
                    span: expr.span,
                    internal_max: None,
                })
            }
            ExprKindAST::Query(_) => {
                ty_check.visit_expr(expr)?;
                let (ty, op) = ty_check.pop();

                let query_tyid = ty_check.get_tyid(TyIr::Query);
                let entity_tyid = ty_check.get_tyid(TyIr::Entity);

                unify!(ty, query_tyid, TyCheckError::RequiresQuery(ty));

                *out = Some(ForLoopKind::Query {
                    entity_tyid,
                    query: op,
                    span: expr.span,
                    active_query_id: None,
                })
            }
            ExprKindAST::Ident(ident) => {
                // Search all symbols
                let found = ty_check.find_symbol(*ident);

                // Throw error if not found
                err_if!(found.is_none(), TyCheckError::SymbolDoesntExist, expr.span);
                let found = found.unwrap();

                let projected_tyid = found.projected_tyid(ty_check.cur_gir());
                let query_tyid = ty_check.get_tyid(TyIr::Query);
                let entity_tyid = ty_check.get_tyid(TyIr::Entity);

                unify!(
                    projected_tyid,
                    query_tyid,
                    TyCheckError::RequiresQuery(projected_tyid)
                );

                *out = Some(ForLoopKind::Query {
                    entity_tyid,
                    query: Operand::Move(found),
                    span: expr.span,
                    active_query_id: None,
                })
            }
            _ => {
                ty_check.ty_ctxt.diag.push_error(
                    EcslError::new(
                        ErrorLevel::Error,
                        TyCheckError::RequiresQuery(TyID::UNKNOWN),
                    )
                    .with_span(|_| expr.span),
                );
                return VisitorCF::Break;
            }
        };
        return VisitorCF::Continue;
    }

    pub fn get_iterator_type(&self) -> TyID {
        match self {
            ForLoopKind::Range { int_ty: tyid, .. }
            | ForLoopKind::Query {
                entity_tyid: tyid, ..
            } => *tyid,
        }
    }

    pub fn create_iterator_local(&mut self, ty_check: &mut TyCheck, span: Span) -> LocalID {
        match self {
            ForLoopKind::Range { .. } => self.create_range_local(ty_check, span),
            ForLoopKind::Query { .. } => self.create_query_local(ty_check, span),
        }
    }

    fn create_range_local(&mut self, ty_check: &mut TyCheck, span: Span) -> LocalID {
        let ForLoopKind::Range {
            int_ty,
            lhs,
            rhs,
            span: range_span,
            internal_max,
            ..
        } = self
        else {
            panic!()
        };

        // Create local ID
        let iterator_local_id =
            ty_check.new_local(Local::new(span, Mutable::Mut, *int_ty, LocalKind::Let));

        // Create Assignment Stmt
        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(iterator_local_id, span),
                Expr {
                    span: *range_span,
                    kind: ExprKind::Value(lhs.clone()),
                },
            ),
        });

        *internal_max =
            Some(ty_check.new_local(Local::new(span, Mutable::Imm, *int_ty, LocalKind::Internal)));

        // Create Max Value
        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(internal_max.unwrap(), span),
                Expr {
                    span: *range_span,
                    kind: ExprKind::Value(rhs.clone()),
                },
            ),
        });

        iterator_local_id
    }

    fn create_query_local(&mut self, ty_check: &mut TyCheck, span: Span) -> LocalID {
        let ForLoopKind::Query {
            entity_tyid: entity_ty,
            query,
            active_query_id,
            ..
        } = self
        else {
            panic!()
        };

        // Create local ID for entity_ty
        let query_id = ty_check.new_local(Local::new(
            span,
            Mutable::Mut,
            ty_check.get_tyid(TyIr::Int), // Type here does not matter just needs to be the correct size (4)
            LocalKind::Let,
        ));

        // Create Assignment Stmt
        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(query_id, span),
                Expr {
                    span,
                    kind: ExprKind::Query(QueryOpKind::Start, query.clone()),
                },
            ),
        });

        // Create local ID for entity
        let iterator_local_id =
            ty_check.new_local(Local::new(span, Mutable::Mut, *entity_ty, LocalKind::Let));

        *active_query_id = Some(query_id);

        iterator_local_id
    }

    pub fn create_comparison(
        &mut self,
        ty_check: &mut TyCheck,
        iterator_local_id: LocalID,
        span: Span,
    ) -> BlockID {
        match self {
            ForLoopKind::Range { .. } => {
                self.create_range_comparison(ty_check, iterator_local_id, span)
            }
            ForLoopKind::Query { .. } => {
                self.create_query_comparison(ty_check, iterator_local_id, span)
            }
        }
    }

    fn create_range_comparison(
        &self,
        ty_check: &mut TyCheck,
        iterator_local_id: LocalID,
        span: Span,
    ) -> BlockID {
        let ForLoopKind::Range {
            range_type,
            span: range_span,
            internal_max: Some(internal_max),
            ..
        } = self
        else {
            panic!()
        };

        let comparison = ty_check.new_local(Local::new(
            *range_span,
            Mutable::Imm,
            ty_check.get_tyid(TyIr::Bool),
            LocalKind::Temp,
        ));

        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(comparison, span),
                Expr {
                    span,
                    kind: ExprKind::BinOp(
                        BinOp(
                            OperandKind::Int,
                            match range_type {
                                RangeType::Exclusive => BinOpKind::Lt,
                                RangeType::Inclusive => BinOpKind::Leq,
                            },
                        ),
                        Operand::Copy(Place::from_local(iterator_local_id, span)),
                        Operand::Copy(Place::from_local(*internal_max, span)),
                    ),
                },
            ),
        });

        let leave_block = ty_check.new_block_without_stack();

        let _start_of_block =
            ty_check.terminate_with_new_block(TerminationKind::Higher, |block, next| {
                block.terminate(Terminator {
                    kind: TerminatorKind::Switch(
                        Operand::Move(Place::from_local(comparison, span)),
                        vec![
                            SwitchCase::Value(Immediate::Bool(true), next),
                            SwitchCase::Default(leave_block),
                        ],
                    ),
                });
            });

        leave_block
    }

    fn create_query_comparison(
        &self,
        ty_check: &mut TyCheck,
        iterator_local_id: LocalID,
        span: Span,
    ) -> BlockID {
        let ForLoopKind::Query {
            active_query_id: Some(active_query_id),
            ..
        } = self
        else {
            panic!()
        };

        let comparison = ty_check.new_local(Local::new(
            span,
            Mutable::Imm,
            ty_check.get_tyid(TyIr::Bool),
            LocalKind::Temp,
        ));

        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(comparison, span),
                Expr {
                    span,
                    kind: ExprKind::Query(
                        QueryOpKind::Next,
                        Operand::Copy(Place::from_local(*active_query_id, span)),
                    ),
                },
            ),
        });

        let leave_block = ty_check.new_block_without_stack();

        ty_check.terminate_with_new_block(TerminationKind::Higher, |block, next| {
            block.terminate(Terminator {
                kind: TerminatorKind::Switch(
                    Operand::Move(Place::from_local(comparison, span)),
                    vec![
                        SwitchCase::Value(Immediate::Bool(true), next),
                        SwitchCase::Default(leave_block),
                    ],
                ),
            });
        });

        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(iterator_local_id, span),
                Expr {
                    span,
                    kind: ExprKind::Query(
                        QueryOpKind::Take,
                        Operand::Copy(Place::from_local(*active_query_id, span)),
                    ),
                },
            ),
        });

        leave_block
    }

    pub fn create_increment(
        &self,
        ty_check: &mut TyCheck,
        iterator_local_id: LocalID,
        condition_block: BlockID,
        span: Span,
    ) {
        match self {
            ForLoopKind::Range { .. } => {
                self.create_range_increment(ty_check, iterator_local_id, condition_block, span)
            }
            ForLoopKind::Query { .. } => {
                self.create_query_increment(ty_check, iterator_local_id, condition_block, span)
            }
        }
    }

    fn create_range_increment(
        &self,
        ty_check: &mut TyCheck,
        iterator_local_id: LocalID,
        condition_block: BlockID,
        span: Span,
    ) {
        let ForLoopKind::Range { .. } = self else {
            panic!()
        };

        let one_const = ty_check.new_constant(Constant::Internal {
            imm: Immediate::Int(1),
        });
        ty_check.push_stmt_to_cur_block(Stmt {
            span,
            kind: StmtKind::Assign(
                Place::from_local(iterator_local_id, span),
                Expr {
                    span,
                    kind: ExprKind::BinOp(
                        BinOp(OperandKind::Int, BinOpKind::Add),
                        Operand::Copy(Place::from_local(iterator_local_id, span)),
                        Operand::Constant(one_const),
                    ),
                },
            ),
        });

        ty_check.terminate_with_existing_block(
            TerminationKind::Lower,
            condition_block,
            |block, next| {
                block.terminate(Terminator {
                    kind: TerminatorKind::Jump(next),
                });
            },
        );
    }

    fn create_query_increment(
        &self,
        ty_check: &mut TyCheck,
        _: LocalID,
        condition_block: BlockID,
        _: Span,
    ) {
        ty_check.terminate_with_existing_block(
            TerminationKind::Lower,
            condition_block,
            |block, next| {
                block.terminate(Terminator {
                    kind: TerminatorKind::Jump(next),
                });
            },
        );
    }
}
