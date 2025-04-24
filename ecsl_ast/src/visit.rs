use crate::{
    data::{EnumDef, FieldDef, StructDef, VariantDef},
    ecs::{QueryExpr, Schedule, ScheduleKind},
    expr::{Expr, ExprKind, FieldExpr},
    item::{ImplBlock, Item, ItemKind, UseDef},
    parse::{Attributes, FnDef, RetTy},
    stmt::{Block, MatchArm, Stmt, StmtKind},
    ty::{ConcreteGenerics, Generics, Ty, TyKind},
    SourceAST,
};
use std::ops::{FromResidual, Try};

pub trait Visitor: Sized {
    fn visit_ast(&mut self, s: &SourceAST) -> VisitorCF {
        walk_ast(self, s)
    }
    #[must_use]
    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        walk_block(self, b)
    }
    #[must_use]
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        walk_stmt(self, s)
    }
    #[must_use]
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        walk_item(self, i)
    }
    #[must_use]
    fn visit_use(&mut self, u: &UseDef) -> VisitorCF {
        walk_use(self, u)
    }
    #[must_use]
    fn visit_fn(&mut self, f: &FnDef, _ctxt: FnCtxt) -> VisitorCF {
        walk_fn(self, f)
    }
    #[must_use]
    fn visit_generics(&mut self, g: &Generics) -> VisitorCF {
        walk_generics(self, g)
    }
    #[must_use]
    fn visit_concrete_generics(&mut self, c: &ConcreteGenerics) -> VisitorCF {
        walk_concrete_generics(self, c)
    }
    #[must_use]
    fn visit_impl(&mut self, i: &ImplBlock) -> VisitorCF {
        walk_impl(self, i)
    }
    #[must_use]
    fn visit_expr(&mut self, e: &Expr) -> VisitorCF {
        walk_expr(self, e)
    }
    #[must_use]
    fn visit_struct_def(&mut self, s: &StructDef) -> VisitorCF {
        walk_struct_def(self, s)
    }
    #[must_use]
    fn visit_enum_def(&mut self, e: &EnumDef) -> VisitorCF {
        walk_enum_def(self, e)
    }
    #[must_use]
    fn visit_variant_def(&mut self, v: &VariantDef) -> VisitorCF {
        walk_variant_def(self, v)
    }
    #[must_use]
    fn visit_field_def(&mut self, f: &FieldDef) -> VisitorCF {
        walk_field_def(self, f)
    }
    #[must_use]
    fn visit_ty(&mut self, t: &Ty) -> VisitorCF {
        walk_ty(self, t)
    }
    #[must_use]
    fn visit_arm(&mut self, a: &MatchArm) -> VisitorCF {
        walk_arm(self, a)
    }
    #[must_use]
    fn visit_field(&mut self, f: &FieldExpr) -> VisitorCF {
        walk_field(self, f)
    }
    #[must_use]
    fn visit_query(&mut self, q: &QueryExpr) -> VisitorCF {
        walk_query(self, q)
    }
    #[must_use]
    fn visit_schedule(&mut self, s: &Schedule) -> VisitorCF {
        walk_schedule(self, s)
    }
    #[must_use]
    fn visit_attributes(&mut self, a: &Attributes) -> VisitorCF {
        walk_attributes(self, a)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FnCtxt {
    Free,
    Impl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisitorCF {
    Continue,
    Break,
}

impl Try for VisitorCF {
    type Output = ();
    type Residual = ();

    fn from_output(_: Self::Output) -> Self {
        Self::Continue
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            VisitorCF::Continue => std::ops::ControlFlow::Continue(()),
            VisitorCF::Break => std::ops::ControlFlow::Break(()),
        }
    }
}

impl FromResidual for VisitorCF {
    fn from_residual(_: <Self as Try>::Residual) -> Self {
        Self::Break
    }
}

macro_rules! visit {
    ($e:expr) => {
        if $e == VisitorCF::Break {
            return VisitorCF::Break;
        }
    };
}

pub fn walk_ast<V: Visitor>(v: &mut V, source: &SourceAST) -> VisitorCF {
    for i in &source.items {
        visit!(v.visit_item(i));
    }
    VisitorCF::Continue
}

pub fn walk_item<V: Visitor>(v: &mut V, item: &Item) -> VisitorCF {
    match &item.kind {
        ItemKind::Use(u) => v.visit_use(u),
        ItemKind::Fn(f) => v.visit_fn(f, FnCtxt::Free),
        ItemKind::Impl(i) => v.visit_impl(i),
        ItemKind::Struct(s) => v.visit_struct_def(s),
        ItemKind::Enum(e) => v.visit_enum_def(e),
    }
}

pub fn walk_use<V: Visitor>(v: &mut V, u: &UseDef) -> VisitorCF {
    v.visit_attributes(&u.attributes)
}

pub fn walk_fn<V: Visitor>(v: &mut V, f: &FnDef) -> VisitorCF {
    visit!(v.visit_attributes(&f.attributes));
    if let Some(g) = &f.generics {
        visit!(v.visit_generics(g));
    }
    if let RetTy::Ty(t) = &f.ret {
        visit!(v.visit_ty(t));
    }
    visit!(v.visit_block(&f.block));
    VisitorCF::Continue
}

pub fn walk_generics<V: Visitor>(_v: &mut V, _g: &Generics) -> VisitorCF {
    VisitorCF::Continue
}

pub fn walk_concrete_generics<V: Visitor>(v: &mut V, c: &ConcreteGenerics) -> VisitorCF {
    for ty in &c.params {
        visit!(v.visit_ty(ty))
    }
    VisitorCF::Continue
}

pub fn walk_struct_def<V: Visitor>(v: &mut V, s: &StructDef) -> VisitorCF {
    visit!(v.visit_attributes(&s.attributes));
    for f in &s.fields {
        visit!(v.visit_field_def(&f))
    }
    VisitorCF::Continue
}

pub fn walk_enum_def<V: Visitor>(v: &mut V, e: &EnumDef) -> VisitorCF {
    visit!(v.visit_attributes(&e.attributes));
    for var in &e.variants {
        visit!(v.visit_variant_def(var));
    }
    VisitorCF::Continue
}

pub fn walk_variant_def<V: Visitor>(v: &mut V, var: &VariantDef) -> VisitorCF {
    for f in &var.fields {
        visit!(v.visit_field_def(&f))
    }
    VisitorCF::Continue
}

pub fn walk_field_def<V: Visitor>(v: &mut V, f: &FieldDef) -> VisitorCF {
    visit!(v.visit_ty(&f.ty));
    VisitorCF::Continue
}

pub fn walk_impl<V: Visitor>(v: &mut V, i: &ImplBlock) -> VisitorCF {
    for f in &i.fn_defs {
        visit!(v.visit_fn(f, FnCtxt::Impl));
    }
    VisitorCF::Continue
}

pub fn walk_block<V: Visitor>(v: &mut V, b: &Block) -> VisitorCF {
    for stmt in &b.stmts {
        visit!(v.visit_stmt(stmt));
    }
    VisitorCF::Continue
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &Stmt) -> VisitorCF {
    match &s.kind {
        StmtKind::Let(_, _, _, ty, expr) => {
            if let Some(ty) = ty {
                visit!(v.visit_ty(ty));
            }
            visit!(v.visit_expr(expr));
        }

        StmtKind::If(expr, block, next) => {
            visit!(v.visit_expr(expr));
            visit!(v.visit_block(block));
            if let Some(next) = next {
                visit!(v.visit_stmt(next));
            }
        }
        StmtKind::ElseIf(expr, block, next) => {
            visit!(v.visit_expr(expr));
            visit!(v.visit_block(block));
            if let Some(next) = next {
                visit!(v.visit_stmt(next));
            }
        }
        StmtKind::Else(block) => {
            visit!(v.visit_block(block))
        }
        StmtKind::For(_, ty, expr, block) => {
            if let Some(ty) = ty {
                visit!(v.visit_ty(ty));
            }
            visit!(v.visit_expr(expr));
            visit!(v.visit_block(block));
        }
        StmtKind::While(expr, block) => {
            visit!(v.visit_expr(expr));
            visit!(v.visit_block(block));
        }
        StmtKind::Match(expr, arms) => {
            visit!(v.visit_expr(expr));
            for arm in arms {
                visit!(v.visit_arm(arm));
            }
        }
        StmtKind::Return(e) => {
            if let Some(e) = e {
                visit!(v.visit_expr(e))
            }
        }
        StmtKind::Expr(e) => visit!(v.visit_expr(e)),

        StmtKind::Break | StmtKind::Continue | StmtKind::Semi | StmtKind::BYT(_) => (),
    }
    VisitorCF::Continue
}

pub fn walk_expr<V: Visitor>(v: &mut V, expr: &Expr) -> VisitorCF {
    match &expr.kind {
        ExprKind::Assign(_, _, e) => visit!(v.visit_expr(e)),
        ExprKind::Ref(_, e) => visit!(v.visit_expr(e)),
        ExprKind::UnOp(_, e) => visit!(v.visit_expr(e)),
        ExprKind::BinOp(_, e1, e2) => {
            visit!(v.visit_expr(e1));
            visit!(v.visit_expr(e2));
        }
        ExprKind::Array(e_list) => {
            for e in e_list {
                visit!(v.visit_expr(e));
            }
        }
        ExprKind::Struct(t, fields) => {
            visit!(v.visit_ty(t));
            for f in fields {
                visit!(v.visit_field(f));
            }
        }
        ExprKind::Enum(t, _, fields) => {
            visit!(v.visit_ty(t));
            for f in fields {
                visit!(v.visit_field(f));
            }
        }
        ExprKind::Range(e1, e2, _) => {
            visit!(v.visit_expr(e1));
            visit!(v.visit_expr(e2));
        }
        ExprKind::Cast(e, t) => {
            visit!(v.visit_expr(e));
            visit!(v.visit_ty(t));
        }
        ExprKind::Field(e, _) => visit!(v.visit_expr(e)),
        ExprKind::StaticFunction(_, _, cg, args) => {
            visit!(v.visit_concrete_generics(cg));
            for e in args {
                visit!(v.visit_expr(e))
            }
        }
        ExprKind::Function(e, cg, _, args) => {
            if let Some(e) = e {
                visit!(v.visit_expr(e));
            }
            visit!(v.visit_concrete_generics(cg));
            for e in args {
                visit!(v.visit_expr(e))
            }
        }
        ExprKind::Query(q) => visit!(v.visit_query(q)),
        ExprKind::Schedule(s) => visit!(v.visit_schedule(s)),
        ExprKind::Lit(_) | ExprKind::Ident(_) | ExprKind::MethodSelf(_) => (),
        ExprKind::Bundle(e_list) => {
            for e in e_list {
                visit!(v.visit_expr(e))
            }
        }
    }
    VisitorCF::Continue
}

pub fn walk_ty<V: Visitor>(v: &mut V, typ: &Ty) -> VisitorCF {
    visit!(v.visit_concrete_generics(&typ.generics));
    match &typ.kind {
        TyKind::Ident(_) => {}
        TyKind::Array(ty, _) => visit!(v.visit_ty(ty)),
        TyKind::ArrayRef(_, ty) => visit!(v.visit_ty(ty)),
        TyKind::Ref(_, ty) => visit!(v.visit_ty(ty)),
        TyKind::Ptr(_, ty) => visit!(v.visit_ty(ty)),
        TyKind::Entity(_, ty) => {
            for attr in &ty.bounds {
                visit!(v.visit_ty(&attr.ty))
            }
        }
        TyKind::Schedule => (),
        TyKind::Query(_) => (),
    }
    VisitorCF::Continue
}

pub fn walk_arm<V: Visitor>(v: &mut V, a: &MatchArm) -> VisitorCF {
    visit!(v.visit_block(&a.block));
    VisitorCF::Continue
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &FieldExpr) -> VisitorCF {
    visit!(v.visit_expr(&f.expr));
    VisitorCF::Continue
}

pub fn walk_query<V: Visitor>(_v: &mut V, _q: &QueryExpr) -> VisitorCF {
    VisitorCF::Continue
}

pub fn walk_schedule<V: Visitor>(v: &mut V, s: &Schedule) -> VisitorCF {
    match &s.kind {
        ScheduleKind::Sys(_, _) => (),
        ScheduleKind::Ordered(list) | ScheduleKind::Unordered(list) => {
            for s in list {
                visit!(v.visit_schedule(s))
            }
        }
    }
    VisitorCF::Continue
}

pub fn walk_attributes<V: Visitor>(_v: &mut V, _a: &Attributes) -> VisitorCF {
    VisitorCF::Continue
}
