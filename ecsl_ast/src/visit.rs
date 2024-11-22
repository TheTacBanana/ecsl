use std::collections::{vec_deque, VecDeque};

use crate::{
    data::{EnumDef, StructDef},
    expr::Expr,
    item::{ImplBlock, Item, ItemKind, UseDef},
    parse::FnDef,
    stmt::{Block, Stmt},
    ty::{Generics, Ty, TyKind},
    SourceAST, SymbolId, P,
};

pub trait Visitor: Sized {
    fn visit_block(&mut self, b: &Block) -> VisitorCF {
        walk_block(self, b)
    }
    fn visit_stmt(&mut self, s: &Stmt) -> VisitorCF {
        walk_stmt(self, s)
    }
    fn visit_item(&mut self, i: &Item) -> VisitorCF {
        walk_item(self, i)
    }
    fn visit_use(&mut self, u: &UseDef) -> VisitorCF {
        walk_use(self, u)
    }
    fn visit_fn(&mut self, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
        walk_fn(self, f, ctxt)
    }
    fn visit_generics(&mut self, g: &Generics) -> VisitorCF {
        walk_generics(self, g)
    }
    fn visit_impl(&mut self, i: &ImplBlock) -> VisitorCF {
        walk_impl(self, i)
    }

    // fn visit_struct_def(&mut self, s: &StructDef);
    // fn visit_enum_def(&mut self, e: &EnumDef);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FnCtxt {
    Free,
    Impl,
}

pub struct ASTVisitor<'v, 'ast, V: Visitor> {
    pub visitor: &'v mut V,
    pub ast: &'ast SourceAST,
}

impl<'v, 'ast, V: Visitor> ASTVisitor<'v, 'ast, V> {
    pub fn new(visitor: &'v mut V, ast: &'ast SourceAST) -> Self {
        Self { visitor, ast }
    }

    pub fn visit(&mut self) {
        for item in &self.ast.items {
            self.visitor.visit_item(item);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisitorCF {
    Continue,
    Break,
}

macro_rules! visit {
    ($e:expr) => {
        if $e == VisitorCF::Break {
            return VisitorCF::Break;
        }
    };
}

pub fn walk_item<V: Visitor>(v: &mut V, item: &Item) -> VisitorCF {
    match &item.kind {
        ItemKind::Use(u) => v.visit_use(u),
        ItemKind::Fn(f) => v.visit_fn(f, FnCtxt::Free),
        ItemKind::Impl(i) => v.visit_impl(i),

        ItemKind::Struct(s) => todo!(),
        ItemKind::Enum(e) => todo!(),
    }
}

pub fn walk_use<V: Visitor>(v: &mut V, u: &UseDef) -> VisitorCF {
    VisitorCF::Continue
}

pub fn walk_fn<V: Visitor>(v: &mut V, f: &FnDef, ctxt: FnCtxt) -> VisitorCF {
    visit!(v.visit_fn(f, ctxt));
    if let Some(g) = &f.generics {
        visit!(v.visit_generics(g));
    }
    visit!(v.visit_block(&f.block));
    VisitorCF::Continue
}

pub fn walk_generics<V: Visitor>(v: &mut V, g: &Generics) -> VisitorCF {
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
    todo!()
}
