use std::collections::BTreeMap;

use cfgrammar::Span;
use cons::Constant;
use ecsl_ast::ty::Mutable;
use ecsl_index::{BlockID, ConstID, LocalID, TyID};
use stmt::Stmt;
use term::Terminator;

pub mod cons;
pub mod expr;
pub mod stmt;
pub mod term;
pub mod visit;

pub type P<T> = Box<T>;

/// (Control Flow) Graph Intermediate Representation for a given function
pub struct GIR {
    pub fn_id: TyID,
    pub locals: BTreeMap<LocalID, Local>,
    pub consts: BTreeMap<ConstID, Constant>,
    pub blocks: Vec<Block>,
}

// Individual block
pub struct Block {
    pub id: BlockID,
    pub stmts: Vec<Stmt>,
    pub term: Option<Terminator>,
}

pub struct Local {
    pub span: Span,
    pub mutable: Mutable,
}
