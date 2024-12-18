use ecsl_ast_derive::AST;
use ecsl_index::SourceFileID;
use item::Item;

pub mod callable;
pub mod data;
pub mod ecs;
pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod visit;

pub mod parse {
    pub use crate::callable::*;
    pub use crate::data::*;
    pub use crate::ecs::*;
    pub use crate::expr::*;
    pub use crate::item::*;
    pub use crate::stmt::*;
    pub use crate::ty::*;
    pub use crate::*;
}

pub type P<T> = Box<T>;

#[derive(Debug, AST)]
pub struct SourceAST {
    pub file: SourceFileID,
    pub items: Vec<Item>,
}
