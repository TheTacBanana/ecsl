use ecsl_index::generate_index_type;
use item::Item;

pub mod callable;
pub mod data;
pub mod ecs;
pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;

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

generate_index_type!(SymbolId);

#[derive(Debug)]
pub struct ParsedFile {
    pub items: Vec<Item>,
}
