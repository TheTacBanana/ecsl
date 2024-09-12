pub mod stmt;
pub mod expr;
pub mod ty;
pub mod op;
pub mod item;
pub mod callable;
pub mod data;
pub mod ecs;

pub (crate) type P<T> = Box<T>;

pub (crate) type Ident = ();