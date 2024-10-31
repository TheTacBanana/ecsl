use cfgrammar::Span;
use ecsl_index::generate_index_type;

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
    pub use crate::P;
    pub use crate::Ident;
}

pub type P<T> = Box<T>;

#[derive(Debug, Clone, Copy)]
pub struct Ident(pub Span);

// #[cfg(test)]
// mod test {
//     /// Test cases for Type Parsing
//     mod ty {
//         use ecsl_index::{
//             index::{BytePos, SourceFileID},
//         };

//         use crate::{
//             path::{Path, Segment},
//             stmt::StmtKind,
//             ty::{Ty, TyKind},
//             Ident,
//         };

//         #[test]
//         fn ident() {
//             "Foo";
//             Ty::new(
//                 Span::new(SourceFileID::ZERO, BytePos::new(0), BytePos::new(2)),
//                 TyKind::Ident(Path::new(
//                     Span::new(SourceFileID::ZERO, BytePos::new(0), BytePos::new(2)),
//                     vec![Segment::new(
//                         Span::new(SourceFileID::ZERO, BytePos::new(0), BytePos::new(2)),
//                         Ident::new(0),
//                         None,
//                     )],
//                 )),
//             );
//             "Foo<Bar>";
//             "Foo<Bar, Baz>";
//             "Foo<Bar<Baz>, Qux>";
//         }

//         #[test]
//         fn array() {
//             "[Foo : 5]";
//             "[Foo<Bar> : 5]";
//             "[[Foo: 5] : 5]";
//         }

//         #[test]
//         fn array_ref() {
//             "&[Foo]";
//             "&[Foo<Bar>]";
//         }

//         #[test]
//         fn ref_() {
//             "&Foo";
//             "&Foo<Bar>";
//             "&mut Bar";
//             "&mut Bar<Baz>";
//         }

//         #[test]
//         fn ptr() {
//             "*imm Foo";
//             "*imm Foo<Bar>";
//             "*mut Bar";
//             "*mut Baz<Qux>";
//         }

//         #[test]
//         fn entity() {
//             "Entity";
//             "Entity<foo: Foo>";
//             "Entity<foo: mut Foo>";
//             "Entity<foo: Bar, baz: mut Qux>";
//             "Entity<foo: Bar, ..>";
//         }

//         #[test]
//         fn query() {
//             "Query";
//             "Query<With<Foo>>";
//             "Query<Without<Bar>>";
//             "Query<With<Baz>,Without<Qux>>";
//         }

//         #[test]
//         fn system() {
//             "System";
//         }

//         #[test]
//         fn schedule() {
//             "Schedule";
//         }
//     }
// }
