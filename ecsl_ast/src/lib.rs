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

#[cfg(test)]
mod test {
    /// Test cases for Type Parsing
    mod ty {
        #[test]
        fn ident() {
            "Foo";
            "Foo<Bar>";
            "Foo<Bar, Baz>";
            "Foo<Bar<Baz>, Qux>";
        }

        #[test]
        fn array() {
            "[Foo : 5]";
            "[Foo<Bar> : 5]";
        }

        #[test]
        fn array_ref() {
            "&[Foo]";
            "&[Foo<Bar>]";
        }

        #[test]
        fn ref_() {
            "&Foo";
            "&Foo<Bar>";
            "&mut Bar";
            "&mut Bar<Baz>";
        }

        #[test]
        fn ptr() {
            "*imm Foo";
            "*imm Foo<Bar>";
            "*mut Bar";
            "*mut Baz<Qux>";
        }

        #[test]
        fn entity() {
            "Entity";
            "Entity<foo: Foo>";
            "Entity<foo: mut Foo>";
            "Entity<foo: Bar, baz: mut Qux>";
            "Entity<foo: Bar, ..>";
        }

        #[test]
        fn query() {
            "Query";
            "Query<With<Foo>>";
            "Query<Without<Bar>>";
            "Query<With<Baz>,Without<Qux>>";
        }

        #[test]
        fn system() {
            "System";
        }

        #[test]
        fn schedule() {
            "Schedule";
        }
    }
}