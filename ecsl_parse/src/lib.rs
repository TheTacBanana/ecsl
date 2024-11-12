// use ecsl_diagnostics::Diagnostics;

use std::{cell::RefCell, rc::Rc};

use ecsl_ast::SymbolId;
use ecsl_source::SourceFile;
use lazy_static::lazy_static;
use lrlex::{lrlex_mod, DefaultLexerTypes, LRNonStreamingLexerDef};
use lrpar::{lrpar_mod, NonStreamingLexer, Span};
use table::{PartialSymbolTable, SymbolKind};

pub mod table;

lrlex_mod!("ecsl.l");
lrpar_mod!("ecsl.y");

type LexerTy = LRNonStreamingLexerDef<DefaultLexerTypes<u32>>;

lazy_static! {
    static ref LEXER_DEF: LexerTy = ecsl_l::lexerdef();
}

pub fn parse_file(source: &SourceFile) {
    let lexer = LEXER_DEF.lexer(&source.contents);

    let table = Rc::new(RefCell::new(PartialSymbolTable::new(&lexer)));
    let (ast, errs) = ecsl_y::parse(&lexer, table.clone());
    println!("{:#?}", ast);
    for e in errs {
        println!("{}", e.pp(&lexer, &ecsl_y::token_epp));
    }
}

pub trait GenerateIdentExt {
    fn definition(&self, span: Span, kind: SymbolKind) -> SymbolId;

    fn usage(&self, span: Span, kind: SymbolKind) -> SymbolId;
}

impl GenerateIdentExt for Rc<RefCell<PartialSymbolTable<'_, '_>>> {
    fn definition(&self, span: Span, kind: SymbolKind) -> SymbolId {
        let mut s = self.borrow_mut();
        let symbol_string = s.lexer.span_str(span).to_string();
        let id = s.define_symbol(symbol_string, span, kind);
        id
    }

    fn usage(&self, span: Span, kind: SymbolKind) -> SymbolId {
        let mut s = self.borrow_mut();
        let symbol_string = s.lexer.span_str(span).to_string();
        let id = s.use_symbol(symbol_string, span, kind);
        id
    }
}

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>) -> Result<Vec<T>, ()> {
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}

#[cfg(test)]
mod test {
    macro_rules! generate_pass_fail {
        ($s:expr) => {
            fn pass(s: &str) {
                println!("Testing: \'{s}\'");
                let f = format!($s, s);
                let lexer = crate::LEXER_DEF.lexer(&f);
                let table = std::rc::Rc::new(std::cell::RefCell::new(
                    crate::PartialSymbolTable::new(&lexer),
                ));
                let (_, errs) = crate::ecsl_y::parse(&lexer, table.clone());
                for e in &errs {
                    println!("{}", e.pp(&lexer, &crate::ecsl_y::token_epp));
                }
                assert!(errs.len() == 0)
            }

            fn fail(s: &str) {
                print!("Fail ");
                assert!(std::panic::catch_unwind(|| pass(s)).is_err());
            }
        };
    }

    /// Test cases for Type Parsing
    mod ty {
        generate_pass_fail!("fn main() {{ let f : {} = 0; }}");

        #[test]
        fn ident() {
            pass("Foo");
            pass("Foo_Bar");
            pass("Foo7");
            pass("Foo<Bar>");
            pass("Foo<Bar, Baz>");
            pass("Foo<Bar<Baz>, Qux>");

            fail("1Foo");
            fail("Foo>");
            fail("Foo-Bar");
        }

        #[test]
        fn array() {
            // Const array
            pass("[Foo : 5]");
            pass("[Foo<Bar> : 5]");
            pass("[[Foo: 5] : 5]");

            // Array Ref
            pass("&[Foo]");
            pass("&[Foo<Bar>]");
            pass("&mut [Foo]");

            fail("Foo : 5]");
            fail("[Foo : 5");
            fail("[Foo  5]");
        }

        #[test]
        fn ref_() {
            pass("&Foo");
            pass("&Foo<Bar>");
            pass("&mut Bar");
            pass("&mut Bar<Baz>");
        }

        #[test]
        fn ptr() {
            pass("*imm Foo");
            pass("*imm Foo<Bar>");
            pass("*mut Bar");
            pass("*mut Baz<Qux>");

            fail("*int Foo");
        }

        #[test]
        fn entity() {
            pass("Entity");
            pass("Entity<>");
            pass("Entity<foo: Foo>");
            pass("Entity<foo: Bar, baz: Qux,>");
        }

        #[test]
        fn query() {
            pass("Query");
        }

        #[test]
        fn schedule() {
            pass("Schedule");
        }
    }

    /// Test cases for Item Parsing
    mod item {
        generate_pass_fail!("{}");

        #[test]
        fn imports() {
            pass("use folder::file::Item;");
            pass(r#"use folder::file::{Item1, Item2};"#);
            pass("use super::file::Item;");
            pass(r#"use super::file::{};"#);

            fail("use ;");
            fail("use ::;");
            fail("use ::file;");
            fail("use super::;");
        }

        #[test]
        fn func_defs() {
            // Fn Kind
            pass(r#"fn foo() {}"#);
            pass(r#"sys foo() {}"#);

            // Return Types
            pass(r#"fn foo() -> Schedule {}"#);
            fail(r#"fn foo() ->  {}"#);

            // Arguments
            pass(r#"fn foo(bar: &Bar,) {}"#);
            pass(r#"fn foo(bar: &Bar, mut buz: Buz) {}"#);

            // Member Functions
            pass(r#"fn foo(self) {}"#);
            pass(r#"fn foo(mut self) {}"#);
            pass(r#"fn foo(&self) {}"#);
            pass(r#"fn foo(&mut self) {}"#);

            // Generics
            pass(r#"fn foo<T, U>(t: T) -> U {}"#);
        }

        #[test]
        fn struct_defs() {
            pass(r#"struct Foo;"#);
            pass(r#"struct comp Foo;"#);

            pass(r#"struct Foo {}"#);
            pass(r#"struct Foo { bar: Bar }"#);
            pass(r#"struct Foo { bar: Bar, baz: Baz, }"#);
            pass(r#"struct Foo<T> { bar: Bar, val: T }"#);
        }

        #[test]
        fn enum_defs() {
            pass(r#"enum Foo;"#);
            pass(r#"enum comp Foo;"#);

            pass(r#"enum Foo { Bar, Buz}"#);
            pass(r#"enum Option { None, Some {} }"#);
            pass(r#"enum Option<T> { None, Some { val: T}}"#);
        }

        #[test]
        fn impl_block() {
            pass(r#"impl Foo { }"#);
            pass(r#"impl<T> Foo<T> { }"#);
            pass(r#"impl Foo { fn bar() -> int {} }"#);
        }
    }

    /// Test cases for Stmt Parsing
    mod stmt {
        generate_pass_fail!("fn main() {{ {} }}");

        #[test]
        fn let_() {
            pass(r#"let i : int = 0;"#);
            pass(r#"let mut i : int = 0;"#);

            fail(r#"let i :  = 0;"#);
        }

        #[test]
        fn for_() {
            pass(r#"for (i : int in array) {}"#);
            pass(r#"for (i : int in &array) {}"#);
            pass(r#"for (i : int in 0..10) {}"#);
            pass(r#"for (i : int in 0..=10) {}"#);
        }

        #[test]
        fn while_() {
            pass(r#"while (true) {}"#);
        }

        #[test]
        fn if_() {
            pass(r#"if (true) {}"#);
            pass(r#"if (true) {} else {}"#);
            pass(r#"if (true) {} else if (true) {}"#);
        }

    }
}
