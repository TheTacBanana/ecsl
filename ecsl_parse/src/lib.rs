use ecsl_ast::SourceAST;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{SourceFileID, SymbolID};
use lazy_static::lazy_static;
use lrlex::{
    lrlex_mod, DefaultLexeme, DefaultLexerTypes, LRLexError, LRNonStreamingLexer,
    LRNonStreamingLexerDef,
};
use lrpar::{lrpar_mod, LexError, LexParseError, Lexeme, NonStreamingLexer, ParseError, Span};
use source::SourceFile;
use std::{cell::RefCell, rc::Rc};
use table::{PartialSymbolTable, SymbolKind, SymbolTable};

pub mod source;
pub mod table;

lrlex_mod!("ecsl.l");
lrpar_mod!("ecsl.y");

type LexerDef = LRNonStreamingLexerDef<DefaultLexerTypes<u32>>;
type LexerTy<'a, 'b> = LRNonStreamingLexer<'a, 'b, DefaultLexerTypes<u32>>;

lazy_static! {
    static ref LEXER_DEF: LexerDef = ecsl_l::lexerdef();
}

pub struct ParseResult {
    pub ast: Option<SourceAST>,
    pub table: SymbolTable,
}

pub fn parse_file(source: &SourceFile, lexer: &LexerTy, diag: DiagConn) -> ParseResult {
    let table = Rc::new(RefCell::new(PartialSymbolTable::new(source.id, &lexer)));
    let (ast, mut errs) = ecsl_y::parse(lexer, table.clone());
    let table = (*table).clone().into_inner().finish();

    let errs = errs
        .drain(..)
        .map(|e| match e {
            LexParseError::LexError(lex_error) => rewrite_lex_error(lex_error),
            LexParseError::ParseError(parse_error) => rewrite_parse_error(lexer, parse_error),
        })
        .collect::<Vec<_>>();
    for e in errs {
        diag.push_error(e);
    }

    ParseResult {
        ast: ast.unwrap_or(Err(())).ok(),
        table,
    }
}

fn rewrite_lex_error(err: LRLexError) -> EcslError {
    EcslError::new(ErrorLevel::Error, "Lexer Error").with_span(|_| err.span())
}

//TODO: Improve representation of repair sequences
fn rewrite_parse_error(lexer: &LexerTy, err: ParseError<DefaultLexeme, u32>) -> EcslError {
    let mut e_out =
        EcslError::new(ErrorLevel::Error, "Parser Error").with_span(|_| err.lexeme().span());

    // let notes = Vec::new();
    if let Some(repair_seq) = err.repairs().iter().next() {
        for step in repair_seq {
            match step {
                lrpar::ParseRepair::Delete(l) => {
                    let s = lexer.span_str(l.span()).replace('\n', "\\n");
                    e_out = e_out.with_note(|_| format!("Try remove '{}'", s));
                }
                lrpar::ParseRepair::Insert(id) => {
                    e_out = e_out
                        .with_note(|_| format!("Try insert '{}'", ecsl_y::token_epp(*id).unwrap()));
                }
                _ => (),
            }
        }
    }
    e_out
}

pub trait GenerateIdentExt {
    fn file_id(&self) -> SourceFileID;

    fn definition(&self, span: Span, kind: SymbolKind) -> SymbolID;

    fn string(&self, span: Span) -> String;

    fn usage(&self, span: Span, kind: SymbolKind) -> SymbolID;
}

impl GenerateIdentExt for Rc<RefCell<PartialSymbolTable<'_, '_>>> {
    fn file_id(&self) -> SourceFileID {
        self.borrow().file
    }

    fn definition(&self, span: Span, kind: SymbolKind) -> SymbolID {
        let mut s = self.borrow_mut();
        let symbol_string = s.lexer.span_str(span).to_string();
        let id = s.define_symbol(symbol_string, span, kind);
        id
    }

    fn string(&self, span: Span) -> String {
        let s = self.borrow_mut();
        s.lexer.span_str(span).to_string()
    }

    fn usage(&self, span: Span, kind: SymbolKind) -> SymbolID {
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
            #[allow(unused)]
            fn run_test(s: &str) -> (Option<Result<ecsl_ast::SourceAST, ()>>, usize) {
                println!("Testing: \'{s}\'");
                let f = format!($s, s);
                let lexer = crate::LEXER_DEF.lexer(&f);
                let table = std::rc::Rc::new(std::cell::RefCell::new(
                    crate::PartialSymbolTable::new(ecsl_index::SourceFileID::ZERO, &lexer),
                ));
                let (out, errs) = crate::ecsl_y::parse(&lexer, table.clone());
                println!("{:#?}", out);
                for e in &errs {
                    println!("{}", e.pp(&lexer, &crate::ecsl_y::token_epp));
                }
                (out, errs.len())
            }

            #[allow(unused)]
            fn pass_ret(s: &str) -> ecsl_ast::SourceAST {
                let (out, len) = run_test(s);
                assert!(len == 0);
                out.unwrap().unwrap()
            }

            #[allow(unused)]
            fn pass(s: &str) {
                let (_, len) = run_test(s);
                assert!(len == 0);
            }

            #[allow(unused)]
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
            pass(r#"if (true) {} else if (true) {} else if (true) {}"#);
        }

        #[test]
        fn match_() {
            pass(r#"match (e) { Some { val } -> {}, None -> {}, }"#);
        }

        #[test]
        fn break_continue() {
            pass(r#"break;"#);
            pass(r#"continue;"#);
        }

        #[test]
        fn return_() {
            pass(r#"return;"#);
            pass(r#"return 1 + 2;"#);
        }

        #[test]
        fn expr() {
            pass("foo = 1;");
            pass(";");
        }
    }

    mod expr {
        generate_pass_fail!("fn main() {{ let i: int = {}; }}");

        #[test]
        fn bin_ops() {
            assert_eq!(pass_ret("1 + 2"), pass_ret("(1 + 2)"));
            assert_eq!(pass_ret("1 + 2 * 3"), pass_ret("1 + (2 * 3)"));
            assert_eq!(
                pass_ret("1 - 2 + 3 * 4 / 5"),
                pass_ret("(1 - 2) + ((3 * 4) / 5)")
            );
            assert_eq!(
                pass_ret("true || false && true"),
                pass_ret("true || (false && true)")
            );
        }

        #[test]
        fn un_ops() {
            pass("*foo");
            pass("-foo");
            pass("!foo");
            pass("&self");
            pass("&mut self");

            assert_eq!(pass_ret("1 + -2"), pass_ret("1 + (-2)"));
            assert_eq!(pass_ret("false && !true"), pass_ret("false && (!true)"));
            assert_eq!(pass_ret("&foo.bar()"), pass_ret("&(foo.bar())"));
            assert_eq!(pass_ret("*foo.bar()"), pass_ret("*(foo.bar())"));
        }

        #[test]
        fn as_() {
            pass("i as int");
            pass("(1 + 2) as long");

            assert_eq!(pass_ret("1 + 2 as int"), pass_ret("1 + (2 as int)"));
        }

        #[test]
        fn array() {
            pass("[]");
            pass("[1]");
            pass("[1, 2,]");
        }

        #[test]
        fn literal() {
            pass(r#""Hello World!""#);
            pass(r#"'c'"#);
            pass(r#"'\n'"#);
            pass("true");
            pass("false");
            pass("1");
        }

        #[test]
        fn range() {
            pass("1..2");
            pass("1.0..2.0");
            pass("1..=2");
            pass("1..n");
        }

        #[test]
        fn struct_() {
            pass(r#"Foo {}"#);
            pass(r#"Foo { bar: 1, }"#);
            pass(r#"Foo { bar: 1, baz: 2 }"#);
            pass(r#"Foo::<int> { bar: 1, baz: 2 }"#);
        }

        #[test]
        fn enum_() {
            pass(r#"Option::None {}"#);
            pass(r#"Foo::Bar { baz: 2 }"#);
            pass(r#"Option::<T>::Some { val: 1, }"#);
        }

        #[test]
        fn field() {
            pass("foo.bar");
            pass("foo.bar.baz");
            pass("foo.bar().baz");

            assert_eq!(pass_ret("foo.bar().baz"), pass_ret("(foo.bar()).baz"))
        }

        #[test]
        fn function() {
            pass("foo()");
            pass("foo.bar()");
            pass("foo.bar().baz()");
        }

        #[test]
        fn schedule() {
            pass(r#"Schedule {}"#);
            pass(r#"Schedule [ {}, {}]"#);
            pass(r#"Schedule [ foo, bar, { baz } ]"#);
        }

        #[test]
        fn query() {
            pass(r#"Query()"#);
            pass(r#"Query.with<Foo>()"#);
            pass(r#"Query.with<Foo>.without<Bar>()"#);
        }
    }
}
