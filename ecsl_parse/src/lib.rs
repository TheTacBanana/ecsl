// use ecsl_diagnostics::Diagnostics;

use ecsl_source::SourceFile;
use lrlex::{lrlex_mod, DefaultLexeme};
use lrpar::{lrpar_mod, Span};


lrlex_mod!("ecsl.l");
lrpar_mod!("ecsl.y");

pub fn parse(source: &SourceFile) {
    let lexer_def = ecsl_l::lexerdef();
    let lexer = lexer_def.lexer(&source.contents);

    let (ast, errs) = ecsl_y::parse(&lexer);
    println!("{:#?}", ast);
    for e in errs {
        println!("{}",e )
    }
}
