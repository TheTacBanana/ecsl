use ecsl_diagnostics::Diagnostics;
// use ecsl_lexer::{stream::TokenStream, token::Token};

use ecsl_source::SourceFile;
use lrlex::{lrlex_mod, DefaultLexerTypes, LRNonStreamingLexerDef};
use lrpar::lrpar_mod;


lrlex_mod!("ecsl.l");
lrpar_mod!("ecsl.y");

// use ecsl_l::

pub fn parse(source: &SourceFile) {
    // LEXER.lexer(source)
}