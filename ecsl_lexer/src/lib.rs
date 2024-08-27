use std::str::Chars;

use ecsl_source::SourceFile;
use stream::TokenStream;

pub mod stream;
pub mod token;

pub struct SourceReader<'a> {
    chars: Chars<'a>
}

impl<'a> SourceReader<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        SourceReader::<'a> {
            chars: source_file.contents.chars()
        }
    }

    pub fn lex(self) -> TokenStream {
        TokenStream::default()
    }
}
