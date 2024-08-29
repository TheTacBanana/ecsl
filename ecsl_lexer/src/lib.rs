use std::str::Chars;

use ecsl_source::SourceFile;
use stream::TokenStream;

pub mod stream;
pub mod token;

pub struct SourceReader<'a> {
    chars: Chars<'a>,
    prev: char,
    chars_length: usize,
}

impl<'a> SourceReader<'a> {
    pub const EOF: char = '\0';

    pub fn new(s: &'a str) -> Self {
        SourceReader::<'a> {
            chars_length: s.len(),
            chars: s.chars(),
            prev: Self::EOF,
        }
    }

    pub fn lex(self) -> TokenStream {
        TokenStream::default()
    }

    pub fn prev(&self) -> char {
        self.prev
    }

    pub fn first(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn second(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next()
    }

    pub fn third(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next()
    }

    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev = c;
        Some(c)
    }

    pub fn bump_while(&mut self, f: impl Fn(char) -> bool) {
        while !self.is_eof() && f(self.first().unwrap()) {
            self.bump();
        }
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn current_token_length(&self) -> usize {
        self.chars_length - self.chars.as_str().len()
    }

    pub fn reset_current_token_length(&mut self) {
        self.chars_length = self.chars.as_str().len();
    }
}

#[cfg(test)]
pub mod test {
    use crate::SourceReader;

    #[test]
    pub fn first_second_third() {
        let s = "abcd";
        let reader = SourceReader::new(&s);

        // Assert the correct characters are acquired
        assert_eq!(
            [reader.first(), reader.second(), reader.third(),],
            [Some('a'), Some('b'), Some('c'),]
        )
    }

    #[test]
    pub fn bump() {
        let s = "abcd";
        let mut reader = SourceReader::new(&s);

        // Bump through all characters in the string
        assert_eq!(reader.bump(), Some('a'));
        assert_eq!(reader.bump(), Some('b'));
        assert_eq!(reader.bump(), Some('c'));
        assert_eq!(reader.bump(), Some('d'));

        // Assert no characters remain
        assert_eq!(reader.bump(), None);
        assert!(reader.is_eof());
    }

    #[test]
    pub fn bump_while() {
        let s = "abcd";
        let mut reader = SourceReader::new(&s);

        // Bump the first 3 characters
        reader.bump_while(|c| c <= 'c');
        assert_eq!(reader.bump(), Some('d'));

        // Assert no more characters remain
        assert!(reader.is_eof());
    }

    #[test]
    pub fn token_length() {
        let s = "abcd";
        let mut reader = SourceReader::new(&s);

        // Assert initial state
        assert_eq!(reader.current_token_length(), 0);

        // Take first char
        assert_eq!(reader.bump(), Some('a'));
        assert_eq!(reader.current_token_length(), 1);

        // Take second char
        assert_eq!(reader.bump(), Some('b'));
        assert_eq!(reader.current_token_length(), 2);

        // Reset and assert length of 0
        reader.reset_current_token_length();
        assert_eq!(reader.current_token_length(), 0);
    }
}
