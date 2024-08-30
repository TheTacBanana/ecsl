use std::str::Chars;

use ecsl_source::SourceFile;
use stream::TokenStream;
use token::{LiteralKind, Token, TokenKind};

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

impl SourceReader<'_> {
    pub fn lex(mut self) -> TokenStream {
        let mut stream = TokenStream::new();

        loop {
            let token = self.next_token();
            stream.push_token(token);

            if token.kind() == TokenKind::Eof {
                break;
            }
        }

        stream
    }

    pub fn next_token(&mut self) -> Token {
        let Some(c) = self.bump() else {
            return Token::new(TokenKind::Eof, 0);
        };

        use TokenKind::*;
        let kind = match c {
            // Match Comments
            '/' => match self.first() {
                Some('/') => self.comment(),
                // Case for Slash Symbol
                _ => Slash,
            },

            // Match Whitespace
            c if c.is_whitespace() => self.whitespace(),

            // Match Identifers
            c if c.ident_start() => {
                self.bump_while(|c| c.ident_continue());
                TokenKind::Identifier
            }

            // Match Numeric Literals
            c if c.is_numeric() => {
                let kind = self.numeric_literal();
                let suffix = self.current_token_length() as u32;
                self.eat_identifier();
                TokenKind::Literal { kind, suffix }
            }

            // Match String Literals
            '"' => self.string_literal(),

            // Match Char Literals
            '\'' => self.char_literal(),

            // Symbols
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '#' => Hash,
            ':' => Colon,
            '=' => Eq,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '+' => Plus,
            '-' => Minus,
            // Slash is already matched
            '&' => Ampersand,
            '|' => Pipe,

            _ => Unknown,
        };

        let token = Token::new(kind, self.current_token_length());
        self.reset_current_token_length();
        token
    }

pub trait TokenRulesExt {
    fn ident_start(&self) -> bool;
    fn ident_continue(&self) -> bool;
}

impl TokenRulesExt for char {
    fn ident_start(&self) -> bool {
        *self == '_' || self.is_alphabetic()
    }

    fn ident_continue(&self) -> bool {
        self.ident_start() || self.is_numeric()
    }
    }

#[rustfmt::skip::macros(test_single_token)]
#[cfg(test)]
pub mod test {
    use crate::{
        token::{LiteralKind, Token, TokenKind},
        SourceReader,
    };

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

    /// Generates a unit test for an input string and expected output token
    macro_rules! test_single_token {
        ($test_name:ident, $string:expr, $kind:expr) => {
            #[test]
            pub fn $test_name() {
                let s: &str = $string;
                let reader = SourceReader::new(&s);

                let stream = reader.lex();

                assert_eq!(stream.tokens()[0], Token::new($kind, s.len()));
                assert_eq!(stream.tokens()[1], Token::new(TokenKind::Eof, 0));
            }
        };
    }

    test_single_token!(comment, "// comment", TokenKind::Comment);
    test_single_token!(whitespace, "    \n  \t \r   ", TokenKind::Whitespace);
    test_single_token!(identifer, "identifier", TokenKind::Identifier);
    test_single_token!(integer, "1235", TokenKind::Literal{ kind: LiteralKind::Int, suffix: 4 });
    test_single_token!(float, "3.1415", TokenKind::Literal{ kind: LiteralKind::Float, suffix: 6 });
    test_single_token!(string, "\"string literal\"", TokenKind::Literal{ kind: LiteralKind::String { terminated: true }, suffix: 17 });
    test_single_token!(unterminated_string, "\"unterminated", TokenKind::Literal { kind: LiteralKind::String { terminated: false }, suffix: 9 });
    test_single_token!(char_literal, "'c'", TokenKind::Literal { kind: LiteralKind::Char { terminated: true }, suffix: 3 });
    test_single_token!(unterminated_char_literal, "'c", TokenKind::Literal { kind: LiteralKind::Char { terminated: false }, suffix: 2 });
    test_single_token!(symbol, ";", TokenKind::Semi);
}
