use crate::token::Token;

#[derive(Debug, Default)]
pub struct TokenStream {
    tokens: Vec<Token>
}

impl TokenStream {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }
}
