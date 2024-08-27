use crate::token::Token;


#[derive(Debug, Default)]
pub struct TokenStream {
    tokens: Vec<Token>
}

impl TokenStream {
    pub fn new() -> Self {
        Self::default()
    }
}
