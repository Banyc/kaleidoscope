use crate::{
    lexer::{Lexer, LexerError},
    token::Token,
};

pub struct TokenSource<'stream> {
    token: Option<Token>,
    lexer: Lexer<'stream>,
}

impl<'stream> TokenSource<'stream> {
    pub fn new(lexer: Lexer<'stream>) -> Self {
        Self { lexer, token: None }
    }

    async fn next_token(&mut self) -> Result<(), LexerError> {
        self.token = Some(self.lexer.get_token().await?);
        Ok(())
    }

    async fn fill_token_slot(&mut self) -> Result<(), LexerError> {
        if self.token.is_none() {
            self.next_token().await?;
        }
        Ok(())
    }

    pub async fn take(&mut self) -> Result<Token, LexerError> {
        self.fill_token_slot().await?;

        Ok(self.token.take().unwrap())
    }

    pub async fn peek(&mut self) -> Result<&Token, LexerError> {
        self.fill_token_slot().await?;

        Ok(self.token.as_ref().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use futures::stream::iter;

    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_token_source() {
        let task = async {
            let mut char_stream = iter("1.0 + 2.0".chars());
            let lexer = Lexer::new(&mut char_stream);
            let mut token_src = TokenSource::new(lexer);

            assert_eq!(token_src.take().await.unwrap(), Token::Number(1.0));
            assert_eq!(token_src.peek().await.unwrap(), &Token::Unknown('+'));
            assert_eq!(token_src.take().await.unwrap(), Token::Unknown('+'));
            assert_eq!(token_src.take().await.unwrap(), Token::Number(2.0));
            assert_eq!(token_src.peek().await.unwrap(), &Token::EOF);
            assert_eq!(token_src.take().await.unwrap(), Token::EOF);
            assert_eq!(token_src.take().await.unwrap(), Token::EOF);
        };

        futures::executor::block_on(task);
    }
}
