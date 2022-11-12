use async_recursion::async_recursion;
use futures::{Stream, StreamExt};

use crate::token::Token;

pub type CharStream = dyn Stream<Item = char> + Unpin + Send;

/// - Learn more: <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html#the-lexer>
pub struct Lexer<'stream> {
    source: &'stream mut CharStream,

    /// The current character emitted by the source stream.
    char: char,
}

impl<'stream> Lexer<'stream> {
    pub fn new(source: &'stream mut CharStream) -> Self {
        Self { source, char: ' ' }
    }

    #[async_recursion]
    pub async fn get_token(&mut self) -> Result<Token, LexerError> {
        // Skip any whitespace.
        while self.char.is_whitespace() {
            self.next_char().await;
        }

        // Collect the identifier.
        if self.char.is_alphabetic() {
            let mut identifier = String::new();
            identifier.push(self.char);

            loop {
                self.next_char().await;
                if self.char.is_alphanumeric() {
                    identifier.push(self.char);
                } else {
                    break;
                }
            }

            return Ok(match identifier.as_str() {
                "def" => Token::Def,
                "extern" => Token::Extern,
                "if" => Token::If,
                "then" => Token::Then,
                "else" => Token::Else,
                "for" => Token::For,
                "in" => Token::In,
                _ => Token::Identifier(identifier),
            });
        }

        // Collect the number.
        if self.char.is_numeric() || self.char == '.' {
            let mut number = String::new();
            number.push(self.char);

            loop {
                self.next_char().await;
                if self.char.is_numeric() || self.char == '.' {
                    number.push(self.char);
                } else {
                    break;
                }
            }

            match number.parse::<f64>() {
                Ok(number) => return Ok(Token::Number(number)),
                Err(_) => return Err(LexerError::ParseFloatError),
            }
        }

        // Skip comments.
        if self.char == '#' {
            loop {
                self.next_char().await;
                if self.char == '\r' || self.char == '\n' || self.char == '\0' {
                    break;
                }
            }

            return self.get_token().await;
        }

        // Collect EOF.
        if self.char == '\0' {
            return Ok(Token::EOF);
        }

        // Collect unknown token.
        let unknown = self.char;
        self.next_char().await;
        Ok(Token::Unknown(unknown))
    }

    async fn next_char(&mut self) {
        match self.source.next().await {
            Some(char) => self.char = char,
            None => self.char = '\0',
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    ParseFloatError,
}

#[cfg(test)]
mod tests {
    use futures::stream::iter;

    use super::*;

    #[test]
    fn test_lexer() {
        let task = async {
            let mut source = iter("extern def(x0) foo 1.2 # comment \n".chars());
            let mut lexer = Lexer::new(&mut source);

            assert_eq!(lexer.get_token().await.unwrap(), Token::Extern);
            assert_eq!(lexer.get_token().await.unwrap(), Token::Def);
            assert_eq!(lexer.get_token().await.unwrap(), Token::Unknown('('));
            assert_eq!(
                lexer.get_token().await.unwrap(),
                Token::Identifier("x0".to_string())
            );
            assert_eq!(lexer.get_token().await.unwrap(), Token::Unknown(')'));
            assert_eq!(
                lexer.get_token().await.unwrap(),
                Token::Identifier("foo".to_string())
            );
            assert_eq!(lexer.get_token().await.unwrap(), Token::Number(1.2));
            assert_eq!(lexer.get_token().await.unwrap(), Token::EOF);
        };

        futures::executor::block_on(task);
    }
}
