use std::collections::HashMap;

use async_recursion::async_recursion;
use lazy_static::lazy_static;

use crate::{
    ast::{AnyAst, ExprAst, FunctionAst, PrototypeAst},
    lexer::{CharStream, Lexer, LexerError},
    token::Token,
};

use self::token_src::TokenSource;

mod token_src;

lazy_static! {
    /// - 1 is lowest precedence
    static ref BIN_OP_PRECEDENCE: HashMap<&'static str, u8> = {
        let mut m = HashMap::new();
        m.insert("<", 10);
        m.insert("+", 20);
        m.insert("-", 20);
        m.insert("*", 40);
        m
    };
}

pub struct Parser<'stream> {
    token_src: TokenSource<'stream>,
}

impl<'stream> Parser<'stream> {
    pub fn from_token_src(token_src: TokenSource<'stream>) -> Self {
        Self { token_src }
    }

    pub fn from_char_stream(char_stream: &'stream mut CharStream) -> Self {
        let lexer = Lexer::new(char_stream);
        Self::from_token_src(TokenSource::new(lexer))
    }

    /// - `numberexpr ::= number`
    async fn parse_number_expr(&mut self) -> Result<ExprAst, ParserError> {
        let Token::Number(_) = self.peek_token().await? else {
            return Err(ParserError::ParserError("Expected number.".to_string()));
        };
        let Token::Number(n) = self.take_token().await? else {
            unreachable!();
        };

        Ok(ExprAst::Number(n))
    }

    /// - `parenexpr ::= '(' expression ')'
    async fn parse_paren_expr(&mut self) -> Result<ExprAst, ParserError> {
        // eat '('
        let Token::Unknown('(') = self.peek_token().await? else {
            return Err(ParserError::ParserError("Expected '('.".to_string()));
        };
        self.take_token().await?;

        let expr = self.parse_expression().await?;

        // eat ')'
        let Token::Unknown(')') = self.peek_token().await? else {
            return Err(ParserError::ParserError("Expected ')'.".to_string()));
        };
        self.take_token().await?;

        Ok(expr)
    }

    /// - ```text
    ///   identifierexpr
    ///     ::= identifier
    ///     ::= identifier '(' (expression (',' expression)*)? ')'
    ///   ```
    async fn parse_identifier_expr(&mut self) -> Result<ExprAst, ParserError> {
        let Token::Identifier(_) = self.peek_token().await? else {
            return Err(ParserError::ParserError("Expected identifier.".to_string()));
        };
        let Token::Identifier(id_name) = self.take_token().await? else {
            unreachable!();
        };

        match self.peek_token().await? {
            // Call.
            Token::Unknown('(') => {
                // eat '('
                self.take_token().await?;

                let mut args = Vec::new();

                match self.peek_token().await? {
                    Token::Unknown(')') => {
                        // eat ')'
                        self.take_token().await?;
                    }
                    _ => loop {
                        args.push(self.parse_expression().await?);

                        if let Token::Unknown(')') = self.peek_token().await? {
                            // eat ')'
                            self.take_token().await?;
                            break;
                        }

                        // eat ','
                        let Token::Unknown(',') = self.peek_token().await? else {
                            return Err(ParserError::ParserError(
                                "Expected ')' or ',' in argument list.".to_string(),
                            ));
                        };
                        self.take_token().await?;
                    },
                };

                Ok(ExprAst::Call {
                    callee: id_name,
                    args,
                })
            }
            // Simple variable ref.
            _ => Ok(ExprAst::Variable(id_name)),
        }
    }

    /// - ```text
    ///   ifexpr ::= 'if' expression 'then' expression 'else' expression
    ///   ```
    async fn parse_if_expr(&mut self) -> Result<ExprAst, ParserError> {
        let Token::If = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `if` keyword.".to_string(),
            ));
        };
        // Eat `if`.
        self.take_token().await?;

        // Parse condition.
        let cond = self.parse_expression().await?;

        let Token::Then = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `then` keyword.".to_string(),
            ));
        };
        // Eat `then`.
        self.take_token().await?;

        // Parse then branch.
        let then = self.parse_expression().await?;

        let Token::Else = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `else` keyword.".to_string(),
            ));
        };
        // Eat `else`.
        self.take_token().await?;

        // Parse else branch.
        let else_ = self.parse_expression().await?;

        Ok(ExprAst::If {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
        })
    }

    /// - ```text
    ///   forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
    ///   ```
    async fn parse_for_expr(&mut self) -> Result<ExprAst, ParserError> {
        let Token::For = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `for` keyword.".to_string(),
            ));
        };
        // Eat `for`.
        self.take_token().await?;

        let Token::Identifier(_) = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected identifier after `for` keyword.".to_string(),
            ));
        };
        let Token::Identifier(var_name) = self.take_token().await? else {
            unreachable!();
        };

        let Token::Unknown('=') = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `=` after identifier.".to_string(),
            ));
        };
        // Eat `=`.
        self.take_token().await?;

        let start = self.parse_expression().await?;

        let Token::Unknown(',') = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `,` after start value.".to_string(),
            ));
        };
        // Eat `,`.
        self.take_token().await?;

        let end = self.parse_expression().await?;

        let step = match self.peek_token().await? {
            Token::Unknown(',') => {
                // Eat `,`.
                self.take_token().await?;

                let step = self.parse_expression().await?;

                Some(step)
            }
            _ => None,
        };

        let Token::In = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected `in` keyword after for-loop.".to_string(),
            ));
        };
        // Eat `in`.
        self.take_token().await?;

        let body = self.parse_expression().await?;

        Ok(ExprAst::For {
            var_name,
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            body: Box::new(body),
        })
    }

    /// - ```text
    ///   primary
    ///     ::= identifierexpr
    ///     ::= numberexpr
    ///     ::= parenexpr
    ///     ::= ifexpr
    ///     ::= forexpr
    ///   ```
    #[async_recursion]
    async fn parse_primary(&mut self) -> Result<ExprAst, ParserError> {
        match self.peek_token().await? {
            Token::Unknown('(') => self.parse_paren_expr().await,
            Token::Identifier(_) => self.parse_identifier_expr().await,
            Token::Number(_) => self.parse_number_expr().await,
            Token::If => self.parse_if_expr().await,
            Token::For => self.parse_for_expr().await,
            _ => Err(ParserError::ParserError(
                "Unknown token when expecting an expression.".to_string(),
            )),
        }
    }

    /// - ```text
    ///   expression
    ///     ::= primary binoprhs
    ///   ```
    #[async_recursion]
    async fn parse_expression(&mut self) -> Result<ExprAst, ParserError> {
        let lhs = self.parse_primary().await?;

        // 0: minimal operator precedence, so that every operator can be handled by this function.
        self.parse_bin_op_rhs(0, lhs).await
    }

    /// - ```text
    ///   binoprhs
    ///     ::= (binop primary)*
    ///   ```
    /// - Algorithm: precedence climbing
    ///   - Learn more: <https://ycpcs.github.io/cs340-fall2018/lectures/lecture06.html>
    /// - e.g. 1 < 2 < 3 < 4 + 5 * 6 + 7 + 8 * 9 < 10
    ///
    ///   ```text
    ///     prec_level
    ///      ^
    ///      |
    ///   40 |          *     *
    ///      |
    ///   20 |        +   + +
    ///   10 |
    ///    0 +  < < <           <
    ///      +--------------------> time
    ///   ```
    ///
    ///   - `prec_level`: the precedence level of the operator currently being handled.
    ///   - All operators at the same level groups a left-skewed binary tree.
    ///   - Every time the `prec_level` jumps to a higher level, a new left-skewed binary tree is created.
    ///     - The new tree will becomes the right branch of the original left-skewed binary tree.
    ///   - Why all `<` are at precedence level 0 rather than 10:
    ///     - The initial call of `parse_bin_op_rhs` is always from `parse_expression`, which always gives it a `prec_level` of 0.
    ///
    ///   ```text
    ///                    <
    ///        <            10
    ///      <         +
    ///    <  3      +   *
    ///   1 2    +    7 8 9
    ///         4  *
    ///           5 6
    ///   ```
    #[async_recursion]
    async fn parse_bin_op_rhs(
        &mut self,
        min_prec: u8,
        mut lhs: ExprAst,
    ) -> Result<ExprAst, ParserError> {
        loop {
            let Some((op, op_prec)) = self.try_peek_bin_op_and_prec().await? else {
                // This token is not a binary operator.
                return Ok(lhs);
            };

            // The operator of every iteration handled in this function MUST have a precedence AT LEAST as high as `min_prec`.
            if op_prec < min_prec {
                // Return to the previous level of precedence.
                // The caller has a lower precedence than that of the current function.
                return Ok(lhs);
            }

            // Eat binop.
            self.take_token().await?;

            let next_primary = self.parse_primary().await?;

            let Some((_, next_prec)) = self.try_peek_bin_op_and_prec().await? else {
                return Ok(ExprAst::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(next_primary),
                });
            };

            // RHS is either:
            let rhs = if op_prec < next_prec {
                // - a binary expression with higher precedence than `op`
                self.parse_bin_op_rhs(next_prec, next_primary).await?
            } else {
                // - a primary expression
                next_primary
            };

            // LHS is always the expression root of the previous iteration.

            // Merge lhs/rhs.
            lhs = ExprAst::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };

            //         /
            //        op
            //       /  \
            //     lhs  rhs
            //
            //         /
            //        op
            //       /  \
            //     op'  rhs
            //    /   \
            //  lhs' rhs'
            //
            // - `x'`: variables of the previous iteration
            // - `op' == lhs`
            // - `op` becomes the new `lhs`
        }
    }

    async fn try_peek_bin_op_and_prec(&mut self) -> Result<Option<(String, u8)>, ParserError> {
        match self.peek_token().await? {
            Token::Unknown(op) => {
                let op = op.to_string();

                if let Some(prec) = BIN_OP_PRECEDENCE.get(op.as_str()) {
                    Ok(Some((op, *prec)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    /// - ```text
    ///   prototype
    ///     ::= id '(' id* ')'
    ///   ```
    async fn parse_prototype(&mut self) -> Result<PrototypeAst, ParserError> {
        let Token::Identifier(_) = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected function name in prototype.".to_string(),
            ));
        };
        let Token::Identifier(name) = self.take_token().await? else {
            unreachable!();
        };

        // Eat '('.
        let Token::Unknown('(') = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected '(' in prototype.".to_string(),
            ));
        };
        self.take_token().await?;

        let mut args = Vec::new();

        loop {
            match self.peek_token().await? {
                Token::Unknown(')') => {
                    // Eat ')'.
                    self.take_token().await?;
                    break;
                }
                Token::Identifier(_) => {
                    let Token::Identifier(arg) = self.take_token().await? else {
                        unreachable!();
                    };
                    args.push(arg);
                }
                _ => {
                    return Err(ParserError::ParserError(
                        "Expected ')' or argument name in prototype.".to_string(),
                    ));
                }
            }
        }

        Ok(PrototypeAst { name, args })
    }

    /// - ```text
    ///   definition ::= 'def' prototype expression
    ///   ```
    pub async fn parse_definition(&mut self) -> Result<FunctionAst, ParserError> {
        // Eat 'def'.
        let Token::Def = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected 'def' keyword.".to_string(),
            ));
        };
        self.take_token().await?;

        let prototype = self.parse_prototype().await?;
        let body = self.parse_expression().await?;

        Ok(FunctionAst { prototype, body })
    }

    /// - ```text
    ///   external ::= 'extern' prototype
    ///   ```
    pub async fn parse_extern(&mut self) -> Result<PrototypeAst, ParserError> {
        // Eat 'extern'.
        let Token::Extern = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected 'extern' keyword.".to_string(),
            ));
        };
        self.take_token().await?;

        self.parse_prototype().await
    }

    /// - ```text
    ///   toplevelexpr ::= expression
    ///   ```
    pub async fn parse_top_level_expr(&mut self) -> Result<FunctionAst, ParserError> {
        let prototype = PrototypeAst {
            name: "".to_string(),
            args: Vec::new(),
        };
        let body = self.parse_expression().await?;

        Ok(FunctionAst { prototype, body })
    }

    pub async fn parse_semicolon(&mut self) -> Result<(), ParserError> {
        let Token::Unknown(';') = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected ';'.".to_string(),
            ));
        };
        self.take_token().await?;
        Ok(())
    }

    pub async fn parse_eof(&mut self) -> Result<(), ParserError> {
        let Token::EOF = self.peek_token().await? else {
            return Err(ParserError::ParserError(
                "Expected EOF.".to_string(),
            ));
        };
        self.take_token().await?;
        Ok(())
    }

    /// - ```text
    ///   any ::= definition | external | toplevelexpr | ';'
    ///   ```
    pub async fn parse_any(&mut self) -> Result<Option<AnyAst>, ParserError> {
        loop {
            match self.peek_token().await? {
                Token::Def => return Ok(Some(AnyAst::Function(self.parse_definition().await?))),
                Token::Extern => return Ok(Some(AnyAst::Prototype(self.parse_extern().await?))),
                Token::Unknown(';') => {
                    self.parse_semicolon().await?;
                }
                Token::EOF => {
                    self.parse_eof().await?;
                    return Ok(None);
                }
                _ => {
                    // Assume top-level expression.
                    return Ok(Some(AnyAst::Function(self.parse_top_level_expr().await?)));
                }
            }
        }
    }

    pub async fn skip_until_semicolon(&mut self) -> Result<(), ParserError> {
        loop {
            match self.peek_token().await? {
                Token::Unknown(';') => {
                    self.parse_semicolon().await?;
                    break;
                }
                Token::EOF => {
                    self.parse_eof().await?;
                    break;
                }
                _ => {
                    self.take_token().await?;
                }
            }
        }
        Ok(())
    }

    async fn take_token(&mut self) -> Result<Token, ParserError> {
        self.token_src
            .take()
            .await
            .map_err(|e| ParserError::LexerError(e))
    }

    async fn peek_token(&mut self) -> Result<&Token, ParserError> {
        self.token_src
            .peek()
            .await
            .map_err(|e| ParserError::LexerError(e))
    }
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    ParserError(String),
}

#[cfg(test)]
mod tests {
    use futures::{executor, stream};

    use super::*;

    #[test]
    fn test_parse_number_expr() {
        let mut char_stream = stream::iter("1.2".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_number_expr().await.unwrap();
            assert_eq!(expr, ExprAst::Number(1.2));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_paren_expr() {
        let mut char_stream = stream::iter("(1.2)".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_paren_expr().await.unwrap();
            assert_eq!(expr, ExprAst::Number(1.2));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_identifier_expr_1() {
        let mut char_stream = stream::iter("foo".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_identifier_expr().await.unwrap();
            assert_eq!(expr, ExprAst::Variable("foo".to_string()));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_identifier_expr_2_1() {
        let mut char_stream = stream::iter("foo()".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_identifier_expr().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::Call {
                    callee: "foo".to_string(),
                    args: vec![],
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_identifier_expr_2_2() {
        let mut char_stream = stream::iter("foo(1.2, x)".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_identifier_expr().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::Call {
                    callee: "foo".to_string(),
                    args: vec![ExprAst::Number(1.2), ExprAst::Variable("x".to_string())],
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_if_expr() {
        let mut char_stream = stream::iter("if 1.2 then 3.4 else 5.6".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_if_expr().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::If {
                    cond: Box::new(ExprAst::Number(1.2)),
                    then: Box::new(ExprAst::Number(3.4)),
                    else_: Box::new(ExprAst::Number(5.6)),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_for_expr_1() {
        let mut char_stream = stream::iter("for i = 1, i < 10 in 42".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_for_expr().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::For {
                    var_name: "i".to_string(),
                    start: Box::new(ExprAst::Number(1.0)),
                    end: Box::new(ExprAst::Binary {
                        op: "<".to_string(),
                        lhs: Box::new(ExprAst::Variable("i".to_string())),
                        rhs: Box::new(ExprAst::Number(10.0)),
                    }),
                    step: None,
                    body: Box::new(ExprAst::Number(42.0)),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_for_expr_2() {
        let mut char_stream = stream::iter("for i = 1, i < 10, 2 in 42".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_for_expr().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::For {
                    var_name: "i".to_string(),
                    start: Box::new(ExprAst::Number(1.0)),
                    end: Box::new(ExprAst::Binary {
                        op: "<".to_string(),
                        lhs: Box::new(ExprAst::Variable("i".to_string())),
                        rhs: Box::new(ExprAst::Number(10.0)),
                    }),
                    step: Some(Box::new(ExprAst::Number(2.0))),
                    body: Box::new(ExprAst::Number(42.0)),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_primary_expr_1() {
        let mut char_stream = stream::iter("1.2".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_primary().await.unwrap();
            assert_eq!(expr, ExprAst::Number(1.2));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_primary_expr_2() {
        let mut char_stream = stream::iter("(1.2)".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_primary().await.unwrap();
            assert_eq!(expr, ExprAst::Number(1.2));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_primary_expr_3() {
        let mut char_stream = stream::iter("foo".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_primary().await.unwrap();
            assert_eq!(expr, ExprAst::Variable("foo".to_string()));
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_bin_op_rhs_1() {
        let mut char_stream = stream::iter("1 * 2 + 3 * 4".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let lhs = parser.parse_number_expr().await.unwrap();
            let expr = parser.parse_bin_op_rhs(0, lhs).await.unwrap();
            assert_eq!(
                expr,
                ExprAst::Binary {
                    op: "+".to_string(),
                    lhs: Box::new(ExprAst::Binary {
                        op: "*".to_string(),
                        lhs: Box::new(ExprAst::Number(1.0)),
                        rhs: Box::new(ExprAst::Number(2.0)),
                    }),
                    rhs: Box::new(ExprAst::Binary {
                        op: "*".to_string(),
                        lhs: Box::new(ExprAst::Number(3.0)),
                        rhs: Box::new(ExprAst::Number(4.0)),
                    }),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_bin_op_rhs_2() {
        let mut char_stream = stream::iter("1 < 2 < 3 < 4 + 5 * 6 + 7 + 8 * 9 < 10".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let lhs = parser.parse_number_expr().await.unwrap();
            let expr = parser.parse_bin_op_rhs(0, lhs).await.unwrap();
            assert_eq!(
                expr,
                ExprAst::Binary {
                    op: "<".to_string(),
                    lhs: Box::new(ExprAst::Binary {
                        op: "<".to_string(),
                        lhs: Box::new(ExprAst::Binary {
                            op: "<".to_string(),
                            lhs: Box::new(ExprAst::Binary {
                                op: "<".to_string(),
                                lhs: Box::new(ExprAst::Number(1.0)),
                                rhs: Box::new(ExprAst::Number(2.0)),
                            }),
                            rhs: Box::new(ExprAst::Number(3.0)),
                        }),
                        rhs: Box::new(ExprAst::Binary {
                            op: "+".to_string(),
                            lhs: Box::new(ExprAst::Binary {
                                op: "+".to_string(),
                                lhs: Box::new(ExprAst::Binary {
                                    op: "+".to_string(),
                                    lhs: Box::new(ExprAst::Number(4.0)),
                                    rhs: Box::new(ExprAst::Binary {
                                        op: "*".to_string(),
                                        lhs: Box::new(ExprAst::Number(5.0)),
                                        rhs: Box::new(ExprAst::Number(6.0)),
                                    })
                                }),
                                rhs: Box::new(ExprAst::Number(7.0)),
                            }),
                            rhs: Box::new(ExprAst::Binary {
                                op: "*".to_string(),
                                lhs: Box::new(ExprAst::Number(8.0)),
                                rhs: Box::new(ExprAst::Number(9.0)),
                            })
                        }),
                    }),
                    rhs: Box::new(ExprAst::Number(10.0)),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_prototype_1() {
        let mut char_stream = stream::iter("foo()".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let proto = parser.parse_prototype().await.unwrap();
            assert_eq!(
                proto,
                PrototypeAst {
                    name: "foo".to_string(),
                    args: vec![],
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_prototype_2() {
        let mut char_stream = stream::iter("foo(x y)".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let proto = parser.parse_prototype().await.unwrap();
            assert_eq!(
                proto,
                PrototypeAst {
                    name: "foo".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_definition() {
        let mut char_stream = stream::iter("def foo() 1".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let def = parser.parse_definition().await.unwrap();
            assert_eq!(
                def,
                FunctionAst {
                    prototype: PrototypeAst {
                        name: "foo".to_string(),
                        args: vec![],
                    },
                    body: ExprAst::Number(1.0),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_extern() {
        let mut char_stream = stream::iter("extern foo()".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let proto = parser.parse_extern().await.unwrap();
            assert_eq!(
                proto,
                PrototypeAst {
                    name: "foo".to_string(),
                    args: vec![],
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_parse_top_level_expr() {
        let mut char_stream = stream::iter("1".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_top_level_expr().await.unwrap();
            assert_eq!(
                expr,
                FunctionAst {
                    prototype: PrototypeAst {
                        name: "".to_string(),
                        args: vec![],
                    },
                    body: ExprAst::Number(1.0),
                }
            );
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }

    #[test]
    fn test_escape_expression() {
        let mut char_stream = stream::iter("1 + 2;".chars());
        let mut parser = Parser::from_char_stream(&mut char_stream);

        let task = async {
            let expr = parser.parse_expression().await.unwrap();
            assert_eq!(
                expr,
                ExprAst::Binary {
                    op: "+".to_string(),
                    lhs: Box::new(ExprAst::Number(1.0)),
                    rhs: Box::new(ExprAst::Number(2.0)),
                }
            );
            parser.parse_semicolon().await.unwrap();
            parser.parse_eof().await.unwrap();
        };

        executor::block_on(task);
    }
}
