use std::io::BufRead;

use error::{ParserError, Result as EResult, Type};
use expressions::Expression;
use lexer::{Lexer, Token};
use operators::{BinaryOperator, UnaryOperator};

pub trait Parser: Iterator<Item = EResult<Token>> {
    fn put_back(&mut self, put_back: Token);

    fn line(&self) -> usize;

    fn parse_expression(&mut self) -> EResult<Expression> {
        self.parse_expression_1(0)
    }

    fn parse_expression_1(&mut self, min_precedence: usize) -> EResult<Expression> {
        // initial left-hand side
        let mut lhs = self.parse_primary()?;
        // as long as we encounter operators with precedence >= min_precedence, we can accumulate
        // them into `lhs`.
        while let Some(tok) = try_opt!(self.next()) {
            if let Some(op) =
                BinaryOperator::from_builtin(&tok).filter(|op| op.precedence >= min_precedence)
            {
                // we have an operator; now to get the right hand side, accumulating operators with
                // greater precedence than the current one
                let rhs = self.parse_expression_1(op.precedence + 1)?;
                lhs = op
                    .apply(lhs, rhs)
                    .map_err(|err| ParserError::Math(err, self.line()))?;
            } else {
                // the token wasn't an operator, so put it back on the stack
                self.put_back(tok);
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_primary(&mut self) -> EResult<Expression> {
        match try_opt!(self.next()) {
            None => Err(ParserError::EndOfInput(self.line()))?,
            Some(Token::LeftParen) => self.parse_parentheses(),
            Some(Token::Number(num)) => Ok(Expression::Number(num)),
            Some(tok) => self.parse_unary(tok),
        }
    }

    fn parse_parentheses(&mut self) -> EResult<Expression> {
        let start_line = self.line();
        let x = self.parse_expression()?;
        match try_opt!(self.next()) {
            Some(Token::RightParen) => Ok(x),
            Some(Token::Comma) => {
                let y = self.parse_expression()?;
                match try_opt!(self.next()) {
                    Some(Token::RightParen) => {
                        use self::Expression::*;
                        match (x, y) {
                            (Number(x), Number(y)) => Ok(Point(x, y)),
                            (Number(_), y) => {
                                Err(ParserError::type_error(Type::Number, y.into(), self.line()))?
                            }
                            (x, _) => {
                                Err(ParserError::type_error(Type::Number, x.into(), self.line()))?
                            }
                        }
                    }
                    Some(_) => Err(ParserError::Token(self.line()))?,
                    None => Err(ParserError::Parentheses(start_line))?,
                }
            }
            Some(_) => Err(ParserError::Token(self.line()))?,
            None => Err(ParserError::Parentheses(start_line))?,
        }
    }

    fn parse_unary(&mut self, token: Token) -> EResult<Expression> {
        if let Some(op) = UnaryOperator::from_builtin(&token) {
            let arg = self.parse_expression_1(op.precedence)?;
            op.apply(arg)
                .map_err(|err| ParserError::Math(err, self.line()).into())
        } else {
            Err(ParserError::Token(self.line()))?
        }
    }
}

impl<R: BufRead> Parser for Lexer<R> {
    fn put_back(&mut self, put_back: Token) {
        self.put_back(put_back)
    }

    fn line(&self) -> usize {
        self.line()
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Lexer, Parser};

    #[test]
    fn basic_arithmetic() {
        let result = Lexer::new("1+2*3+4".as_bytes()).parse_expression().unwrap();
        assert_eq!(result, Expression::Number(11.0));
    }

    #[test]
    fn basic_arithmetic_2() {
        let result = Lexer::new("1-2*3+4".as_bytes()).parse_expression().unwrap();
        assert_eq!(result, Expression::Number(-1.0));
    }

    #[test]
    fn basic_arithmetic_3() {
        let result = Lexer::new("1-3/2*5".as_bytes()).parse_expression().unwrap();
        assert_eq!(result, Expression::Number(-6.5));
    }

    #[test]
    fn parentheses() {
        let result = Lexer::new("(1+2)*3+4".as_bytes())
            .parse_expression()
            .unwrap();
        assert_eq!(result, Expression::Number(13.0));
    }

    #[test]
    fn points() {
        let result = Lexer::new("(1,2) + (3,4)".as_bytes())
            .parse_expression()
            .unwrap();
        assert_eq!(result, Expression::Point(4.0, 6.0));
    }

    #[test]
    fn dot_product() {
        let result = Lexer::new("(1,2) * (3,4)".as_bytes())
            .parse_expression()
            .unwrap();
        assert_eq!(result, Expression::Number(11.0));
    }

    #[test]
    fn scalar_product() {
        let result = Lexer::new("3 * (1,2)".as_bytes())
            .parse_expression()
            .unwrap();
        assert_eq!(result, Expression::Point(3.0, 6.0));
    }

    #[test]
    fn unary_minus() {
        let result = Lexer::new("3* -2".as_bytes()).parse_expression().unwrap();
        assert_eq!(result, Expression::Number(-6.0));
    }

    #[test]
    fn unary_minus_2() {
        let result = Lexer::new("-2*3".as_bytes()).parse_expression().unwrap();
        assert_eq!(result, Expression::Number(-6.0));
    }

    #[test]
    fn unary_minus_3() {
        let result = Lexer::new("-(1,2)*(3,4)".as_bytes())
            .parse_expression()
            .unwrap();
        assert_eq!(result, Expression::Number(-11.0));
    }
}
