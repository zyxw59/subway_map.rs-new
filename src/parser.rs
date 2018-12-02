use std::error;
use std::io::BufRead;

use expressions::Expression;
use lexer::{Lexer, Token};
use operators::BinaryOperator;

type Error = Box<dyn error::Error>;
type EResult<T> = Result<T, Error>;

pub trait Parser: Iterator<Item = EResult<Token>> {
    fn put_back(&mut self, put_back: Token);

    fn parse_expression(&mut self) -> EResult<Expression> {
        let lhs = self.parse_primary()?;
        self.parse_expression_1(lhs, 0)
    }

    fn parse_expression_1(
        &mut self,
        mut lhs: Expression,
        min_precedence: usize,
    ) -> EResult<Expression> {
        // we start with the given left hand value.
        // as long as we encounter operators with precedence >= min_precedence, we can accumulate
        // them into that value.
        while let Some(tok) = try_opt!(self.next()) {
            if let Some(op) = BinaryOperator::from_builtin(&tok, min_precedence) {
                // we have an operator; now to get the right hand side
                let mut rhs = self.parse_primary()?;
                // and check for operators with greater precedence to accumulate onto the rhs
                while let Some(tok) = try_opt!(self.next()) {
                    if let Some(op) = BinaryOperator::from_builtin(&tok, op.precedence + 1) {
                        // put the operator back so that we can match it in the recursive call to
                        // `parse_expression_1`
                        self.put_back(tok);
                        rhs = self.parse_expression_1(rhs, op.precedence)?;
                    } else {
                        self.put_back(tok);
                        break;
                    }
                }
                // we've accumulated the maximal rhs, now apply the operation to get our new lhs
                lhs = op.apply(lhs, rhs)?;
            } else {
                self.put_back(tok);
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_primary(&mut self) -> EResult<Expression> {
        match try_opt!(self.next()) {
            None => Err("Unexpected end of input")?,
            Some(Token::LeftParen) => unimplemented!(),
            Some(Token::Number(num)) => Ok(Expression::Number(num)),
            Some(Token::Tag(_)) => unimplemented!(),
            _ => Err("Unexpected token")?,
        }
    }
}

impl<R: BufRead> Parser for Lexer<R> {
    fn put_back(&mut self, put_back: Token) {
        self.put_back(put_back)
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
}