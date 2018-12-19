use std::collections::HashMap;

use crate::error::{ParserError, Result as EResult};
use crate::expressions::Expression;
use crate::lexer::Token;
use crate::operators::{BinaryBuiltins, UnaryBuiltins};
use crate::tables::{Table, TableMut};
use crate::values::Value;

pub trait ParserExt: Iterator<Item = EResult<Token>> {
    fn put_back(&mut self, put_back: Token);

    fn line(&self) -> usize;

    fn into_parser(self) -> Parser<Self, HashMap<String, Value>>
    where
        Self: Sized,
    {
        Parser {
            tokens: self,
            variables: HashMap::<String, Value>::new(),
        }
    }
}

pub struct Parser<T, V> {
    tokens: T,
    variables: V,
}

impl<T, V> Parser<T, V>
where
    T: ParserExt,
    V: TableMut<String, Value>,
{
    fn next(&mut self) -> Option<EResult<Token>> {
        self.tokens.next()
    }

    fn put_back(&mut self, put_back: Token) {
        self.tokens.put_back(put_back);
    }

    fn line(&self) -> usize {
        self.tokens.line()
    }

    fn parse_statement(&mut self) -> EResult<()> {
        match try_opt!(self.next()) {
            // tag; start of an assignment expression or function definition
            Some(Token::Tag(tag)) => unimplemented!(),
            // other token; unexpected
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
            // empty statement, do nothing
            None => {}
        };
        Ok(())
    }

    pub fn parse_value(&mut self) -> EResult<Value> {
        self.parse_expression_1(0).and_then(|exp| {
            exp.evaluate(&self.variables)
                .map_err(|err| ParserError::Math(err, self.line()).into())
        })
    }

    fn parse_expression(&mut self) -> EResult<Expression> {
        self.parse_expression_1(0)
    }

    fn parse_expression_1(&mut self, min_precedence: usize) -> EResult<Expression> {
        // initial left-hand side
        let mut lhs = self.parse_primary()?;
        // as long as we encounter operators with precedence >= min_precedence, we can accumulate
        // them into `lhs`.
        while let Some(tok) = try_opt!(self.next()) {
            if let Some(op) = tok
                .as_tag()
                .and_then(|tag| BinaryBuiltins.get(&tag))
                .filter(|op| op.precedence >= min_precedence)
            {
                // we have an operator; now to get the right hand side, accumulating operators with
                // greater precedence than the current one
                let rhs = self.parse_expression_1(op.precedence + 1)?;
                lhs = op
                    .expression(lhs, rhs)
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
            Some(Token::Number(num)) => Ok(Expression::Value(Value::Number(num))),
            Some(Token::Tag(tag)) => match self.variables.get(&tag) {
                Some(&exp) => Ok(Expression::Value(exp)),
                None => self.parse_unary(tag),
            },
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
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
                    Some(Token::RightParen) => Expression::point(x, y)
                        .map_err(|err| ParserError::Math(err, self.line()).into()),
                    Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                    None => Err(ParserError::Parentheses(start_line))?,
                }
            }
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
            None => Err(ParserError::Parentheses(start_line))?,
        }
    }

    fn parse_unary(&mut self, tag: String) -> EResult<Expression> {
        if let Some(op) = UnaryBuiltins.get(&tag) {
            let arg = self.parse_expression_1(op.precedence)?;
            op.expression(arg)
                .map_err(|err| ParserError::Math(err, self.line()).into())
        } else {
            Err(ParserError::Token(Token::Tag(tag), self.line()))?
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::values::Value;

    use super::ParserExt;

    #[test]
    fn basic_arithmetic() {
        let result = Lexer::new("1+2*3+4".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(11.0));
    }

    #[test]
    fn basic_arithmetic_2() {
        let result = Lexer::new("1-2*3+4".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(-1.0));
    }

    #[test]
    fn basic_arithmetic_3() {
        let result = Lexer::new("1-3/2*5".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(-6.5));
    }

    #[test]
    fn parentheses() {
        let result = Lexer::new("(1+2)*3+4".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(13.0));
    }

    #[test]
    fn points() {
        let result = Lexer::new("(1,2) + (3,4)".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Point(4.0, 6.0));
    }

    #[test]
    fn dot_product() {
        let result = Lexer::new("(1,2) * (3,4)".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(11.0));
    }

    #[test]
    fn scalar_product() {
        let result = Lexer::new("3 * (1,2)".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Point(3.0, 6.0));
    }

    #[test]
    fn unary_minus() {
        let result = Lexer::new("3* -2".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(-6.0));
    }

    #[test]
    fn unary_minus_2() {
        let result = Lexer::new("-2*3".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(-6.0));
    }

    #[test]
    fn unary_minus_3() {
        let result = Lexer::new("-(1,2)*(3,4)".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(-11.0));
    }
}
