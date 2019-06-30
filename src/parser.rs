use std::collections::{hash_map::Entry, HashMap};

use crate::error::{ParserError, Result as EResult};
use crate::expressions::{Expression, Function, Variable};
use crate::lexer::Token;
use crate::operators::{BinaryBuiltins, UnaryBuiltins};
use crate::tables::{Table, TableMut};
use crate::values::Value;

pub trait ParserExt: Iterator<Item = EResult<Token>> {
    fn put_back(&mut self, put_back: Token);

    fn line(&self) -> usize;

    fn into_parser(self) -> Parser<Self, HashMap<Variable, Value>, HashMap<Variable, Function>>
    where
        Self: Sized,
    {
        Parser {
            tokens: self,
            variables: HashMap::<Variable, Value>::new(),
            functions: HashMap::<Variable, Function>::new(),
        }
    }
}

impl<'a, T> ParserExt for &'a mut T
where
    T: ParserExt,
{
    fn put_back(&mut self, put_back: Token) {
        (**self).put_back(put_back);
    }

    fn line(&self) -> usize {
        (**self).line()
    }
}

macro_rules! expect {
    ($self:ident, $token:pat) => {
        expect!($self, $token, ())
    };
    ($self:ident, $token:pat, $capture:expr) => {
        match try_opt!($self.next()) {
            Some($token) => $capture,
            Some(tok) => Err(ParserError::Token(tok, $self.line()))?,
            None => Err(ParserError::EndOfInput($self.line()))?,
        }
    };
}

pub struct Parser<T, V, F> {
    tokens: T,
    variables: V,
    functions: F,
}

impl<T, V, F> Parser<T, V, F>
where
    T: ParserExt,
    V: Table<Variable, Value>,
    F: Table<Variable, Function>,
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

    /// Returns a parser in which all variables are undefined, in order to produce an expression
    /// tree that can be evaluated with a different set of variable values than the current one.
    fn function_def_parser(&mut self) -> Parser<&mut T, (), ()> {
        Parser {
            tokens: &mut self.tokens,
            variables: (),
            functions: (),
        }
    }

    pub fn parse_value(&mut self) -> EResult<Value> {
        self.parse_expression_1(0).and_then(|exp| {
            exp.evaluate(&self.variables, &self.functions)
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
            Some(Token::Tag(tag)) => match UnaryBuiltins.get(&tag) {
                Some(op) => op
                    .expression(self.parse_expression_1(op.precedence)?)
                    .map_err(|err| ParserError::Math(err, self.line()).into()),
                None => {
                    let next_tok = try_opt!(self.next());
                    if let Some(Token::LeftParen) = next_tok {
                        // function call
                        let start_line = self.line();
                        let args = self.parse_comma_list()?;
                        match try_opt!(self.next()) {
                            Some(Token::RightParen) => {}
                            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                            None => Err(ParserError::Parentheses(start_line))?,
                        };
                        Ok(Expression::Function(tag, args))
                    } else {
                        if let Some(tok) = next_tok {
                            self.put_back(tok);
                        }
                        match self.variables.get(&tag) {
                            Some(&value) => Ok(Expression::Value(value)),
                            None => Ok(Expression::Variable(tag)),
                        }
                    }
                }
            },
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
        }
    }

    fn parse_parentheses(&mut self) -> EResult<Expression> {
        let start_line = self.line();
        let mut list = self.parse_comma_list()?;
        let exp = match list.len() {
            1 => list.pop().unwrap(),
            2 => {
                let y = list.pop().unwrap();
                let x = list.pop().unwrap();
                Expression::point(x, y).map_err(|err| ParserError::Math(err, self.line()))?
            }
            n => Err(ParserError::ParenList(n, start_line))?,
        };
        match try_opt!(self.next()) {
            Some(Token::RightParen) => Ok(exp),
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
            None => Err(ParserError::Parentheses(start_line))?,
        }
    }

    fn parse_comma_list(&mut self) -> EResult<Vec<Expression>> {
        let mut list = Vec::new();
        while let Some(tok) = try_opt!(self.next()) {
            match tok {
                Token::Comma => {}
                Token::RightParen => {
                    self.put_back(tok);
                    break;
                }
                _ => self.put_back(tok),
            }
            list.push(self.parse_expression()?);
        }
        Ok(list)
    }

    /// Parses a function definition.
    ///
    /// On success, returns a tuple of the function name and the function definition.
    fn parse_function_def(&mut self) -> EResult<(String, Function)> {
        let tag = expect!(self, Token::Tag(tag), tag);
        expect!(self, Token::LeftParen);
        // maps argument names to their index in the function signature
        let mut args = HashMap::new();
        let mut index = 0;
        let start_line = self.line();
        loop {
            match try_opt!(self.next()) {
                // a named argument
                Some(Token::Tag(arg)) => {
                    // insert the new argument into the hashmap
                    match args.entry(arg) {
                        // there's already an argument with this name
                        Entry::Occupied(e) => {
                            return Err(ParserError::Argument(
                                e.remove_entry().0,
                                tag,
                                self.line(),
                            )
                            .into());
                        }
                        Entry::Vacant(e) => e.insert(index),
                    };
                    index += 1;
                    match try_opt!(self.next()) {
                        Some(Token::Comma) => {}
                        Some(Token::RightParen) => break,
                        Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                        None => Err(ParserError::Parentheses(start_line))?,
                    }
                }
                // end of the arguments
                Some(Token::RightParen) => break,
                Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                None => Err(ParserError::Parentheses(start_line))?,
            }
        }
        expect!(self, Token::Equal);
        // get the function body, as an expression tree
        let expression = self.function_def_parser().parse_expression()?;
        Ok((tag, Function { args, expression }))
    }
}

impl<T, V, F> Parser<T, V, F>
where
    T: ParserExt,
    V: TableMut<Variable, Value>,
    F: TableMut<Variable, Function>,
{
    /// Parse and evaluate a statement.
    ///
    /// The `Ok` value indicates whether there are more statements remaining (i.e. whether this
    /// statement ended with a `;`).
    fn parse_statement(&mut self) -> EResult<bool> {
        match try_opt!(self.next()) {
            // tag; start of an assignment expression or function definition
            Some(Token::Tag(tag)) => {
                match tag.as_ref() {
                    // function definition
                    "fn" => {
                        let (func_name, func) = self.parse_function_def()?;
                        self.functions.insert(func_name, func);
                    }
                    // single point
                    "point" => {
                        unimplemented!();
                    }
                    // sequence of points
                    "points" => {
                        unimplemented!();
                    }
                    // line
                    "line" => {
                        unimplemented!();
                    }
                    // stop
                    "stop" => {
                        unimplemented!();
                    }
                    // other (variable assignment)
                    _ => {
                        expect!(self, Token::Equal);
                        let value = self.parse_value()?;
                        self.variables.insert(tag, value);
                    }
                }
                //match try_opt!(self.next()) {
                //    // assignment
                //    Some(Token::Equal) => {
                //        let value = self.parse_value()?;
                //        self.variables.insert(tag, value);
                //    }
                //    // function definition
                //    Some(Token::LeftParen) => {
                //        let (tag, func) = self.parse_function_def()?;
                //        self.functions.insert(tag, func);
                //    }
                //    // other token; unexpected
                //    Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                //    // unexpected end of input
                //    None => Err(ParserError::EndOfInput(self.line()))?,
                //};
                match try_opt!(self.next()) {
                    // end of statement
                    Some(Token::Semicolon) => Ok(true),
                    // end of input (also acceptable)
                    None => Ok(false),
                    // other token; unexpected
                    Some(tok) => Err(ParserError::Token(tok, self.line()))?,
                }
            }
            // other token; unexpected
            Some(tok) => Err(ParserError::Token(tok, self.line()))?,
            // empty statement, end of input
            None => Ok(false),
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
    fn hypot() {
        let result = Lexer::new("3++4".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn hypot_sub() {
        let result = Lexer::new("5+-+3".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(4.0));
    }

    #[test]
    fn pow() {
        let result = Lexer::new("3^4".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(81.0));
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
    fn angle() {
        let result = Lexer::new("angle (3, 3)".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(45.0));
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

    #[test]
    fn unary_cos() {
        let result = Lexer::new("cos 90".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(0.0));
    }

    #[test]
    fn unary_sin() {
        let result = Lexer::new("sin 90".as_bytes())
            .into_parser()
            .parse_value()
            .unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn variables_set() {
        let mut parser = Lexer::new("x = 1".as_bytes()).into_parser();
        assert!(!parser.parse_statement().unwrap());
        assert_eq!(parser.variables.get("x"), Some(&Value::Number(1.0)));
    }

    #[test]
    fn variables_get() {
        let mut parser = Lexer::new("x = 1; z = x * 2".as_bytes()).into_parser();
        while parser.parse_statement().unwrap() {}
        assert_eq!(parser.variables.get("x"), Some(&Value::Number(1.0)));
        assert_eq!(parser.variables.get("z"), Some(&Value::Number(2.0)));
    }

    #[test]
    fn functions() {
        let mut parser = Lexer::new("fn f(x) = x + 1; y = f(3)".as_bytes()).into_parser();
        while parser.parse_statement().unwrap() {}
        assert_eq!(parser.variables.get("y"), Some(&Value::Number(4.0)));
    }

    #[test]
    fn functions_2() {
        let mut parser = Lexer::new("fn f(x, y) = x * y; z = f(3, 2)".as_bytes()).into_parser();
        while parser.parse_statement().unwrap() {}
        assert_eq!(parser.variables.get("z"), Some(&Value::Number(6.0)));
    }
}
