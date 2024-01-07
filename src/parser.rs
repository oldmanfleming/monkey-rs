#![allow(dead_code)]

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.cur_token().is_some() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(Program { statements })
    }

    fn cur_token(&mut self) -> Option<Token> {
        self.cur_token.clone()
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.peek_token.clone()
    }

    fn next_token(&mut self) -> &mut Self {
        self.cur_token = self.peek_token();
        self.peek_token = self.lexer.next_token();
        self
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token().ok_or("no token found")? {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name = match self
            .next_token()
            .cur_token()
            .ok_or(format!("no token found"))?
        {
            Token::Ident(value) => Expression::Identifier(value),
            token => Err(format!("expected identifier, found {token}"))?,
        };

        match self
            .next_token()
            .cur_token()
            .ok_or(format!("no token found"))?
        {
            Token::Assign => (),
            token => Err(format!("expected assign, found {token}"))?,
        }

        // TODO: skip expresion for now
        while self
            .cur_token()
            .is_some_and(|token| token.variant_eq(Token::Semicolon).is_err())
        {
            self.next_token();
        }
        let value = Expression::Identifier("TODO".to_string());
        self.next_token();
        // END TODO

        Ok(Statement::Let { name, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        // TODO: skip expresion for now
        while self
            .cur_token()
            .is_some_and(|token| token.variant_eq(Token::Semicolon).is_err())
        {
            self.next_token();
        }
        let value = Expression::Identifier("TODO".to_string());
        self.next_token();
        // END TODO

        Ok(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Expression(expression);

        if self
            .cur_token()
            .is_some_and(|token| !token.variant_eq(Token::Semicolon).is_err())
        {
            self.next_token();
        }

        Ok(statement)
    }

    // TODO: wip
    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, String> {
        let cur_token = self.cur_token().ok_or(format!("no token found"))?;

        let prefix = match cur_token.clone() {
            Token::Ident(value) => Some(self.parse_identifier(value)),
            Token::Int(value) => Some(self.parse_integer_literal(value)?),
            Token::Bang | Token::Minus => Some(self.parse_prefix_expression(cur_token.clone())?),
            _ => None,
        };

        self.next_token();

        match prefix {
            Some(prefix) => Ok(prefix),
            // TODO: here we would do the infix parsing? since we don't have a prefix function for the token
            None => Err(format!("no prefix parse function for {cur_token}"))?,
        }
    }

    fn parse_identifier(&mut self, value: String) -> Expression {
        Expression::Identifier(value)
    }

    fn parse_integer_literal(&mut self, literal: String) -> Result<Expression, String> {
        let value = literal
            .parse::<i64>()
            .map_err(|err| format!("could not parse integer literal as i64: {err}"))?;
        Ok(Expression::IntegerLiteral(value))
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression, String> {
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix {
            operator: token,
            right: Box::new(right),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        ast::{Expression, Statement},
        token::Token,
    };

    use super::*;

    #[test]
    fn test_let_statements() {
        let program = get_program(
            r#"
                let x = 5;
                let y = 10;
                let foobar = 838383;
            "#,
        );
        assert_eq!(program.statements.len(), 3);
        let cases = vec![("x", "5"), ("y", "10"), ("foobar", "838383")];
        let mut statements = program.statements.iter();
        for (name, value) in cases {
            let statement = statements.next().unwrap();
            assert_let_statement(statement, name, value);
        }
    }

    #[test]
    fn test_return_statements() {
        let program = get_program(
            r#"
                return 5;
                return 10;
                return 993322;
            "#,
        );
        assert_eq!(program.statements.len(), 3);
        let cases = vec!["5", "10", "993322"];
        let mut statements = program.statements.iter();
        for value in cases {
            let statement = statements.next().unwrap();
            assert_return_statement(statement, value);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let program = get_program("foobar;");
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        assert_expression_statement(statement, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let program = get_program("5;");
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        assert_expression_statement(statement, "5");
    }

    #[test]
    fn test_prefix_operators() {
        let cases = vec![
            ("!5;", Token::Bang, Expression::IntegerLiteral(5)),
            ("-15;", Token::Minus, Expression::IntegerLiteral(15)),
        ];
        for (input, expected_operator, expected_int) in cases {
            let program = get_program(input);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.first().unwrap();
            let expr = match statement {
                Statement::Expression(expression) => expression,
                _ => panic!("expected expression statement, found {statement}"),
            };
            match expr {
                Expression::Prefix { operator, right } => {
                    assert_eq!(*operator, expected_operator);
                    assert_eq!(**right, expected_int);
                }
                _ => panic!("expected prefix expression, found {expr}"),
            }
        }
    }

    // #[test]
    // fn parse_infix_expressions() {
    //     let cases = vec![
    //         (
    //             "5 + 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Plus,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 - 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Minus,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 * 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Asterisk,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 / 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Slash,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 > 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Gt,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 < 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Lt,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 == 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::Eq,
    //             Expression::IntegerLiteral(5),
    //         ),
    //         (
    //             "5 != 5;",
    //             Expression::IntegerLiteral(5),
    //             Token::NotEq,
    //             Expression::IntegerLiteral(5),
    //         ),
    //     ];
    //     for (input, expected_left, expected_operator, expected_right) in cases {
    //         let program = get_program(input);
    //         assert_eq!(program.statements.len(), 1);
    //         let statement = program.statements.first().unwrap();
    //         let expr = match statement {
    //             Statement::Expression(expression) => expression,
    //             _ => panic!("expected expression statement, found {statement}"),
    //         };
    //         match expr {
    //             Expression::Infix {
    //                 left,
    //                 operator,
    //                 right,
    //             } => {
    //                 assert_eq!(**left, expected_left);
    //                 assert_eq!(*operator, expected_operator);
    //                 assert_eq!(**right, expected_right);
    //             }
    //             _ => panic!("expected infix expression, found {expr}"),
    //         }
    //     }
    // }

    fn get_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => program,
            Err(err) => panic!("parse_program() returned an error: {}", err),
        }
    }

    fn assert_let_statement(statement: &Statement, expected_name: &str, _expected_value: &str) {
        let (name, value) = match statement {
            Statement::Let { name, value } => (name, value),
            _ => panic!("expected let statement, found {statement}"),
        };

        match name {
            Expression::Identifier(value) => {
                assert_eq!(*value, expected_name.to_string())
            }
            _ => panic!("expected identifier, found {name}"),
        }

        match value {
            Expression::Identifier(value) => {
                assert_eq!(*value, String::from("TODO"));
            }
            _ => panic!("expected identifier, found {value}"),
        }
    }

    fn assert_return_statement(statement: &Statement, _expected_value: &str) {
        let expr = match statement {
            Statement::Return(expression) => expression,
            _ => panic!("expected return statement, found {statement}"),
        };

        match expr {
            Expression::Identifier(value) => {
                assert_eq!(*value, String::from("TODO"));
            }
            _ => panic!("expected identifier, found {expr}"),
        }
    }

    fn assert_expression_statement(statement: &Statement, expected_value: &str) {
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };

        match expr {
            Expression::Identifier(value) => {
                assert_eq!(*value, String::from(expected_value));
            }
            Expression::IntegerLiteral(value) => {
                assert_eq!(*value, expected_value.parse::<i64>().unwrap());
            }
            _ => panic!("expected identifier, found {expr}"),
        }
    }
}
