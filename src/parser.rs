#![allow(dead_code)]

use std::error::Error;

use crate::{
    ast::{Expression, Statement},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token(),
            lexer,
        }
    }

    fn cur_token(&self) -> &Option<Token> {
        &self.cur_token
    }

    fn parse_program(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        let mut program: Vec<Statement> = Vec::new();
        while self.cur_token.is_some() {
            let statement = self.parse_statement()?;
            program.push(statement);
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.parse_token()?;
        match token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => Err(format!("unimplemented token: {token}"))?,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    fn cur_token_is(&mut self, token: Token) -> bool {
        match self.cur_token() {
            Some(cur_token) => token.variant_eq(cur_token),
            None => false,
        }
    }

    fn parse_token(&mut self) -> Result<Token, Box<dyn Error>> {
        let token = match self.cur_token() {
            Some(token) => Ok(token.clone()),
            None => Err("no token found")?,
        };
        self.next_token();
        token
    }

    fn parse_token_expect(&mut self, expected_token: Token) -> Result<Token, Box<dyn Error>> {
        let token = self.parse_token()?;
        if !token.variant_eq(&expected_token) {
            Err(format!("expected {expected_token}, found {token}"))?;
        }
        Ok(token)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let ident_token = self.parse_token_expect(Token::Ident(String::new()))?;

        let ident = Expression::Identifier(ident_token);

        self.parse_token_expect(Token::Assign)?;

        // TODO: skip expresion for now
        while !self.cur_token_is(Token::Semicolon) {
            self.parse_token()?;
        }

        // TODO: build a fake expression value for now
        let value = Expression::Identifier(Token::Ident("TODO".to_string()));

        let let_statement = Statement::Let { name: ident, value };

        Ok(let_statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        // TODO: skip expresion for now
        while !self.cur_token_is(Token::Semicolon) {
            self.parse_token()?;
        }

        // TODO: build a fake expression value for now
        let value = Expression::Identifier(Token::Ident("TODO".to_string()));

        let return_statement = Statement::Return(value);

        Ok(return_statement)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Statement},
        token::Token,
    };

    use super::*;

    #[test]
    fn test_let_statements() {
        let lexer = Lexer::new(
            r#"
                let x = 5;
                let y = 10;
                let foobar = 838383;
            "#,
        );
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => {
                assert_eq!(program.len(), 3);

                let cases = vec!["x", "y", "foobar"];
                let mut statements = program.iter();
                for case in cases {
                    let statement = statements.next().unwrap();
                    assert_let_statement(statement, case);
                }
                assert_eq!(statements.len(), 0);
            }
            Err(err) => {
                panic!("parse_program() returned an error: {}", err);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let lexer = Lexer::new(
            r#"
                return 5;
                return 10;
                return 993322;
            "#,
        );
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => {
                assert_eq!(program.len(), 3);
                for statement in program.iter() {
                    assert_return_statement(statement);
                }
            }
            Err(err) => {
                panic!("parse_program() returned an error: {}", err);
            }
        }
    }

    fn assert_let_statement(statement: &Statement, expected_name: &str) {
        let (name, value) = match statement {
            Statement::Let { name, value } => (name, value),
            _ => panic!("expected let statement, found {statement}"),
        };

        match name {
            Expression::Identifier(token) => {
                assert_eq!(token, &Token::Ident(expected_name.to_string()))
            }
            _ => panic!("expected identifier, found {name}"),
        }

        match value {
            Expression::Identifier(token) => {
                assert_eq!(token, &Token::Ident(String::from("TODO")));
            }
        }
    }

    fn assert_return_statement(statement: &Statement) {
        let expr = match statement {
            Statement::Return(expression) => expression,
            _ => panic!("expected return statement, found {statement}"),
        };

        match expr {
            Expression::Identifier(token) => {
                assert_eq!(token, &Token::Ident(String::from("TODO")));
            }
        }
    }
}
