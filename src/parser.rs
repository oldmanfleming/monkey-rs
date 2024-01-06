#![allow(dead_code)]

use std::error::Error;

use crate::{
    ast::{Identifier, LetStatement, Statement},
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

    fn parse_program(&mut self) -> Result<Vec<Box<dyn Statement>>, Box<dyn Error>> {
        let mut program: Vec<Box<dyn Statement>> = Vec::new();
        while self.cur_token.is_some() {
            let statement = self.parse_statement()?;
            program.push(statement);
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, Box<dyn Error>> {
        let token = self.parse_token()?;
        match token {
            Token::Let => self.parse_let_statement(token),
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

    // TODO: Work on this
    // fn parse_token_expect(&mut self, expected_token: Token) -> Result<Token, Box<dyn Error>> {
    //     let token = self.parse_token()?;
    //     if !token.variant_eq(&expected_token) {
    //         Err(format!("expected {expected_token}, found {token}"))?;
    //     }
    //     Ok(token)
    // }

    //TODO: use parse_token_expect? kind of annoying to have to construct the variants that have
    //values with dummy values
    fn parse_let_statement(
        &mut self,
        let_token: Token,
    ) -> Result<Box<dyn Statement>, Box<dyn Error>> {
        let ident_token = self.parse_token()?;

        match ident_token.clone() {
            Token::Ident(_) => (),
            token => return Err(format!("expected identifier, found {token}"))?,
        };

        let ident = Identifier::new(ident_token);

        match self.parse_token()? {
            Token::Assign => (),
            token => return Err(format!("expected assign, found {token}"))?,
        }

        // TODO: skip expresion for now
        while !self.cur_token_is(Token::Semicolon) {
            self.parse_token()?;
        }

        // TODO: build a fake expression value for now
        let value = Box::new(Identifier::new(Token::Ident("TODO".to_string())));

        let let_statement = LetStatement::new(let_token, ident, value);
        Ok(Box::new(let_statement))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{LetStatement, Node, Statement},
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
                    assert_let_statement(statement.as_ref(), case);
                }
                assert_eq!(statements.len(), 0);
            }
            Err(err) => {
                panic!("parse_program() returned an error: {}", err);
            }
        }
    }

    fn get<T: std::any::Any>(statement: &dyn Node) -> &T {
        statement.as_any().downcast_ref::<T>().unwrap()
    }

    fn assert_let_statement(statement: &dyn Statement, expected_name: &str) {
        let let_statement = get::<LetStatement>(statement);
        assert_eq!(let_statement.token(), &Token::Let);
        assert_eq!(
            let_statement.name().token(),
            &Token::Ident(expected_name.to_string())
        );
        let value = get::<Identifier>(let_statement.value());
        assert_eq!(value.token(), &Token::Ident("TODO".to_string()));
    }
}
