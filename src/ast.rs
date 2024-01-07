#![allow(dead_code)]

use std::fmt;

use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let { name: Expression, value: Expression },
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {name} = {value};"),
            Statement::Return(expression) => write!(f, "return {expression};"),
            Statement::Expression(expression) => write!(f, "{expression};"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Token),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(token) => match token {
                Token::Ident(ident) => write!(f, "{}", ident),
                _ => write!(f, "unimplemented token: {token}"),
            },
        }
    }
}
