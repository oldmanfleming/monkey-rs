#![allow(dead_code)]

use std::fmt;

use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
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
            Statement::Let { name, value } => write!(f, "let {name} = {value}"),
            Statement::Return(expression) => write!(f, "return {expression}"),
            Statement::Expression(expression) => write!(f, "{expression}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    Prefix {
        operator: Token,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{ident}"),
            Expression::IntegerLiteral(value) => write!(f, "{value}"),
            Expression::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({left} {operator} {right})"),
        }
    }
}
