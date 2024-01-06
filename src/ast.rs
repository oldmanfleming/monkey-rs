#![allow(dead_code)]

use std::any::Any;

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Box<dyn Expression>) -> Self {
        Self { token, name, value }
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn name(&self) -> &Identifier {
        &self.name
    }

    pub fn value(&self) -> &dyn Expression {
        self.value.as_ref()
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct Identifier {
    token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Self { token }
    }

    pub fn token(&self) -> &Token {
        &self.token
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
