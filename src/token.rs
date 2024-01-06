use std::{error::Error, fmt};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    Int(String),
    String(String),
    Illegal(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,
    Eq,
    NotEq,

    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn variant_eq(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn inner(&self) -> Result<&String, Box<dyn Error>> {
        match self {
            Token::Ident(value) => Ok(value),
            Token::Int(value) => Ok(value),
            Token::String(value) => Ok(value),
            Token::Illegal(value) => Ok(value),
            _ => Err(format!("no inner value found for {self}"))?,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
