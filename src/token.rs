use std::fmt;

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

    pub fn inner(&self) -> Option<&String> {
        match self {
            Token::Ident(value) => Some(value),
            Token::Int(value) => Some(value),
            Token::String(value) => Some(value),
            Token::Illegal(value) => Some(value),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
