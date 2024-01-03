use std::fmt;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Ident,

    Int,
    String,
    Illegal,

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

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    literal: String,
}

impl Token {
    pub fn new<T: Into<String>>(token_type: TokenType, literal: T) -> Self {
        Self {
            token_type,
            literal: literal.into(),
        }
    }

    #[allow(dead_code)] // doesn't seem to detect the usage inside of a test?
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.literal,)
    }
}
