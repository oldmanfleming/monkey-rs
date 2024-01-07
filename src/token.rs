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
    pub fn variant_eq(&self, other: Token) -> Result<Token, String> {
        if std::mem::discriminant(self) == std::mem::discriminant(&other) {
            return Ok(self.clone());
        }
        Err(format!("expected {:?}, got {:?}", other, self))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
