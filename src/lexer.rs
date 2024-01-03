use crate::token::{Token, TokenType};

pub struct Lexer {
    chars: Vec<char>,
    position: usize,
    char: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let char = chars.first().copied();
        Self {
            chars,
            position: 0,
            char,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while self.char.is_some_and(|char| char.is_whitespace()) {
            self.read_char();
        }

        let token = match self.char {
            Some(char) => match char {
                '=' if self.is_next_char('=') => {
                    self.read_char();
                    Some(Token::new(TokenType::Eq, "=="))
                }
                '=' => Some(Token::new(TokenType::Assign, char)),
                '!' if self.is_next_char('=') => {
                    self.read_char();
                    Some(Token::new(TokenType::NotEq, "!="))
                }
                '!' => Some(Token::new(TokenType::Bang, char)),
                '+' => Some(Token::new(TokenType::Plus, char)),
                '-' => Some(Token::new(TokenType::Minus, char)),
                '*' => Some(Token::new(TokenType::Asterisk, char)),
                '/' => Some(Token::new(TokenType::Slash, char)),
                '<' => Some(Token::new(TokenType::Lt, char)),
                '>' => Some(Token::new(TokenType::Gt, char)),
                ';' => Some(Token::new(TokenType::Semicolon, char)),
                ',' => Some(Token::new(TokenType::Comma, char)),
                ':' => Some(Token::new(TokenType::Colon, char)),
                '(' => Some(Token::new(TokenType::Lparen, char)),
                ')' => Some(Token::new(TokenType::Rparen, char)),
                '{' => Some(Token::new(TokenType::Lbrace, char)),
                '}' => Some(Token::new(TokenType::Rbrace, char)),
                '[' => Some(Token::new(TokenType::Lbracket, char)),
                ']' => Some(Token::new(TokenType::Rbracket, char)),
                '"' => {
                    self.read_char();
                    let literal = self.read_until(|char| char == '"');
                    self.read_char();
                    Some(Token::new(TokenType::String, literal))
                }
                _ if char.is_ascii_digit() => {
                    let literal = self.read_until(|char| !char.is_ascii_digit());
                    Some(Token::new(TokenType::Int, literal))
                }
                _ if char.is_ascii_alphabetic() || char == '_' => {
                    let literal =
                        self.read_until(|char| !char.is_ascii_alphanumeric() && char != '_');
                    match literal.as_str() {
                        "let" => Some(Token::new(TokenType::Let, literal)),
                        "fn" => Some(Token::new(TokenType::Function, literal)),
                        "true" => Some(Token::new(TokenType::True, literal)),
                        "false" => Some(Token::new(TokenType::False, literal)),
                        "if" => Some(Token::new(TokenType::If, literal)),
                        "else" => Some(Token::new(TokenType::Else, literal)),
                        "return" => Some(Token::new(TokenType::Return, literal)),
                        _ => Some(Token::new(TokenType::Ident, literal)),
                    }
                }
                _ => Some(Token::new(TokenType::Illegal, char)),
            },
            None => None,
        };

        self.read_char();

        token
    }

    fn read_char(&mut self) {
        self.position += 1;
        self.char = self.chars.get(self.position).copied();
    }

    fn is_next_char(&self, ch: char) -> bool {
        self.chars.get(self.position + 1).eq(&Some(&ch))
    }

    fn read_until(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut literal = String::new();
        while let Some(char) = self.char {
            if condition(char) {
                self.position -= 1;
                break;
            }
            literal.push(char);
            self.read_char();
        }
        literal
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = r#"
                    let five = 5;
                    let ten = 10;
                    let add = fn(x, y) {
                        x + y;
                    };
                    let result = add(five, ten);
                    !-/*5;
                    5 < 10 > 5;
                    if (5 < 10) {
                        return true;
                    } else {
                        return false;
                    }
                    10 == 10;
                    10 != 9;
                    "foobar"
                    "foo bar"
                    [1, 2];
                    {"foo": "bar"}
                    "#;

        let mut lexer = Lexer::new(input);

        let mut expected = vec![
            TokenType::Let,
            TokenType::Ident,
            TokenType::Assign,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident,
            TokenType::Assign,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident,
            TokenType::Assign,
            TokenType::Function,
            TokenType::Lparen,
            TokenType::Ident,
            TokenType::Comma,
            TokenType::Ident,
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Ident,
            TokenType::Plus,
            TokenType::Ident,
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident,
            TokenType::Assign,
            TokenType::Ident,
            TokenType::Lparen,
            TokenType::Ident,
            TokenType::Comma,
            TokenType::Ident,
            TokenType::Rparen,
            TokenType::Semicolon,
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Slash,
            TokenType::Asterisk,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::Int,
            TokenType::Lt,
            TokenType::Int,
            TokenType::Gt,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::If,
            TokenType::Lparen,
            TokenType::Int,
            TokenType::Lt,
            TokenType::Int,
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Return,
            TokenType::True,
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Else,
            TokenType::Lbrace,
            TokenType::Return,
            TokenType::False,
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Int,
            TokenType::Eq,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::Int,
            TokenType::NotEq,
            TokenType::Int,
            TokenType::Semicolon,
            TokenType::String,
            TokenType::String,
            TokenType::Lbracket,
            TokenType::Int,
            TokenType::Comma,
            TokenType::Int,
            TokenType::Rbracket,
            TokenType::Semicolon,
            TokenType::Lbrace,
            TokenType::String,
            TokenType::Colon,
            TokenType::String,
            TokenType::Rbrace,
        ]
        .into_iter();

        while let Some(token) = lexer.next_token() {
            let expected_type = expected.next().unwrap();
            println!("{} - {:?}", token, expected_type);
            assert_eq!(*token.token_type(), expected_type);
        }
    }
}
