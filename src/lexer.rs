use crate::token::Token;

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
                    Some(Token::Eq)
                }
                '=' => Some(Token::Assign),
                '!' if self.is_next_char('=') => {
                    self.read_char();
                    Some(Token::NotEq)
                }
                '!' => Some(Token::Bang),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Asterisk),
                '/' => Some(Token::Slash),
                '<' => Some(Token::Lt),
                '>' => Some(Token::Gt),
                ';' => Some(Token::Semicolon),
                ',' => Some(Token::Comma),
                ':' => Some(Token::Colon),
                '(' => Some(Token::Lparen),
                ')' => Some(Token::Rparen),
                '{' => Some(Token::Lbrace),
                '}' => Some(Token::Rbrace),
                '[' => Some(Token::Lbracket),
                ']' => Some(Token::Rbracket),
                '"' => {
                    self.read_char();
                    let literal = self.read_until(|char| char == '"');
                    self.read_char();
                    Some(Token::String(literal))
                }
                _ if char.is_ascii_digit() => {
                    let literal = self.read_until(|char| !char.is_ascii_digit());
                    Some(Token::Int(literal))
                }
                _ if char.is_ascii_alphabetic() || char == '_' => {
                    let literal =
                        self.read_until(|char| !char.is_ascii_alphanumeric() && char != '_');
                    match literal.as_str() {
                        "let" => Some(Token::Let),
                        "fn" => Some(Token::Function),
                        "true" => Some(Token::True),
                        "false" => Some(Token::False),
                        "if" => Some(Token::If),
                        "else" => Some(Token::Else),
                        "return" => Some(Token::Return),
                        _ => Some(Token::Ident(literal)),
                    }
                }
                _ => Some(Token::Illegal(char.to_string())),
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
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Lbracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
        ]
        .into_iter();

        while let Some(token) = lexer.next_token() {
            let expected_type = expected.next().unwrap();
            println!("{} - {:?}", token, expected_type);
            assert_eq!(token, expected_type);
        }
    }
}
