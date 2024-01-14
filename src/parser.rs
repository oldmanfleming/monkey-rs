use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from_token(token: Token) -> Self {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.cur_token().is_some() {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }
        Ok(Program { statements })
    }

    fn cur_token(&mut self) -> Option<Token> {
        self.cur_token.clone()
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.peek_token.clone()
    }

    fn next_token(&mut self) -> &mut Self {
        self.cur_token = self.peek_token();
        self.peek_token = self.lexer.next_token();
        self
    }

    fn peek_precedence(&mut self) -> Result<Precedence, String> {
        Ok(Precedence::from_token(
            self.peek_token().ok_or("no token found")?,
        ))
    }

    fn cur_precedence(&mut self) -> Result<Precedence, String> {
        Ok(Precedence::from_token(
            self.cur_token().ok_or("no token found")?,
        ))
    }

    fn expect_peek(&mut self, exp_token: Token) -> Result<(), String> {
        let peek_token = self.peek_token().ok_or(format!("no token found"))?;
        if peek_token.variant_eq(exp_token.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "expected next token to be {exp_token}, found {peek_token}",
            ))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token().ok_or("no token found")? {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name = match self
            .next_token()
            .cur_token()
            .ok_or(format!("no token found"))?
        {
            Token::Ident(value) => Expression::Identifier(value),
            token => Err(format!("expected identifier, found {token}"))?,
        };

        self.expect_peek(Token::Assign)?;

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Semicolon))
        {
            self.next_token();
        }

        Ok(Statement::Let { name, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Semicolon))
        {
            self.next_token();
        }

        Ok(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Expression(expression);

        if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Semicolon))
        {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let cur_token = self.cur_token().ok_or(format!("no token found"))?;

        let mut left_exp = match cur_token.clone() {
            Token::Ident(value) => self.parse_identifier(value),
            Token::Int(value) => self.parse_integer_literal(value)?,
            Token::String(value) => Expression::StringLiteral(value),
            Token::True | Token::False => self.parse_boolean_literal(cur_token)?,
            Token::Bang | Token::Minus => self.parse_prefix_expression(cur_token)?,
            Token::Lparen => self.parse_grouped_expression()?,
            Token::If => self.parse_if_expression()?,
            Token::Function => self.parse_function_literal()?,
            token => Err(format!("no prefix parse function for {token}"))?,
        };

        while self
            .peek_token()
            .is_some_and(|token| !token.variant_eq(Token::Semicolon))
            && precedence < self.peek_precedence()?
        {
            match self.peek_token().ok_or(format!("no token found"))? {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    left_exp = self.parse_infix_expression(left_exp)?;
                }
                Token::Lparen => {
                    self.next_token();
                    left_exp = self.parse_call_expression(left_exp)?;
                }
                _ => return Ok(left_exp),
            };
        }

        Ok(left_exp)
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression, String> {
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix {
            operator: token,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let token = self.cur_token().ok_or(format!("no token found"))?;
        let precedence = self.cur_precedence()?;
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::Infix {
            left: Box::new(left),
            operator: token,
            right: Box::new(right),
        })
    }

    fn parse_identifier(&mut self, value: String) -> Expression {
        Expression::Identifier(value)
    }

    fn parse_integer_literal(&mut self, literal: String) -> Result<Expression, String> {
        let value = literal
            .parse::<i64>()
            .map_err(|err| format!("could not parse integer literal as i64: {err}"))?;
        Ok(Expression::IntegerLiteral(value))
    }

    fn parse_boolean_literal(&mut self, token: Token) -> Result<Expression, String> {
        let value = match token {
            Token::True => true,
            Token::False => false,
            _ => Err(format!("no boolean parse function for {token}"))?,
        };
        Ok(Expression::BooleanLiteral(value))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rparen)?;
        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Rparen)?;

        self.expect_peek(Token::Lbrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Else))
        {
            self.next_token();
            self.expect_peek(Token::Lbrace)?;
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Ok(Expression::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::Lbrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral {
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>, String> {
        let mut identifiers: Vec<Expression> = Vec::new();

        if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Rparen))
        {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        match self.cur_token().ok_or(format!("no token found"))? {
            Token::Ident(value) => identifiers.push(Expression::Identifier(value)),
            token => Err(format!("expected identifier, found {token}"))?,
        };

        while self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Comma))
        {
            self.next_token();
            self.next_token();
            match self.cur_token().ok_or(format!("no token found"))? {
                Token::Ident(value) => identifiers.push(Expression::Identifier(value)),
                token => Err(format!("expected identifier, found {token}"))?,
            };
        }

        self.expect_peek(Token::Rparen)?;

        Ok(identifiers)
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let mut statements: Vec<Statement> = Vec::new();

        while self
            .cur_token()
            .is_some_and(|token| !token.variant_eq(Token::Rbrace))
        {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }

        Ok(Statement::Block(statements))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, String> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut arguments: Vec<Expression> = Vec::new();

        if self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Rparen))
        {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();

        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self
            .peek_token()
            .is_some_and(|token| token.variant_eq(Token::Comma))
        {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(Token::Rparen)?;

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::Deref, vec};

    use crate::{
        ast::{Expression, Statement},
        token::Token,
    };

    use super::*;

    #[test]
    fn let_statements() {
        let program = get_program(
            r#"
                let x = 5;
            "#,
        );
        assert_eq!(program.statements.len(), 1);
        let cases = vec![("x", Expression::IntegerLiteral(5))];
        let mut statements = program.statements.iter();
        for (name, value) in cases {
            let statement = statements.next().unwrap();
            assert_let_statement(statement, name, value);
        }
    }

    #[test]
    fn return_statements() {
        let program = get_program(
            r#"
                return 5;
            "#,
        );
        assert_eq!(program.statements.len(), 1);
        let cases = vec![5];
        let mut statements = program.statements.iter();
        for value in cases {
            let statement = statements.next().unwrap();
            let expr = match statement {
                Statement::Return(expression) => expression,
                _ => panic!("expected return statement, found {statement}"),
            };
            assert_integer_literal(expr, value);
        }
    }

    #[test]
    fn identifier_expression() {
        let program = get_program("foobar;");
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };
        assert_identifier_expression(expr, "foobar");
    }

    #[test]
    fn integer_literal_expression() {
        let program = get_program("5;");
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };
        assert_integer_literal(expr, 5);
    }

    #[test]
    fn bool_expression() {
        let program = get_program(
            r#"
            true;
            false;
        "#,
        );
        assert_eq!(program.statements.len(), 2);
        let cases = vec![true, false];
        let mut statements = program.statements.iter();
        for value in cases {
            let statement = statements.next().unwrap();
            let expr = match statement {
                Statement::Expression(expression) => expression,
                _ => panic!("expected expression statement, found {statement}"),
            };
            assert_boolean_literal(expr, value);
        }
    }

    #[test]
    fn string_literals() {
        let program = get_program(
            r#"
            "hello world";
        "#,
        );
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };
        match expr {
            Expression::StringLiteral(value) => assert_eq!(value, "hello world"),
            _ => panic!("expected string literal, found {expr}"),
        }
    }

    #[test]
    fn prefix_operators() {
        let cases = vec![
            ("!5;", Token::Bang, Expression::IntegerLiteral(5)),
            ("-15;", Token::Minus, Expression::IntegerLiteral(15)),
            ("!true;", Token::Bang, Expression::BooleanLiteral(true)),
            ("!false;", Token::Bang, Expression::BooleanLiteral(false)),
        ];
        for (input, expected_operator, expected_int) in cases {
            let program = get_program(input);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.first().unwrap();
            let expr = match statement {
                Statement::Expression(expression) => expression,
                _ => panic!("expected expression statement, found {statement}"),
            };
            match expr {
                Expression::Prefix { operator, right } => {
                    assert_eq!(*operator, expected_operator);
                    assert_eq!(**right, expected_int);
                }
                _ => panic!("expected prefix expression, found {expr}"),
            }
        }
    }

    #[test]
    fn infix_expressions() {
        let cases = vec![
            (
                "5 + 5;",
                Expression::IntegerLiteral(5),
                Token::Plus,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 - 5;",
                Expression::IntegerLiteral(5),
                Token::Minus,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 * 5;",
                Expression::IntegerLiteral(5),
                Token::Asterisk,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 / 5;",
                Expression::IntegerLiteral(5),
                Token::Slash,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 > 5;",
                Expression::IntegerLiteral(5),
                Token::Gt,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 < 5;",
                Expression::IntegerLiteral(5),
                Token::Lt,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 == 5;",
                Expression::IntegerLiteral(5),
                Token::Eq,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 != 5;",
                Expression::IntegerLiteral(5),
                Token::NotEq,
                Expression::IntegerLiteral(5),
            ),
            (
                "true == true",
                Expression::BooleanLiteral(true),
                Token::Eq,
                Expression::BooleanLiteral(true),
            ),
            (
                "true != false",
                Expression::BooleanLiteral(true),
                Token::NotEq,
                Expression::BooleanLiteral(false),
            ),
            (
                "false == false",
                Expression::BooleanLiteral(false),
                Token::Eq,
                Expression::BooleanLiteral(false),
            ),
        ];
        for (input, expected_left, expected_operator, expected_right) in cases {
            let program = get_program(input);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.first().unwrap();
            let expr = match statement {
                Statement::Expression(expression) => expression,
                _ => panic!("expected expression statement, found {statement}"),
            };
            assert_infix_expression(expr, expected_left, expected_operator, expected_right);
        }
    }

    #[test]
    fn operator_precedence() {
        let cases = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        for (input, expected) in cases {
            let program = get_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn if_expression() {
        let cases = vec![
            (
                "if (x < y) { x }",
                Expression::Infix {
                    left: Box::new(Expression::Identifier("x".to_string())),
                    operator: Token::Lt,
                    right: Box::new(Expression::Identifier("y".to_string())),
                },
                Statement::Block(vec![Statement::Expression(Expression::Identifier(
                    "x".to_string(),
                ))]),
                None,
            ),
            (
                "if (x < y) { x } else { y }",
                Expression::Infix {
                    left: Box::new(Expression::Identifier("x".to_string())),
                    operator: Token::Lt,
                    right: Box::new(Expression::Identifier("y".to_string())),
                },
                Statement::Block(vec![Statement::Expression(Expression::Identifier(
                    "x".to_string(),
                ))]),
                Some(Statement::Block(vec![Statement::Expression(
                    Expression::Identifier("y".to_string()),
                )])),
            ),
        ];
        for (case, exp_cond, exp_cons, exp_alt) in cases {
            let program = get_program(case);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.first().unwrap();

            assert_if_else(statement, exp_cond, exp_cons, exp_alt);
        }
    }

    #[test]
    fn function_literals() {
        let cases = vec![
            ("fn() {};", vec![], Statement::Block(vec![])),
            (
                "fn (x) {};",
                vec![Expression::Identifier("x".to_string())],
                Statement::Block(vec![]),
            ),
            (
                "fn(x, y) { x + y; }",
                vec![
                    Expression::Identifier("x".to_string()),
                    Expression::Identifier("y".to_string()),
                ],
                Statement::Block(vec![Statement::Expression(Expression::Infix {
                    left: Box::new(Expression::Identifier("x".to_string())),
                    operator: Token::Plus,
                    right: Box::new(Expression::Identifier("y".to_string())),
                })]),
            ),
        ];
        for (case, exp_params, exp_body) in cases {
            let program = get_program(case);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.first().unwrap();
            let expr = match statement {
                Statement::Expression(expression) => expression,
                _ => panic!("expected expression statement, found {statement}"),
            };

            assert_function_literal(expr, exp_params, exp_body);
        }
    }

    #[test]
    fn call_expressions() {
        let program = get_program("add(1, 2 * 3, 4 + 5);");

        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.first().unwrap();
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };
        match expr {
            Expression::Call {
                function,
                arguments,
            } => {
                assert_identifier_expression(function.deref(), "add");
                assert_eq!(arguments.len(), 3);
                assert_integer_literal(&arguments[0], 1);
                assert_infix_expression(
                    &arguments[1],
                    Expression::IntegerLiteral(2),
                    Token::Asterisk,
                    Expression::IntegerLiteral(3),
                );
                assert_infix_expression(
                    &arguments[2],
                    Expression::IntegerLiteral(4),
                    Token::Plus,
                    Expression::IntegerLiteral(5),
                );
            }
            _ => panic!("expected call expression, found {expr}"),
        }
    }

    fn get_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => program,
            Err(err) => panic!("parse_program() returned an error: {}", err),
        }
    }

    fn assert_function_literal(
        expression: &Expression,
        expected_parameters: Vec<Expression>,
        expected_body: Statement,
    ) {
        match expression {
            Expression::FunctionLiteral { parameters, body } => {
                assert_eq!(parameters, &expected_parameters);
                assert_eq!(body.deref(), &expected_body);
            }
            _ => panic!("expected function literal, found {expression}"),
        }
    }

    fn assert_if_else(
        statement: &Statement,
        exp_condition: Expression,
        exp_consequence: Statement,
        exp_alternative: Option<Statement>,
    ) {
        let expr = match statement {
            Statement::Expression(expression) => expression,
            _ => panic!("expected expression statement, found {statement}"),
        };
        match expr {
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                assert_eq!(**condition, exp_condition);
                assert_eq!(**consequence, exp_consequence);
                match exp_alternative {
                    Some(exp_alternative) => {
                        assert_eq!(*alternative.as_deref().unwrap(), exp_alternative)
                    }
                    None => assert_eq!(*alternative, None),
                }
            }
            _ => panic!("expected if expression, found {expr}"),
        }
    }

    fn assert_let_statement(
        statement: &Statement,
        expected_name: &str,
        expected_value: Expression,
    ) {
        let (name, value) = match statement {
            Statement::Let { name, value } => (name, value),
            _ => panic!("expected let statement, found {statement}"),
        };

        match name {
            Expression::Identifier(value) => {
                assert_eq!(*value, expected_name.to_string())
            }
            _ => panic!("expected identifier, found {name}"),
        }

        assert_eq!(*value, expected_value);
    }

    fn assert_infix_expression(
        expr: &Expression,
        expected_left: Expression,
        expected_operator: Token,
        expected_right: Expression,
    ) {
        match expr {
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                assert_eq!(**left, expected_left);
                assert_eq!(*operator, expected_operator);
                assert_eq!(**right, expected_right);
            }
            _ => panic!("expected infix expression, found {expr}"),
        }
    }

    fn assert_identifier_expression(expression: &Expression, expected_value: &str) {
        match expression {
            Expression::Identifier(value) => {
                assert_eq!(*value, String::from(expected_value));
            }
            _ => panic!("expected identifier, found {expression}"),
        }
    }

    fn assert_integer_literal(expression: &Expression, expected_value: i64) {
        match expression {
            Expression::IntegerLiteral(value) => {
                assert_eq!(*value, expected_value);
            }
            _ => panic!("expected identifier, found {expression}"),
        }
    }

    fn assert_boolean_literal(expression: &Expression, expected_value: bool) {
        match expression {
            Expression::BooleanLiteral(value) => {
                assert_eq!(*value, expected_value);
            }
            _ => panic!("expected identifier, found {expression}"),
        }
    }
}
