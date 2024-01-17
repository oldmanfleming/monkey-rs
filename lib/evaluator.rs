use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, bail, Result};

use crate::{
    ast::{Expression, Program, Statement},
    environment::Environment,
    object::Object,
    token::Token,
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn eval(&mut self, program: Program) -> Result<Object> {
        let mut result = Object::Null;
        for statement in program.statements {
            result = match self.eval_statement(Box::new(statement))? {
                Object::ReturnValue(value) => return Ok(*value),
                object => object,
            };
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: Box<Statement>) -> Result<Object> {
        let object = match *statement {
            Statement::Expression(expression) => self.eval_expression(Box::new(expression))?,
            Statement::Block(statements) => self.eval_block_statement(statements)?,
            Statement::Return(expression) => {
                let value = self.eval_expression(Box::new(expression))?;
                return Ok(Object::ReturnValue(Box::new(value)));
            }
            Statement::Let { name, value } => {
                self.eval_let_statement(Box::new(value), Box::new(name))?
            }
        };
        Ok(object)
    }

    fn eval_let_statement(
        &mut self,
        value: Box<Expression>,
        name: Box<Expression>,
    ) -> Result<Object> {
        let value = self.eval_expression(value)?;
        let str_name = match *name {
            Expression::Identifier(name) => name,
            _ => bail!("{} not implemented", name),
        };
        self.env.borrow_mut().set(&str_name, value);
        Ok(Object::Null)
    }

    fn eval_block_statement(&mut self, statements: Vec<Statement>) -> Result<Object> {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(Box::new(statement))?;
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_expression(&mut self, expression: Box<Expression>) -> Result<Object> {
        let object = match *expression {
            Expression::IntegerLiteral(value) => Object::Integer(value),
            Expression::StringLiteral(value) => Object::String(value),
            Expression::BooleanLiteral(value) => self.native_bool_to_boolean_object(value),
            Expression::Prefix { operator, right } => {
                let right = self.eval_expression(right)?;
                self.eval_prefix_expression(operator, right)?
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let left = self.eval_expression(left)?;
                let right = self.eval_expression(right)?;
                self.eval_infix_expression(left, operator, right)?
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(condition, consequence, alternative)?,
            Expression::Identifier(name) => self.eval_identifier_expression(name)?,
            Expression::FunctionLiteral { parameters, body } => {
                self.eval_function_literal(parameters, body)?
            }
            Expression::Call {
                function,
                arguments,
            } => self.eval_call_function(function, arguments)?,
            Expression::ArrayLiteral(elements) => self.eval_array_literal(elements)?,
            Expression::HashLiteral(pairs) => self.eval_hash_literal(pairs)?,
            Expression::Index { left, index } => self.eval_index(left, index)?,
        };
        Ok(object)
    }

    fn eval_hash_literal(&mut self, pairs: Vec<(Expression, Expression)>) -> Result<Object> {
        let mut hash = HashMap::new();
        for (key, value) in pairs {
            let key = self.eval_expression(Box::new(key))?;
            if !key.hashable() {
                bail!("unusable as hash key: {}", key)
            }
            let value = self.eval_expression(Box::new(value))?;
            hash.insert(key, value);
        }
        Ok(Object::Hash(hash))
    }

    fn eval_array_literal(&mut self, elements: Vec<Expression>) -> Result<Object> {
        let elements = elements
            .into_iter()
            .map(|e| self.eval_expression(Box::new(e)))
            .collect::<Result<Vec<Object>>>()?;
        Ok(Object::Array(elements))
    }

    fn eval_index(&mut self, left: Box<Expression>, index: Box<Expression>) -> Result<Object> {
        let left = self.eval_expression(left)?;
        let index = self.eval_expression(index)?;
        match (left.clone(), index) {
            (Object::Array(elements), Object::Integer(index)) => match elements.get(index as usize)
            {
                Some(element) => Ok(element.clone()),
                None => Ok(NULL),
            },
            (Object::Hash(hash), index) => {
                if !index.hashable() {
                    bail!("unusable as hash key: {}", index)
                }
                match hash.get(&index) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(NULL),
                }
            }
            _ => Err(anyhow!("index operator not supported: {}", left)),
        }
    }

    fn eval_function_literal(
        &mut self,
        parameters: Vec<Expression>,
        body: Box<Statement>,
    ) -> Result<Object> {
        Ok(Object::Function {
            parameters: parameters
                .into_iter()
                .map(|p| match p {
                    Expression::Identifier(name) => Ok(name),
                    _ => Err(anyhow!("{} not implemented", p)),
                })
                .collect::<Result<Vec<String>>>()?,
            body,
            env: self.env.clone(),
        })
    }

    fn eval_call_function(
        &mut self,
        function: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Result<Object> {
        let fn_literal = self.eval_expression(function)?;
        let args = arguments
            .into_iter()
            .map(|arg| self.eval_expression(Box::new(arg)))
            .collect::<Result<Vec<Object>>>()?;
        Ok(match fn_literal {
            Object::Function {
                parameters,
                body,
                env,
            } => {
                if parameters.len() != args.len() {
                    bail!(
                        "wrong number of arguments: want={}, got={}",
                        parameters.len(),
                        args.len()
                    )
                }
                let old_env = self.env.clone();
                let mut new_env = Environment::new_enclosed_environment(env.clone());
                parameters.into_iter().zip(args).for_each(|(param, arg)| {
                    new_env.set(&param, arg);
                });
                self.env = Rc::new(RefCell::new(new_env));
                let result = self.eval_statement(body)?;
                self.env = old_env;
                match result {
                    Object::ReturnValue(value) => return Ok(*value),
                    object => object,
                }
            }
            Object::BuiltInFunction(function) => function(args)?,
            _ => bail!("{} is not a function", fn_literal),
        })
    }

    fn eval_prefix_expression(&mut self, operator: Token, right: Object) -> Result<Object> {
        match operator {
            Token::Bang => self.eval_bang_operator_expression(right),
            Token::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => bail!("unknown operator: {}{}", operator, right),
        }
    }

    fn eval_bang_operator_expression(&mut self, right: Object) -> Result<Object> {
        match right {
            Object::Boolean(true) => Ok(FALSE),
            Object::Boolean(false) => Ok(TRUE),
            Object::Null => Ok(TRUE),
            _ => Ok(FALSE),
        }
    }

    fn eval_minus_prefix_operator_expression(&mut self, right: Object) -> Result<Object> {
        match right {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(anyhow!("unknown operator: -{}", right)),
        }
    }

    fn eval_infix_expression(
        &mut self,
        left: Object,
        operator: Token,
        right: Object,
    ) -> Result<Object> {
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.eval_integer_infix_expression(left, operator, right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.eval_boolean_infix_expression(left, operator, right)
            }
            (Object::String(left), Object::String(right)) => {
                self.eval_string_infix_expression(left, operator, right)
            }
            (left, right) => Err(anyhow!("type mismatch: {} {} {}", left, operator, right)),
        }
    }

    fn eval_integer_infix_expression(
        &mut self,
        left: i64,
        operator: Token,
        right: i64,
    ) -> Result<Object> {
        match operator {
            Token::Plus => Ok(Object::Integer(left + right)),
            Token::Minus => Ok(Object::Integer(left - right)),
            Token::Asterisk => Ok(Object::Integer(left * right)),
            Token::Slash => Ok(Object::Integer(left / right)),
            Token::Lt => Ok(self.native_bool_to_boolean_object(left < right)),
            Token::Gt => Ok(self.native_bool_to_boolean_object(left > right)),
            Token::Eq => Ok(self.native_bool_to_boolean_object(left == right)),
            Token::NotEq => Ok(self.native_bool_to_boolean_object(left != right)),
            _ => Err(anyhow!("unknown operator: {} {} {}", left, operator, right)),
        }
    }

    fn eval_boolean_infix_expression(
        &mut self,
        left: bool,
        operator: Token,
        right: bool,
    ) -> Result<Object> {
        match operator {
            Token::Eq => Ok(self.native_bool_to_boolean_object(left == right)),
            Token::NotEq => Ok(self.native_bool_to_boolean_object(left != right)),
            _ => Err(anyhow!("unknown operator: {} {} {}", left, operator, right)),
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        left: String,
        operator: Token,
        right: String,
    ) -> Result<Object> {
        match operator {
            Token::Plus => Ok(Object::String(format!("{}{}", left, right))),
            _ => Err(anyhow!("unknown operator: {} {} {}", left, operator, right)),
        }
    }
    fn native_bool_to_boolean_object(&mut self, input: bool) -> Object {
        if input {
            TRUE
        } else {
            FALSE
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    ) -> Result<Object> {
        let condition = self.eval_expression(condition)?;
        if self.is_truthy(condition) {
            self.eval_statement(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_statement(alternative)
        } else {
            Ok(NULL)
        }
    }

    fn is_truthy(&mut self, object: Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(true) => true,
            Object::Boolean(false) => false,
            _ => true,
        }
    }

    fn eval_identifier_expression(&mut self, name: String) -> Result<Object> {
        match self.env.borrow().get(&name) {
            Some(value) => Ok(value.clone()),
            None => Err(anyhow!("identifier not found: {}", name)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_fibonacci() {
        let input = r#"
        let fibonacci = fn(x) {
            if (x == 0) {
                0
            } else {
                if (x == 1) {
                    1
                } else {
                    fibonacci(x - 1) + fibonacci(x - 2);
                }
            }
        };
        fibonacci(10);
        "#;
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated, Object::Integer(55));
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Integer(expected));
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
                "#,
                Object::Integer(10),
            ),
            (r#"9; return if(true) { 10 };"#, Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: 5 + true"),
            ("5 + true; 5;", "type mismatch: 5 + true"),
            ("-true", "unknown operator: -true"),
            ("true + false;", "unknown operator: true + false"),
            ("5; true + false; 5", "unknown operator: true + false"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: true + false",
            ),
            ("foobar", "identifier not found: foobar"),
            (r#""Hello" - "World""#, "unknown operator: Hello - World"),
            (r#"len(1)"#, "argument to `len` not supported, got 1"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments for len: want=1, got=2",
            ),
            (
                r#"{fn(x) { x }: "Monkey"};"#,
                "unusable as hash key: fn(x) { x\n }",
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: fn(x) { x\n }",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Ok(_) => panic!("no error returned for {}", input),
                Err(error) => assert_eq!(error.to_string(), expected),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_function_objects() {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Function {
                parameters: vec!["x".to_string()],
                body: Box::new(Statement::Block(vec![Statement::Expression(
                    Expression::Infix {
                        left: Box::new(Expression::Identifier("x".to_string())),
                        operator: Token::Plus,
                        right: Box::new(Expression::IntegerLiteral(2)),
                    },
                )])),
                env: Rc::new(RefCell::new(Environment::new())),
            },
        )];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
            (
                "let i = 5; let inc = fn(i) { i + 1; }; inc(i); i;",
                Object::Integer(5),
            ),
            (
                "let double_inc = fn (x) { let inc = fn(x) { x + 1; }; inc(x) + inc(x); }; double_inc(5);",
                Object::Integer(12),
            ),
            (
                "let double_add = fn (x) { fn (y) { x + y; }; }; double_add(5)(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn (x) { let y = 5; fn () { x + y }; }; add(5)();",
                Object::Integer(10),
            ),
            (
                "let add = fn (fun) { fun() + fun(); } let fun = fn () { 5; }; add(fun);",
                Object::Integer(10),
            ),
            (
                "let concat = fn (x) { fn (y) { x + y; }; }; concat(\"Hello \")(\"World!\");",
                Object::String("Hello World!".to_string()),
            )
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let tests = vec![(
            r#""Hello" + " " + "World!""#,
            Object::String("Hello World!".to_string()),
        )];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            (
                r#"[1, "2", true, fn (x) { x + x; }];"#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::String("2".to_string()),
                    Object::Boolean(true),
                    Object::Function {
                        parameters: vec!["x".to_string()],
                        body: Box::new(Statement::Block(vec![Statement::Expression(
                            Expression::Infix {
                                left: Box::new(Expression::Identifier("x".to_string())),
                                operator: Token::Plus,
                                right: Box::new(Expression::Identifier("x".to_string())),
                            },
                        )])),
                        env: Rc::new(RefCell::new(Environment::new())),
                    },
                ]),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ]),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![(
            r#"{"one": 5 + 5, "two": 10 * 2, "thr" + "ee": 6 / 2, 4: 4, true: 5, false: 6 }"#,
            Object::Hash(
                vec![
                    (Object::String("one".to_string()), Object::Integer(10)),
                    (Object::String("two".to_string()), Object::Integer(20)),
                    (Object::String("three".to_string()), Object::Integer(3)),
                    (Object::Integer(4), Object::Integer(4)),
                    (Object::Boolean(true), Object::Integer(5)),
                    (Object::Boolean(false), Object::Integer(6)),
                ]
                .into_iter()
                .collect(),
            ),
        )];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_hash_indexing() {
        let cases = vec![
            (r#"{"foo": 5}["foo"]"#, Object::Integer(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Object::Integer(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, Object::Integer(5)),
            (r#"{true: 5}[true]"#, Object::Integer(5)),
            (r#"{false: 5}[false]"#, Object::Integer(5)),
        ];
        for (input, expected) in cases {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (r#"len([1, 2, 3])"#, Object::Integer(3)),
            (r#"len([])"#, Object::Integer(0)),
            (
                r#"len({ "one": 1, "two": 2, "three": 3 })"#,
                Object::Integer(3),
            ),
            (r#"first([1, 2, 3])"#, Object::Integer(1)),
            (r#"first([])"#, Object::Null),
            (r#"last([1, 2, 3])"#, Object::Integer(3)),
            (r#"last([])"#, Object::Null),
            (
                r#"rest([1, 2, 3])"#,
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            (r#"rest([])"#, Object::Null),
            (r#"push([], 1)"#, Object::Array(vec![Object::Integer(1)])),
            (
                r#"push([1, 2, 3], 4)"#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ]),
            ),
        ];
        for (case, expected) in tests {
            let evaluated = test_eval(case).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Result<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let mut evaluator = Evaluator::new();
        evaluator.eval(program)
    }
}
