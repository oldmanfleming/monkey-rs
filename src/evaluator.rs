use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Program, Statement},
    environment::Environment,
    object::Object,
    token::Token,
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(program: Program, env: Rc<RefCell<Environment>>) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in program.statements {
        result = match eval_statement(statement, env.clone())? {
            Object::ReturnValue(value) => return Ok(*value),
            object => object,
        };
    }
    Ok(result)
}

fn eval_statement(statement: Statement, env: Rc<RefCell<Environment>>) -> Result<Object, String> {
    let object = match statement {
        Statement::Expression(expression) => eval_expression(expression, env)?,
        Statement::Block(statements) => eval_block_statement(statements, env)?,
        Statement::Return(expression) => {
            let value = eval_expression(expression, env)?;
            return Ok(Object::ReturnValue(Box::new(value)));
        }
        Statement::Let { name, value } => eval_let_statement(value, name, env)?,
    };
    Ok(object)
}

fn eval_let_statement(
    value: Expression,
    name: Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let value = eval_expression(value, env.clone())?;
    let str_name = match name {
        Expression::Identifier(name) => name,
        _ => Err(format!("{} not implemented", name))?,
    };
    env.borrow_mut().set(&str_name, value);
    Ok(Object::Null)
}

fn eval_block_statement(
    statements: Vec<Statement>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env.clone())?;
        if let Object::ReturnValue(_) = result {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_expression(
    expression: Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let object = match expression {
        Expression::IntegerLiteral(value) => Object::Integer(value),
        Expression::StringLiteral(value) => Object::String(value),
        Expression::BooleanLiteral(value) => native_bool_to_boolean_object(value),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(*right, env)?;
            eval_prefix_expression(operator, right)?
        }
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left, env.clone())?;
            let right = eval_expression(*right, env)?;
            eval_infix_expression(left, operator, right)?
        }
        Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, *consequence, alternative, env)?,
        Expression::Identifier(name) => eval_identifier_expression(name, env)?,
        Expression::FunctionLiteral { parameters, body } => {
            eval_function_literal(parameters, body, env)?
        }
        Expression::Call {
            function,
            arguments,
        } => eval_call_function(function, env, arguments)?,
        Expression::ArrayLiteral(elements) => eval_array_literal(elements, env)?,
        Expression::Index { left, index } => eval_index(left, index, env)?,
    };
    Ok(object)
}

fn eval_array_literal(
    elements: Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let elements = elements
        .into_iter()
        .map(|e| eval_expression(e, env.clone()))
        .collect::<Result<Vec<Object>, String>>()?;
    Ok(Object::Array(elements))
}

fn eval_index(
    left: Box<Expression>,
    index: Box<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let left = eval_expression(*left, env.clone())?;
    let index = eval_expression(*index, env)?;
    match (left.clone(), index) {
        (Object::Array(elements), Object::Integer(index)) => match elements.get(index as usize) {
            Some(element) => Ok(element.clone()),
            None => Ok(NULL),
        },
        _ => Err(format!("index operator not supported: {}", left)),
    }
}

fn eval_call_function(
    function: Box<Expression>,
    env: Rc<RefCell<Environment>>,
    arguments: Vec<Expression>,
) -> Result<Object, String> {
    let fn_literal = eval_expression(*function, env.clone())?;
    let args_env = env.clone();
    let args = arguments
        .into_iter()
        .map(move |arg| eval_expression(arg, args_env.clone()))
        .collect::<Result<Vec<Object>, String>>()?;
    Ok(match fn_literal {
        Object::Function {
            parameters,
            body,
            env: fn_env,
        } => {
            if parameters.len() != args.len() {
                Err(format!(
                    "wrong number of arguments: want={}, got={}",
                    parameters.len(),
                    args.len()
                ))?
            }

            parameters.into_iter().zip(args).for_each(|(param, arg)| {
                fn_env.borrow_mut().set(&param, arg);
            });
            eval_statement(body, fn_env)?
        }
        Object::BuiltInFunction(function) => function(args)?,
        _ => Err(format!("{} is not a function", fn_literal))?,
    })
}

fn eval_function_literal(
    parameters: Vec<Expression>,
    body: Box<Statement>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    Ok(Object::Function {
        parameters: parameters
            .into_iter()
            .map(|p| match p {
                Expression::Identifier(name) => Ok(name),
                _ => Err(format!("{} not implemented", p)),
            })
            .collect::<Result<Vec<String>, String>>()?,
        body: *body,
        env: Environment::new_enclosed_environment(env),
    })
}

fn eval_prefix_expression(operator: Token, right: Object) -> Result<Object, String> {
    match operator {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Err(format!("unknown operator: {}{}", operator, right))?,
    }
}

fn eval_bang_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Boolean(true) => Ok(FALSE),
        Object::Boolean(false) => Ok(TRUE),
        Object::Null => Ok(TRUE),
        _ => Ok(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Integer(value) => Ok(Object::Integer(-value)),
        _ => Err(format!("unknown operator: -{}", right)),
    }
}

fn eval_infix_expression(left: Object, operator: Token, right: Object) -> Result<Object, String> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(left, operator, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(left, operator, right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(left, operator, right)
        }
        (left, right) => Err(format!("type mismatch: {} {} {}", left, operator, right)),
    }
}

fn eval_integer_infix_expression(left: i64, operator: Token, right: i64) -> Result<Object, String> {
    match operator {
        Token::Plus => Ok(Object::Integer(left + right)),
        Token::Minus => Ok(Object::Integer(left - right)),
        Token::Asterisk => Ok(Object::Integer(left * right)),
        Token::Slash => Ok(Object::Integer(left / right)),
        Token::Lt => Ok(native_bool_to_boolean_object(left < right)),
        Token::Gt => Ok(native_bool_to_boolean_object(left > right)),
        Token::Eq => Ok(native_bool_to_boolean_object(left == right)),
        Token::NotEq => Ok(native_bool_to_boolean_object(left != right)),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_boolean_infix_expression(
    left: bool,
    operator: Token,
    right: bool,
) -> Result<Object, String> {
    match operator {
        Token::Eq => Ok(native_bool_to_boolean_object(left == right)),
        Token::NotEq => Ok(native_bool_to_boolean_object(left != right)),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_string_infix_expression(
    left: String,
    operator: Token,
    right: String,
) -> Result<Object, String> {
    match operator {
        Token::Plus => Ok(Object::String(format!("{}{}", left, right))),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}
fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Statement,
    alternative: Option<Box<Statement>>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    let condition = eval_expression(condition, env.clone())?;
    if is_truthy(condition) {
        eval_statement(consequence, env.clone())
    } else if let Some(alternative) = alternative {
        eval_statement(*alternative, env)
    } else {
        Ok(NULL)
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        _ => true,
    }
}

fn eval_identifier_expression(
    name: String,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, String> {
    match env.borrow().get(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(format!("identifier not found: {}", name)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Ok(_) => panic!("no error returned for {}", input),
                Err(error) => assert_eq!(error, expected),
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
                body: Statement::Block(vec![Statement::Expression(Expression::Infix {
                    left: Box::new(Expression::Identifier("x".to_string())),
                    operator: Token::Plus,
                    right: Box::new(Expression::IntegerLiteral(2)),
                })]),
                env: Environment::new_enclosed_environment(Environment::new()),
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
                        body: Statement::Block(vec![Statement::Expression(Expression::Infix {
                            left: Box::new(Expression::Identifier("x".to_string())),
                            operator: Token::Plus,
                            right: Box::new(Expression::Identifier("x".to_string())),
                        })]),
                        env: Environment::new_enclosed_environment(Environment::new()),
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
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (r#"len([1, 2, 3])"#, Object::Integer(3)),
            (r#"len([])"#, Object::Integer(0)),
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

    fn test_eval(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let env = Environment::new();
        eval(program, env)
    }
}
