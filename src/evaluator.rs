#![allow(dead_code)]

use crate::{
    ast::{Expression, Program, Statement},
    object::Object,
    token::Token,
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(program: Program) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in program.statements {
        result = match eval_statement(statement)? {
            Object::ReturnValue(value) => return Ok(*value),
            object => object,
        };
    }
    Ok(result)
}

fn eval_statement(statement: Statement) -> Result<Object, String> {
    let object = match statement {
        Statement::Expression(expression) => eval_expression(expression)?,
        Statement::Block(statements) => eval_block_statement(statements)?,
        Statement::Return(expression) => {
            let value = eval_expression(expression)?;
            return Ok(Object::ReturnValue(Box::new(value)));
        }
        expression_statement => Err(format!("{} not implemented", expression_statement))?,
    };
    Ok(object)
}

fn eval_block_statement(statements: Vec<Statement>) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement)?;
        if let Object::ReturnValue(_) = result {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_expression(expression: Expression) -> Result<Object, String> {
    let object = match expression {
        Expression::IntegerLiteral(value) => Object::Integer(value),
        Expression::BooleanLiteral(value) => native_bool_to_boolean_object(value),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(*right)?;
            eval_prefix_expression(operator, right)?
        }
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left)?;
            let right = eval_expression(*right)?;
            eval_infix_expression(left, operator, right)?
        }
        Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, *consequence, alternative)?,
        expression => Err(format!("{} not implemented", expression))?,
    };
    Ok(object)
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
) -> Result<Object, String> {
    let condition = eval_expression(condition)?;
    if is_truthy(condition) {
        eval_statement(consequence)
    } else if let Some(alternative) = alternative {
        eval_statement(*alternative)
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
            let evaluated = test_eval(input);
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
            let evaluated = test_eval(input);
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
            let evaluated = test_eval(input);
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
            let evaluated = test_eval(input);
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
            let evaluated = test_eval(input);
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
        ];

        for (input, expected) in tests {
            let evaluated = test_eval_result(input);
            match evaluated {
                Ok(_) => panic!("no error returned for {}", input),
                Err(error) => assert_eq!(error, expected),
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        eval(program).unwrap()
    }

    fn test_eval_result(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        eval(program)
    }
}
