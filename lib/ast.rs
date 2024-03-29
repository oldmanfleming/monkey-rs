use std::fmt;

use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let { name: Expression, value: Expression },
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {name} = {value}"),
            Statement::Return(expression) => write!(f, "return {expression}"),
            Statement::Expression(expression) => write!(f, "{expression}"),
            Statement::Block(statements) => {
                for statement in statements {
                    writeln!(f, "{}", statement)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    Prefix {
        operator: Token,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    FunctionLiteral {
        name: Option<String>,
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{ident}"),
            Expression::IntegerLiteral(value) => write!(f, "{value}"),
            Expression::StringLiteral(value) => write!(f, "{value}"),
            Expression::BooleanLiteral(value) => write!(f, "{value}"),
            Expression::ArrayLiteral(values) => {
                let values = values
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{values}]")
            }
            Expression::HashLiteral(pairs) => {
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{pairs}}}")
            }
            Expression::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({left} {operator} {right})"),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if {condition} {{ {consequence} }}",)?;
                if let Some(alternative) = alternative {
                    write!(f, " else {{ {alternative} }}",)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral {
                name: _,
                parameters,
                body,
            } => {
                let params = parameters
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fn({params}) {{ {body} }}",)?;
                Ok(())
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{function}({args})",)?;
                Ok(())
            }
            Expression::Index { left, index } => write!(f, "({left}[{index}])"),
        }
    }
}
