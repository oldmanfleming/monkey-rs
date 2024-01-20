use anyhow::{bail, Result};

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

use super::{
    code::{Instructions, Opcode},
    object::Object,
};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<()> {
        for statement in program.statements.into_iter() {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::Pop, vec![])?;
            }
            _ => bail!("unimplemented statement: {}", statement),
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<()> {
        match expression {
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                // The order of the operands is important for the VM.
                // We re-order the operands for Lt to create an Gt (compiler fun).
                // E.g 1 < 2 => 2 > 1
                if operator == Token::Lt {
                    self.compile_expression(*right)?;
                    self.compile_expression(*left)?;
                    self.emit(Opcode::GreaterThan, vec![])?;
                    return Ok(());
                }

                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match operator {
                    Token::Plus => self.emit(Opcode::Add, vec![])?,
                    Token::Minus => self.emit(Opcode::Sub, vec![])?,
                    Token::Asterisk => self.emit(Opcode::Mul, vec![])?,
                    Token::Slash => self.emit(Opcode::Div, vec![])?,
                    Token::Eq => self.emit(Opcode::Equal, vec![])?,
                    Token::NotEq => self.emit(Opcode::NotEqual, vec![])?,
                    Token::Gt => self.emit(Opcode::GreaterThan, vec![])?,
                    _ => bail!("unimplemented operator: {:?}", operator),
                };
            }
            Expression::IntegerLiteral(value) => {
                let integer = Object::Integer(value);
                self.constants.push(integer);
                self.emit(Opcode::Constant, vec![self.constants.len() - 1])?;
            }
            Expression::BooleanLiteral(value) => {
                if value {
                    self.emit(Opcode::True, vec![])?;
                } else {
                    self.emit(Opcode::False, vec![])?;
                }
            }
            _ => bail!("unimplemented expression: {:?}", expression),
        }

        Ok(())
    }

    fn emit(&mut self, opcode: Opcode, operands: Vec<usize>) -> Result<usize> {
        let instruction = Instructions::make(opcode, operands)?;
        Ok(self.instructions.extend(instruction))
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    use super::*;

    #[test]
    fn test_compile_integer_arithmetic() {
        let tests = vec![
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Add, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Sub, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Mul, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "2 / 1",
                vec![Object::Integer(2), Object::Integer(1)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Div, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1; 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            (
                "true",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "false",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::False, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::GreaterThan, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::GreaterThan, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Equal, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::NotEqual, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "true == false",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::False, vec![]).unwrap(),
                    Instructions::make(Opcode::Equal, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "true != false",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::False, vec![]).unwrap(),
                    Instructions::make(Opcode::NotEqual, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            // (
            //     "!true",
            //     vec![],
            //     Instructions::from(vec![
            //         Instructions::make(Opcode::True, vec![])?,
            //         Instructions::make(Opcode::Bang, vec![])?,
            //         Instructions::make(Opcode::Pop, vec![])?,
            //     ]),
            // ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    fn run_compiler_tests(
        input: &str,
        expected_constants: Vec<Object>,
        expected_instructions: Instructions,
    ) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();
        let bytecode = compiler.bytecode();

        test_instructions(expected_instructions, bytecode.instructions);
        test_constants(expected_constants, bytecode.constants);
    }

    fn test_instructions(expected: Instructions, actual: Instructions) {
        let expected_data = expected.inner();
        let actual_data = actual.inner();

        assert_eq!(
            expected_data.len(),
            actual_data.len(),
            "wrong instruction length",
        );

        for (i, instruction) in expected_data.iter().enumerate() {
            assert_eq!(
                actual_data[i], *instruction,
                "wrong instruction at {}, wanted={}, got={}",
                i, expected, actual
            );
        }
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
        assert_eq!(expected.len(), actual.len(), "wrong constants length");

        for (i, constant) in expected.iter().enumerate() {
            assert_eq!(
                actual[i], *constant,
                "wrong constant at {}, wanted={}, got={}",
                i, constant, actual[i]
            );
        }
    }
}
