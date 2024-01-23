use anyhow::{anyhow, bail, Result};

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

use super::{
    code::{Instructions, Opcode},
    object::Object,
    symbol_table::SymbolTable,
};

/// The compiler is responsible for converting the AST into bytecode.
/// It does this by walking the AST and emitting bytecode instructions for each node.
/// It uses a single pass with back-patching to handle forward references.
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    symbol_table: SymbolTable,
    last_instruction: Option<(Opcode, usize)>,
    prev_instruction: Option<(Opcode, usize)>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            instructions: Instructions::new(),
            constants: vec![],
            last_instruction: None,
            prev_instruction: None,
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<Bytecode> {
        for statement in program.statements.into_iter() {
            self.compile_statement(statement)?;
        }

        let bytecode = Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        };

        // We wipe the state of the compiler after compiling.
        // But we keep the symbol table and constants around so that any accumulated
        // state is kept like in the REPL.
        self.instructions = Instructions::new();
        self.last_instruction = None;
        self.prev_instruction = None;

        Ok(bytecode)
    }

    fn emit(&mut self, opcode: Opcode, operands: Vec<usize>) -> Result<usize> {
        let instruction = Instructions::make(opcode.clone(), operands)?;
        let pos = self.instructions.extend(instruction);

        self.prev_instruction = self.last_instruction.clone();
        self.last_instruction = Some((opcode, pos));

        Ok(pos)
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::Pop, vec![])?;
            }
            Statement::Block(statements) => {
                for statement in statements.into_iter() {
                    self.compile_statement(statement)?;
                }
            }
            Statement::Let { name, value } => {
                self.compile_expression(value)?;
                let symbol_name = match name {
                    Expression::Identifier(name) => name,
                    _ => bail!("expected identifier, got {:?}", name),
                };
                let symbol = self.symbol_table.define(symbol_name);
                self.emit(Opcode::SetGlobal, vec![symbol.index])?;
            }
            _ => bail!("unimplemented statement: {}", statement),
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<()> {
        match expression {
            Expression::Identifier(name) => {
                self.compile_identifier_expression(name)?;
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_conditional_expression(condition, consequence, alternative)?;
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.compile_infix_expression(operator, right, left)?;
            }
            Expression::Prefix { operator, right } => {
                self.compile_prefix_expression(right, operator)?;
            }
            Expression::IntegerLiteral(value) => {
                self.compile_integer_literal(value)?;
            }
            Expression::BooleanLiteral(value) => {
                self.compile_boolean_literal(value)?;
            }
            Expression::StringLiteral(value) => {
                self.compile_string_literal(value)?;
            }
            Expression::ArrayLiteral(elements) => {
                self.compile_array_literals(elements)?;
            }
            Expression::HashLiteral(pairs) => {
                self.compile_hash_literal(pairs)?;
            }
            Expression::Index { left, index } => {
                self.compile_index_expression(left, index)?;
            }
            _ => bail!("unimplemented expression: {:?}", expression),
        }

        Ok(())
    }

    fn compile_index_expression(
        &mut self,
        left: Box<Expression>,
        index: Box<Expression>,
    ) -> Result<()> {
        self.compile_expression(*left)?;
        self.compile_expression(*index)?;
        self.emit(Opcode::Index, vec![])?;
        Ok(())
    }

    fn compile_hash_literal(&mut self, pairs: Vec<(Expression, Expression)>) -> Result<()> {
        let num_pairs = pairs.len();

        for (key, value) in pairs.into_iter() {
            self.compile_expression(key)?;
            self.compile_expression(value)?;
        }

        self.emit(Opcode::Hash, vec![num_pairs])?;

        Ok(())
    }

    fn compile_array_literals(&mut self, elements: Vec<Expression>) -> Result<(), anyhow::Error> {
        let len = elements.len();
        for element in elements.into_iter() {
            self.compile_expression(element)?;
        }
        self.emit(Opcode::Array, vec![len])?;
        Ok(())
    }

    fn compile_identifier_expression(&mut self, name: String) -> Result<()> {
        let symbol = self
            .symbol_table
            .resolve(&name)
            .ok_or(anyhow!("could not find symbol {name}"))?;
        self.emit(Opcode::GetGlobal, vec![symbol.index])?;
        Ok(())
    }

    fn compile_boolean_literal(&mut self, value: bool) -> Result<()> {
        if value {
            self.emit(Opcode::True, vec![])?;
        } else {
            self.emit(Opcode::False, vec![])?;
        }
        Ok(())
    }

    fn compile_integer_literal(&mut self, value: i64) -> Result<()> {
        let integer = Object::Integer(value);
        self.constants.push(integer);
        self.emit(Opcode::Constant, vec![self.constants.len() - 1])?;
        Ok(())
    }

    fn compile_string_literal(&mut self, value: String) -> Result<()> {
        let string = Object::String(value);
        self.constants.push(string);
        self.emit(Opcode::Constant, vec![self.constants.len() - 1])?;
        Ok(())
    }

    fn compile_prefix_expression(&mut self, right: Box<Expression>, operator: Token) -> Result<()> {
        self.compile_expression(*right)?;
        match operator {
            Token::Bang => self.emit(Opcode::Bang, vec![])?,
            Token::Minus => self.emit(Opcode::Minus, vec![])?,
            _ => bail!("unimplemented operator: {:?}", operator),
        };
        Ok(())
    }

    fn compile_infix_expression(
        &mut self,
        operator: Token,
        right: Box<Expression>,
        left: Box<Expression>,
    ) -> Result<()> {
        // The order of the operands is important for the VM.
        // We re-order the operands for Lt to create a Gt (compiler fun).
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
        Ok(())
    }

    fn compile_conditional_expression(
        &mut self,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    ) -> Result<()> {
        self.compile_expression(*condition)?;

        // We add the jump instruction with a dummy value because we don't know how far we
        // have to jump until we evaluate the number of instructions in the consequence block
        let jump_pos = self.emit(Opcode::JumpNotTruthy, vec![9999])?;

        self.compile_statement(*consequence)?;

        // remove the last instruction from the consequence block if it's a pop
        // because we do want to keep the last value on the stack after evaluating the consequence
        if let Some((Opcode::Pop, pos)) = self.last_instruction.clone() {
            self.instructions.drain_at(pos);
            self.last_instruction = self.prev_instruction.clone();
        }

        // This is the jump over the else block that the if block will take
        let alternative_jump_pos = self.emit(Opcode::Jump, vec![9999])?;

        // update the jump position of the falsey jump to point to the start of the alternative block
        self.instructions
            .change_u16_operand(jump_pos, self.instructions.inner().len())?;

        match alternative {
            Some(alternative) => {
                self.compile_statement(*alternative)?;

                if let Some((Opcode::Pop, pos)) = self.last_instruction.clone() {
                    self.instructions.drain_at(pos);
                    self.last_instruction = self.prev_instruction.clone();
                }
            }
            None => {
                // if there is no alternative block, and the if block is not evaluated, we push a null value onto the stack
                self.emit(Opcode::Null, vec![])?;
            }
        }

        // update the jump position of the if block jump to point to the end of the alternative block
        self.instructions
            .change_u16_operand(alternative_jump_pos, self.instructions.inner().len())?;

        Ok(())
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
            (
                "-1",
                vec![Object::Integer(1)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Minus, vec![]).unwrap(),
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
            (
                "!true",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::Bang, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::JumpNotTruthy, vec![10]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Jump, vec![11]).unwrap(),
                    Instructions::make(Opcode::Null, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::True, vec![]).unwrap(),
                    Instructions::make(Opcode::JumpNotTruthy, vec![10]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Jump, vec![13]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            (
                "let one = 1; let two = 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::SetGlobal, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::SetGlobal, vec![1]).unwrap(),
                ]),
            ),
            (
                "let one = 1; one;",
                vec![Object::Integer(1)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::SetGlobal, vec![0]).unwrap(),
                    Instructions::make(Opcode::GetGlobal, vec![0]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "let one = 1; let two = one; two;",
                vec![Object::Integer(1)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::SetGlobal, vec![0]).unwrap(),
                    Instructions::make(Opcode::GetGlobal, vec![0]).unwrap(),
                    Instructions::make(Opcode::SetGlobal, vec![1]).unwrap(),
                    Instructions::make(Opcode::GetGlobal, vec![1]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            (
                "\"monkey\"",
                vec![Object::String("monkey".to_string())],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "\"mon\" + \"key\"",
                vec![
                    Object::String("mon".to_string()),
                    Object::String("key".to_string()),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Add, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            (
                "[]",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::Array, vec![0]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Array, vec![3]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Add, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![3]).unwrap(),
                    Instructions::make(Opcode::Sub, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![4]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![5]).unwrap(),
                    Instructions::make(Opcode::Mul, vec![]).unwrap(),
                    Instructions::make(Opcode::Array, vec![3]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            (
                "{}",
                vec![],
                Instructions::from(vec![
                    Instructions::make(Opcode::Hash, vec![0]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![3]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![4]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![5]).unwrap(),
                    Instructions::make(Opcode::Hash, vec![3]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Add, vec![]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![3]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![4]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![5]).unwrap(),
                    Instructions::make(Opcode::Mul, vec![]).unwrap(),
                    Instructions::make(Opcode::Hash, vec![2]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
        ];

        for (input, expected_constants, expected_instructions) in tests {
            run_compiler_tests(input, expected_constants, expected_instructions);
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            (
                "[1, 2, 3][1 + 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(1),
                    Object::Integer(1),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Array, vec![3]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![3]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![4]).unwrap(),
                    Instructions::make(Opcode::Add, vec![]).unwrap(),
                    Instructions::make(Opcode::Index, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
            (
                "{1: 2}[2 - 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(2),
                    Object::Integer(1),
                ],
                Instructions::from(vec![
                    Instructions::make(Opcode::Constant, vec![0]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![1]).unwrap(),
                    Instructions::make(Opcode::Hash, vec![1]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![2]).unwrap(),
                    Instructions::make(Opcode::Constant, vec![3]).unwrap(),
                    Instructions::make(Opcode::Sub, vec![]).unwrap(),
                    Instructions::make(Opcode::Index, vec![]).unwrap(),
                    Instructions::make(Opcode::Pop, vec![]).unwrap(),
                ]),
            ),
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
        let bytecode = compiler.compile(program).unwrap();

        test_instructions(expected_instructions, bytecode.instructions);
        test_constants(expected_constants, bytecode.constants);
    }

    fn test_instructions(expected: Instructions, actual: Instructions) {
        let expected_data = expected.inner();
        let actual_data = actual.inner();

        println!("expected={:?}", expected_data);
        println!("actual={:?}", actual_data);

        assert_eq!(
            expected_data.len(),
            actual_data.len(),
            "wrong instruction length, want=\n{}, got=\n{}",
            expected,
            actual
        );

        println!("GOT HERE!!!");

        for (i, instruction) in expected_data.iter().enumerate() {
            assert_eq!(
                actual_data[i], *instruction,
                "wrong instruction at {}, wanted=\n{}, got=\n{}",
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
