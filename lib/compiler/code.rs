use std::{fmt, io::Cursor};

use anyhow::{bail, Context, Result};
use byteorder::{ReadBytesExt, WriteBytesExt};

#[derive(Debug, Clone, PartialEq)]
pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }

    /// Mostly useful for testing when specifying the instructions individually
    pub fn from(instructions: Vec<Vec<u8>>) -> Self {
        Self(instructions.into_iter().flatten().collect::<Vec<u8>>())
    }

    pub fn inner(&self) -> &Vec<u8> {
        &self.0
    }

    pub fn extend(&mut self, instruction: Vec<u8>) -> usize {
        let instr_start_pos = self.0.len();
        self.0.extend(instruction);
        instr_start_pos
    }

    /// Convert an opcode and a vector of operands into a byte vector instruction
    pub fn make(opcode: Opcode, operands: Vec<usize>) -> Result<Vec<u8>> {
        let widths = opcode.operand_width();
        let name = opcode.name();

        let mut instruction = vec![opcode.into()];
        for (i, operand) in operands.into_iter().enumerate() {
            match widths.get(i) {
                Some(2) => {
                    instruction.write_u16::<byteorder::BigEndian>(operand.try_into()?)?;
                }
                Some(1) => {
                    instruction.write_u8(operand.try_into()?)?;
                }
                Some(width) => bail!("unknown operand width: {}", width),
                None => bail!("no operand width for opcode: {}", name),
            }
        }
        Ok(instruction)
    }
}

impl fmt::Display for Instructions {
    /// Print the instructions in a human-readable format
    /// TODO: handle errors instead of unwraping
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut instructions = Cursor::new(&self.0);
        while !instructions.is_empty() {
            write!(f, "{:04} ", instructions.position())?;
            let instruction = Opcode::try_from(instructions.read_u8().unwrap()).unwrap();
            write!(f, "{}", instruction.name())?;

            for width in instruction.operand_width().iter() {
                match width {
                    2 => {
                        let operand = instructions
                            .read_u16::<byteorder::BigEndian>()
                            .context("could not read 2-byte operand")
                            .unwrap();
                        write!(f, " {}", operand)?;
                    }
                    1 => {
                        let operand = instructions
                            .read_u8()
                            .context("could not read 1-byte operand")
                            .unwrap();
                        write!(f, " {}", operand)?;
                    }
                    _ => panic!("Unknown operand width {}", width),
                }
            }

            write!(f, "\n",)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant,
    Add,
}

impl Opcode {
    fn name(&self) -> &'static str {
        match self {
            Opcode::Constant => "Constant",
            Opcode::Add => "Add",
        }
    }

    /// The number of elements is the number of operands for the opcode
    ///
    /// The value of each element is the byte width of the operand
    ///
    /// E.g. OpConstant has one operand, which is a 2-byte unsigned integer (u16).
    /// In this case, this operand is an index into the constants pool that contains the actual value.
    fn operand_width(&self) -> Vec<usize> {
        match self {
            Opcode::Constant => vec![2],
            Opcode::Add => vec![],
        }
    }
}

impl TryFrom<u8> for Opcode {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self> {
        let opcode = match value {
            0 => Opcode::Constant,
            1 => Opcode::Add,
            _ => bail!("unknown opcode: {}", value),
        };
        Ok(opcode)
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::Constant => 0,
            Opcode::Add => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = vec![
            (Opcode::Constant, vec![65534], vec![0u8, 255u8, 254u8]),
            (Opcode::Add, vec![], vec![1u8]),
        ];

        for (opcode, operands, expected) in tests {
            let instruction = Instructions::make(opcode, operands).unwrap();
            assert_eq!(instruction, expected);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = Instructions::from(vec![
            Instructions::make(Opcode::Add, vec![]).unwrap(),
            Instructions::make(Opcode::Constant, vec![2]).unwrap(),
            Instructions::make(Opcode::Constant, vec![65535]).unwrap(),
        ]);

        let expected = r#"0000 Add
0001 Constant 2
0004 Constant 65535
"#;

        assert_eq!(instructions.to_string(), expected);
    }
}
