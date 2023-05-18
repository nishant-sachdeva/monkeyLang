use crate::object_system::Object;
use std::vec;

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

type Instructions = Vec<Instruction>;
type Instruction = Vec<u8>;

#[derive(Debug, Clone, PartialEq)]
pub struct Opcode {
    pub name: OpcodeName,
    pub operand_widths: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpcodeName {
    OpConstant,
}

impl Opcode {
    pub fn new(name: OpcodeName) -> Opcode {
        match name {
            OpcodeName::OpConstant => Opcode {
                name,
                operand_widths: vec![2],
            },
        }
    }
}

pub fn make_instruction(opcode: Opcode, operands: Vec<usize>) -> Result<Instruction, String> {
    let expected_operand_count = opcode.operand_widths.len();
    let actual_operand_count = operands.len();

    if expected_operand_count != actual_operand_count {
        return Err(format!(
            "Expected {} operands, got {}",
            expected_operand_count,
            actual_operand_count,
        ));
    } else {
        let mut byte_operands = Vec::new();
        for i in 0..operands.len() {
            let operand_width = opcode.operand_widths[i];
            let operand = operands[i];

            let mut bytes= match operand_width {
                2 => (operand as u16).to_be_bytes().to_vec(),
                _ => return Err(format!("Invalid operand width {}", operand_width)),
            };
            byte_operands.append(&mut bytes);
        }
        let mut instruction = vec![opcode.name as u8];
        instruction.append(&mut byte_operands);
        Ok(instruction)
    } 
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_instruction() {
        struct Test {
            opcode: Opcode,
            operands: Vec<usize>,
            expected: Instruction,
        }

        let inputs = vec![
            Test {
                opcode: Opcode::new(OpcodeName::OpConstant),
                operands: vec![65534],
                expected: vec![0, 255, 254],
            },
            Test {
                opcode: Opcode::new(OpcodeName::OpConstant),
                operands: vec![45],
                expected: vec![0, 0, 45],
            },
        ];

        for test in inputs {
            let result = match make_instruction(test.opcode, test.operands) {
                Ok(instruction) => instruction,
                Err(error) => panic!("Error: {}", error),
            };
            assert_eq!(result, test.expected);
        }
    }

}