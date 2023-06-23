use crate::{
    interpreter::evaluate::object_system::*,
    virtual_machine::bytecode::{
        Instructions,
        OpCode,
        make_bytecode,
        get_raw_assembly,
    },
    interpreter::ast,
};

#[derive(Debug, Clone, PartialEq)]
pub struct RawAssembly {
    pub instructions: String,
    pub constants: Vec<Object>,
}

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, _program: &mut ast::Program) -> Result<(), String> {
        for statement in _program.statements.iter() {
            let result = self.compile_statement(statement);

            match result {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        }
        return Ok(());
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), String> {
        match statement {
            ast::Statement::ExpressionStatement(expression_statement) => {
                let result = self.compile_expression(&expression_statement.expression);

                match result {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }

                let _ = match self.emit(OpCode::OpPop, vec![]) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };
            },
            _ => return Err(format!("Statement type not supported: {:?}", statement)),
        }

        return Ok(());
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), String> {
        match expression {
            ast::Expression::PrefixExpression(prefix) => {
                let _ = match self.compile_expression(&prefix.right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                let opcode = match prefix.operator.as_str() {
                    "!" => OpCode::OpBang,
                    "-" => OpCode::OpMinus,
                    _ => return Err(format!("Operator not supported: {}", prefix.operator)),
                };

                let _ = match self.emit(opcode, vec![]) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };
            },
            ast::Expression::InfixExpression(infix) => {
                let _ = match self.compile_expression(&infix.left) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                let _ = match self.compile_expression(&infix.right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                let opcode = match infix.operator.as_str() {
                    "+" => OpCode::OpAdd,
                    "-" => OpCode::OpSub,
                    "*" => OpCode::OpMul,
                    "/" => OpCode::OpDiv,
                    ">" => OpCode::OpGreaterThan,
                    "<" => OpCode::OpLessThan,
                    "==" => OpCode::OpEqual,
                    "!=" => OpCode::OpNotEqual,
                    _ => return Err(format!("Operator not supported: {}", infix.operator)),
                };

                let _ = match self.emit(opcode, vec![]) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };
            },
            ast::Expression::IntegerLiteral(integer) => {
                let integer_object = Object::Integer(Integer { value: integer.value });
                let constant_index = self.add_constant(integer_object);

                let result = self.emit(OpCode::OpConstant, vec![constant_index]);

                match result {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            ast::Expression::BooleanLiteral(boolean) => {
                let opcode = match boolean.value {
                    true => OpCode::OpTrue,
                    false => OpCode::OpFalse,
                };

                let result = self.emit(opcode, vec![]);

                match result {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => return Err(format!("Expression type not supported: {:?}", expression)),
        }

        return Ok(());
    }

    fn add_constant(&mut self, object: Object) -> usize {
        self.constants.push(object);
        return self.constants.len() - 1;
    }

    fn emit(&mut self, opcode: OpCode, operands: Vec<usize>) -> Result<(), String> {
        let instruction = match make_bytecode(opcode, operands) {
            Ok(instruction) => instruction,
            Err(e) => return Err(e),
        };

        self.instructions.push(instruction);

        return Ok(());
    }

    pub fn raw_assembly(&mut self) -> Result<RawAssembly, String> {
        return Ok(RawAssembly {
            instructions: match get_raw_assembly(self.instructions.clone()) {
                Ok(instructions) => instructions,
                Err(e) => return Err(e),
            },
            constants: self.constants.clone(),
        })
    }
}

#[cfg(test)]
mod test {
    use core::panic;
    use std::vec;
    use crate::{parser::Parser, interpreter::lexer::Lexer};
    use crate::virtual_machine::bytecode::{make_bytecode, OpCode};

    use super::*;

    pub struct CompilerTest {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Instructions,
    }

    pub fn test_instructions(expected: Instructions, actual: Instructions) -> Result<(), String> {
        if expected.len() != actual.len() {
            return Err(format!("Wrong instructions length.\nExpected: {}\nActual: {}", expected.len(), actual.len()));
        }

        for (i, expected_instruction) in expected.iter().enumerate() {
            let actual_instruction = actual.get(i).unwrap();

            if expected_instruction.len() != actual_instruction.len() {
                return Err(format!("Wrong instruction length at {}.\nExpected: {}\nActual: {}", i, expected_instruction.len(), actual_instruction.len()));
            }

            for (j, expected_byte) in expected_instruction.iter().enumerate() {
                let actual_byte = actual_instruction.get(j).unwrap();

                if expected_byte != actual_byte {
                    return Err(format!("Wrong byte at {}:{}.\nExpected: {}\nActual: {}", i, j, expected_byte, actual_byte));
                }
            }
        }

        return Ok(());
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) -> Result<(), String> {
        if expected.len() != actual.len() {
            return Err(format!("Wrong constants length.\nExpected: {}\nActual: {}", expected.len(), actual.len()));
        }

        for (i, expected_constant) in expected.iter().enumerate() {
            let actual_constant = actual.get(i).unwrap();

            if expected_constant != actual_constant {
                return Err(format!("Wrong constant at {}.\nExpected: {:?}\nActual: {:?}", i, expected_constant, actual_constant));
            }
        }

        return Ok(());
    }

    pub fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        return parser.parse_program().unwrap();
    }

    fn run_compiler_tests(tests: Vec<CompilerTest>) {
        for test in tests {
            let mut program = parse(test.input);

            let mut compiler = Compiler::new();
            let result = match compiler.compile(&mut program) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            };

            assert_eq!(result, Ok(()));

            let _raw_assembly = match compiler.raw_assembly() {
                Ok(bytecode) => bytecode,
                Err(e) => panic!("{e}"),
            };

            _ = match test_instructions(test.expected_instructions, compiler.instructions) {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            };

            _ = match test_constants(test.expected_constants, compiler.constants) {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            };
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let input = vec![
            CompilerTest {
                input: String::from("1 + 2"),
                expected_constants: vec![
                    Object::Integer(Integer { value: 1 }),
                    Object::Integer(Integer { value: 2 }),
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpAdd, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 - 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpSub, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 * 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpMul, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("2/1"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 2}),
                    Object::Integer(Integer {value: 1})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpDiv, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("-1"),
                expected_constants: vec![
                    Object::Integer(
                        Integer {value: 1}
                    )
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpMinus, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ]
            }
        ];
        run_compiler_tests(input);
    }

    #[test]
    fn test_boolean_expressions() {
        let input = vec![
            CompilerTest {
                input: String::from("true"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpFalse, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 > 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpGreaterThan, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 < 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpLessThan, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 == 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpEqual, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("1 != 2"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpNotEqual, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("true == false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpFalse, vec![]).unwrap(),
                    make_bytecode(OpCode::OpEqual, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("true != false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpFalse, vec![]).unwrap(),
                    make_bytecode(OpCode::OpNotEqual, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            },
            CompilerTest {
                input: String::from("!true"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpBang, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ]
            }
        ];
        run_compiler_tests(input);
    }
}