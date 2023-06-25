use crate::{
    interpreter::{evaluate::object_system::*},
    virtual_machine::{
        bytecode::{
            OpCode,
            make_bytecode,
            get_raw_instruction,
            Instruction,
            opcode_lookup,
        },
        symbol_table::SymbolTable,
    },
    interpreter::ast,
};

pub type InstructionPosition = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct RawAssembly {
    pub instructions: String,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmittedInstruction {
    pub opcode: Option<OpCode>,
    pub position: usize,
}

pub struct Compiler {
    pub raw_assembly: RawAssembly,
    pub last_instruction: EmittedInstruction,
    pub previous_instruction: EmittedInstruction,
    pub symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            raw_assembly: RawAssembly { instructions: String::new(), constants: Vec::new() },
            last_instruction: EmittedInstruction {
                opcode: None,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                opcode: None,
                position: 0,
            },
            symbol_table: SymbolTable::new(),
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

    pub fn compile_block(&mut self, block: &ast::BlockStatement) -> Result<(), String> {
        for statement in block.statements.iter() {
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
            ast::Statement::LetStatement(let_statement) => {
                let _ = match self.compile_expression(&let_statement.value) {
                    Ok(_) => (),
                    Err(e) => return Err(e)
                };

                let symbol = match self.symbol_table.define(let_statement.name.value.clone()) {
                    Ok(symbol) => symbol,
                    Err(e) => return Err(e),
                };
                let _ = self.emit(OpCode::OpSetGlobal, vec![symbol.index as usize]);
            }
            _ => return Err(format!("Statement type not supported: {:?}", statement)),
        }

        return Ok(());
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), String> {
        match expression {
            ast::Expression::Identifier(identifier) => {
                let symbol = match self.symbol_table.resolve(identifier.value.clone()) {
                    Ok(symbol) => symbol,
                    Err(e) => return Err(e),
                };

                let _ = match self.emit(OpCode::OpGetGlobal, vec![symbol.index as usize]) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };
            }
            ast::Expression::IfExpression(if_expr) => {
                let _ = match self.compile_expression(&if_expr.condition) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                // emit an OpJumpNotTruthy with a bogus value
                let jump_not_truthy_position = match self.emit(OpCode::OpJumpNotTruthy, vec![9999]) {
                    Ok(pos) => pos,
                    Err(e) => return Err(e),
                };

                let _ = match self.compile_block(&if_expr.consequence) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                if self.last_instruction.opcode == Some(OpCode::OpPop) {
                    self.remove_last_pop();
                }

                let jump_position = match self.emit(OpCode::OpJump, vec![9999]) {
                    Ok(pos) => pos,
                    Err(e) => return Err(e),
                };

                let after_consequence_position = self.raw_assembly.instructions.len()/2;
                _ = self.change_operand(jump_not_truthy_position, after_consequence_position);

                if if_expr.alternative.is_some() {
                    let _ = match self.compile_block(if_expr.alternative.as_ref().unwrap()) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };

                    if self.last_instruction.opcode == Some(OpCode::OpPop) {
                        self.remove_last_pop();
                    }
                } else {
                    let _ = self.emit(OpCode::OpNull, vec![]);
                }

                let after_alternative_position = self.raw_assembly.instructions.len()/2;
                _ = self.change_operand(jump_position, after_alternative_position);
                
            }
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
        self.raw_assembly.constants.push(object);
        return self.raw_assembly.constants.len() - 1;
    }

    fn emit(&mut self, opcode: OpCode, operands: Vec<usize>) -> Result<InstructionPosition, String> {
        let position_new_instruction = self.raw_assembly.instructions.len();
        _ = self.add_instruction(
            match make_bytecode(opcode, operands) {
                Ok(instruction) => instruction,
                Err(e) => return Err(e),
            }
        );

        self.set_last_instruction(opcode, position_new_instruction);

        return Ok(position_new_instruction);
    }

    pub fn set_last_instruction(&mut self, opcode: OpCode, position: usize) {
        self.previous_instruction = self.last_instruction.clone();

        let last_instruction = EmittedInstruction {
            opcode: Some(opcode),
            position: position,
        };

        self.last_instruction = last_instruction;
    }

    pub fn add_instruction(&mut self, instruction: Instruction) -> Result<(), String> {
        self.raw_assembly.instructions += &match get_raw_instruction(instruction) {
            Ok(raw_instruction) => raw_instruction,
            Err(e) => return Err(e),
        };
        
        return Ok(())
    }

    pub fn change_operand(&mut self, position: usize, operand: usize) -> Result<(), String> {
        let instruction = match make_bytecode(
            match opcode_lookup(
                match usize::from_str_radix(&self.raw_assembly.instructions[position..position+2], 16) {
                    Ok(opcode) => opcode,
                    Err(e) => return Err(e.to_string()),
                }
            ) {
                Ok(opcode) => opcode,
                Err(e) => return Err(e)
            },
            vec![operand],
        ) {
            Ok(instruction) => instruction,
            Err(e) => return Err(e),
        };

        self.replace_instruction(
            position,
            get_raw_instruction(instruction).unwrap().as_str()
        );

        return Ok(())
    }

    pub fn replace_instruction(&mut self, pos: usize, new_instruction: &str) {
        self.raw_assembly.instructions.replace_range(pos..pos + new_instruction.len(), new_instruction);
    }

    pub fn remove_last_pop(&mut self) {
        let last_instruction_position = self.last_instruction.position;
        self.raw_assembly.instructions = self.raw_assembly.instructions[..last_instruction_position].to_string();
        self.last_instruction = self.previous_instruction.clone();
    }

    pub fn raw_assembly(&mut self) -> Result<RawAssembly, String> {
        return Ok(self.raw_assembly.clone())
    }
}

#[cfg(test)]
mod test {
    use core::panic;
    use std::vec;
    use crate::{parser::Parser, interpreter::lexer::Lexer};
    use crate::virtual_machine::bytecode::{make_bytecode, OpCode, Instructions, get_raw_assembly, format_raw_assembly};

    use super::*;

    pub struct CompilerTest {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Instructions,
    }

    pub fn test_instructions(expected: String, actual: String) -> Result<(), String> {
        if expected != actual {
            return Err(
                format!("Wrong instructions.\nExpected: {}\nActual: {}", 
                format_raw_assembly(expected).unwrap(), format_raw_assembly(actual).unwrap()
            ));
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

            _ = match test_instructions(
                get_raw_assembly(test.expected_instructions).unwrap(),
                compiler.raw_assembly.instructions
            ) {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            };

            _ = match test_constants(test.expected_constants, compiler.raw_assembly.constants) {
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

    #[test]
    fn test_conditionals() {
        let inputs = vec![
            CompilerTest {
                input: String::from("if (true) { 10 }; 3333;"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 10}),
                    Object::Integer(Integer {value: 3333})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpJumpNotTruthy, vec![10]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpJump, vec![11]).unwrap(),
                    make_bytecode(OpCode::OpNull, vec![]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ]
            },
            CompilerTest {
                input: String::from("if (true) { 10 } else { 20 }; 3333;"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 10}),
                    Object::Integer(Integer {value: 20}),
                    Object::Integer(Integer {value: 3333})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpTrue, vec![]).unwrap(),
                    make_bytecode(OpCode::OpJumpNotTruthy, vec![10]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpJump, vec![13]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![2]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ],
            }
        ];
        run_compiler_tests(inputs);
    }

    #[test]
    fn test_global_let_statements() {
        let inputs = vec![
            CompilerTest {
                input: String::from("let one = 1; let two = 2;"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1}),
                    Object::Integer(Integer {value: 2})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpSetGlobal, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpSetGlobal, vec![1]).unwrap(),

                ]
            },
            CompilerTest {
                input: String::from("let one = 1; one;"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpSetGlobal, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpGetGlobal, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ]
            },
            CompilerTest {
                input: String::from("let one = 1; let two = one; two;"),
                expected_constants: vec![
                    Object::Integer(Integer {value: 1})
                ],
                expected_instructions: vec![
                    make_bytecode(OpCode::OpConstant, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpSetGlobal, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpGetGlobal, vec![0]).unwrap(),
                    make_bytecode(OpCode::OpSetGlobal, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpGetGlobal, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpPop, vec![]).unwrap(),
                ]
            },
        ];
        run_compiler_tests(inputs);
    }
}