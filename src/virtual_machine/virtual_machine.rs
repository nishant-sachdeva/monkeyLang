use crate::object_system::Object;
use crate::compiler::compiler::RawAssembly;
use crate::virtual_machine::bytecode::{
    OpCode,
    opcode_lookup,
};

pub const STACK_SIZE: usize = 2048;

pub struct VmStack {
    pub stack: Vec<Object>,
    pub stack_pointer: usize,
}

impl VmStack {
    pub fn new() -> Self {
        VmStack {
            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,
        }
    }

    pub fn stack_top(&self) -> Result<Object, String> {
        if self.stack_pointer == 0 {
            return Err(String::from("Stack is empty"));
        }

        let stack_top = self.stack.get(self.stack_pointer - 1).unwrap();

        return Ok(stack_top.clone());
    }

    pub fn stack_pop(&mut self) -> Result<Object, String> {
        if self.stack_pointer == 0 {
            return Err(String::from("Stack is empty"));
        }

        let stack_top = self.stack.pop().unwrap();
        self.stack_pointer -= 1;

        return Ok(stack_top);
    }

    pub fn push_constant(&mut self, object: Object) -> Result<(), String> {
        self.stack.push(object);
        self.stack_pointer += 1;

        return Ok(());
    }
}

pub struct VirtualMachine {
    pub stack: VmStack,
    pub assembly: RawAssembly,
}

impl VirtualMachine {
    pub fn new(assembly: RawAssembly) -> Self {
        VirtualMachine {
            stack: VmStack::new(),
            assembly,
        }
    }

    pub fn stack_top(&self) -> Result<Object, String> {
        self.stack.stack_top()
    }

    pub fn stack_pop(&mut self) -> Result<Object, String> {
        self.stack.stack_pop()
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut stack_pointer = 0;
        while stack_pointer < self.assembly.instructions.len() {
            let opcode = opcode_lookup(
                self.assembly.instructions[stack_pointer..stack_pointer+2].parse::<usize>().unwrap()
            ).unwrap();
            stack_pointer += 2;

            match opcode {
                OpCode::OpConstant => {
                    let const_index = self.assembly.instructions[stack_pointer..stack_pointer+4].parse::<usize>().unwrap();
                    stack_pointer += 4;

                    match self.stack.push_constant(
                        self.assembly.constants[const_index].clone()
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpAdd => {
                    let right = match self.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };

                    let left = match self.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };

                    match (left, right) {
                        (Object::Integer(left), Object::Integer(right)) => {
                            let result = Object::Integer(
                                crate::object_system::Integer {
                                    value: left.value + right.value,
                                }
                            );

                            match self.stack.push_constant(result) {
                                Ok(_) => (),
                                Err(e) => return Err(e),
                            }
                        },
                        _ => return Err(String::from("Unsupported types for addition")),
                    }
                }
            }
        }

        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, interpreter::lexer::Lexer};
    use crate::object_system::{
        Object,
        Integer,
    };
    use crate::compiler::compiler::Compiler;
    use crate::interpreter::ast;

    use super::*;

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program.unwrap()
    }

    fn test_expected_object(expected: Object, actual: Object) -> Result<(), String> {
        match expected {
            Object::Integer(integer) => {
                test_integer_object(integer.value, actual)
            },
            _ => {
                return Err(format!("Wrong object type.\nExpected: Integer\nActual: {:?}", actual));
            }
        }
    }

    fn test_integer_object(expected: i64, actual: Object) -> Result<(), String> {
        match actual {
            Object::Integer(integer) => {
                if integer.value != expected {
                    return Err(format!("Wrong integer value.\nExpected: {}\nActual: {}", expected, integer.value));
                }
            },
            _ => {
                return Err(format!("Wrong object type.\nExpected: Integer\nActual: {:?}", actual));
            }
        }

        return Ok(());
    }

    struct VirtualMachineTest {
        pub input: String,
        pub expected_stack: Vec<Object>,
    }

    fn run_vm_tests(tests: Vec<VirtualMachineTest>) {
        for test in tests {
            let mut program = parse(test.input);

            let mut compiler = Compiler::new();
            
            match compiler.compile(&mut program) {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            }

            let mut vm = VirtualMachine::new(compiler.raw_assembly().unwrap());

            match vm.run() {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            }

            let stack_element = vm.stack_top().unwrap();

            test_expected_object(
                test.expected_stack.get(0).unwrap().clone(),
                stack_element.clone()
            ).unwrap();
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VirtualMachineTest {
                input: String::from("1 + 2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 3 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
        ];

        run_vm_tests(tests);
    }

}
