use crate::interpreter::{
    ast
};

pub mod object_system {
    pub enum Object {
        Integer(Integer),
        Boolean(Boolean),
        Null,
    }
    
    pub enum ObjectType {
        INTEGER,
        BOOLEAN,
        NULL,
    }
    
    trait ObjectInterface {
        fn inspect(&self) -> String;
        fn object_type(&self) -> ObjectType;
    }

    impl Object {
        pub fn inspect(&self) -> String {
            match self {
                Object::Integer(i) => i.inspect(),
                Object::Boolean(b) => b.inspect(),
                Object::Null => "null".to_string(),            
            }
        }
    }
    
    
    pub struct Integer {
        pub value: i64,
    }
    
    impl ObjectInterface for Integer {
        fn inspect(&self) -> String {
            format!("{}", self.value)
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::INTEGER
        }
    }
    
    
    pub struct Boolean {
        pub value: bool,
    }
    
    impl ObjectInterface for Boolean {
        fn inspect(&self) -> String {
            format!("{}", self.value)
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::BOOLEAN
        }
    }
    
    pub struct Null;
    
    impl ObjectInterface for Null {
        fn inspect(&self) -> String {
            format!("null")
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::NULL
        }
    }

}


pub fn eval(program: ast::Program) -> object_system::Object {
    let mut result = object_system::Object::Null;
    for statement in program.statements {
        result = eval_statement(statement);
    }
    result
}

fn eval_statement(statement: ast::Statement) -> object_system::Object {
    match statement {
        ast::Statement::ExpressionStatement(expression_statement) => {
            eval_expression(expression_statement.expression)
        }
        _ => object_system::Object::Null,
    }
}

fn eval_expression(expression: ast::Expression) -> object_system::Object {
    match expression {
        ast::Expression::IntegerLiteral(integer_literal) => {
            object_system::Object::Integer(object_system::Integer {
                value: integer_literal.value,
            })
        }
        ast::Expression::BooleanLiteral(bool_literal) => {
            object_system::Object::Boolean(object_system::Boolean{
                value: bool_literal.value,
            })
        }
        ast::Expression::PrefixExpression(prefix_expression) => {
            let right = eval_expression(*prefix_expression.right);
            eval_prefix_expression(prefix_expression.operator, right)
        }
        ast::Expression::InfixExpression(infix_expression) => {
            let left = eval_expression(*infix_expression.left);
            let right = eval_expression(*infix_expression.right);
            eval_infix_expression(infix_expression.operator, left, right)
        }
        _ => object_system::Object::Null,
    }
}

fn eval_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {

    if match left { object_system::Object::Integer(_) => true, _ => false } &&
        match right { object_system::Object::Integer(_) => true, _ => false } {
        return eval_integer_infix_expression(operator, left, right);
    } else if match left { object_system::Object::Boolean(_) => true, _ => false } &&
        match right { object_system::Object::Boolean(_) => true, _ => false } {
        return eval_boolean_infix_expression(operator, left, right);
    } else {
        return object_system::Object::Null;
    }
}

fn eval_boolean_infix_expression(operator:String, left:object_system::Object, right:object_system::Object) -> object_system::Object {
    let object_system::Object::Boolean(left_val) = left else { return object_system::Object::Null; };
    let object_system::Object::Boolean(right_val) = right else { return object_system::Object::Null; };

    match operator.as_str() {
        "==" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value == right_val.value,
        }),
        "!=" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value != right_val.value,
        }),
        _ => object_system::Object::Null,
    }
}

fn eval_integer_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {
    let object_system::Object::Integer(left_val) = left else { return object_system::Object::Null; };
    let object_system::Object::Integer(right_val) = right else { return object_system::Object::Null; };

    match operator.as_str() {
        "+" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value + right_val.value,
        }),
        "-" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value - right_val.value,
        }),
        "*" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value * right_val.value,
        }),
        "/" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value / right_val.value,
        }),
        "<" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value < right_val.value,
        }),
        ">" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value > right_val.value,
        }),
        "==" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value == right_val.value,
        }),
        "!=" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value != right_val.value,
        }),
        _ => object_system::Object::Null,
    }
}

fn eval_prefix_expression(operator: String, right: object_system::Object) -> object_system::Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => object_system::Object::Null,
    }
}

fn eval_minus_prefix_operator_expression(right: object_system::Object) -> object_system::Object { 
    match right {
        object_system::Object::Integer(integer) => {
            object_system::Object::Integer(object_system::Integer {
                value: -integer.value,
            })
        }
        _ => object_system::Object::Null,
    }
}

fn eval_bang_operator_expression(right: object_system::Object) -> object_system::Object {
    match right {
        object_system::Object::Boolean(boolean) => {
            object_system::Object::Boolean(object_system::Boolean {
                value: !boolean.value,
            })
        }
        object_system::Object::Null => object_system::Object::Boolean(object_system::Boolean {
            value: true,
        }),
        _ => object_system::Object::Boolean(object_system::Boolean {
            value: false,
        }),
    }
}



#[cfg(test)]
mod tests {
    use crate::interpreter::*;
    use super::*;

    #[test]
    fn test_bang_operator() {
        struct Test {
            input: String,
            expected: bool,
        }

        let inputs = vec![
            Test {input: "!true".to_string(), expected: false},
            Test {input: "!false".to_string(), expected: true},
            Test {input: "!0".to_string(), expected: false},
            Test {input: "!5".to_string(), expected: false},
            Test {input: "!!true".to_string(), expected: true},
            Test {input: "!!false".to_string(), expected: false},
            Test {input: "!!5".to_string(), expected: true},
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = parser::Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(_) => panic!("Error parsing program"),
            };
            let evaluated = eval(program);
            assert_eq!(evaluated.inspect(), input.expected.to_string());
        }
    }

    #[test]
    fn test_integer_eval() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "5".to_string(), expected: 5},
            Test {input: "10".to_string(), expected: 10},
            Test {input: "-5".to_string(), expected: -5},
            Test {input: "-10".to_string(), expected: -10},
            Test {input: "5 + 5 + 5 + 5 - 10".to_string(), expected: 10},
            Test {input: "2 * 2 * 2 * 2 * 2".to_string(), expected: 32},
            Test {input: "-50 + 100 + -50".to_string(), expected: 0},
            Test {input: "5 * 2 + 10".to_string(), expected: 20},
            Test {input: "5 + 2 * 10".to_string(), expected: 25},
            Test {input: "20 + 2 * -10".to_string(), expected: 0},
            Test {input: "50 / 2 * 2 + 10".to_string(), expected: 60},
            Test {input: "2 * (5 + 10)".to_string(), expected: 30},
            Test {input: "3 * 3 * 3 + 10".to_string(), expected: 37},
            Test {input: "3 * (3 * 3) + 10".to_string(), expected: 37},
            Test {input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(), expected: 50},
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = parser::Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(_) => panic!("Error parsing program"),
            };
            let evaluated = eval(program);
            assert_eq!(evaluated.inspect(), input.expected.to_string());
        }
    }

    #[test]
    fn test_boolean_eval() {
        struct Test {
            input: String,
            expected: bool,
        }

        let inputs = vec![
            Test {input: "true".to_string(), expected: true},
            Test {input: "false".to_string(), expected: false},
            Test {input: "1 < 2".to_string(), expected: true},
            Test {input: "1 > 2".to_string(), expected: false},
            Test {input: "1 < 1".to_string(), expected: false},
            Test {input: "1 > 1".to_string(), expected: false},
            Test {input: "1 == 1".to_string(), expected: true},
            Test {input: "1 != 1".to_string(), expected: false},
            Test {input: "1 == 2".to_string(), expected: false},
            Test {input: "1 != 2".to_string(), expected: true},
            Test {input: "true == true".to_string(), expected: true},
            Test {input: "false == false".to_string(), expected: true},
            Test {input: "true == false".to_string(), expected: false},
            Test {input: "true != false".to_string(), expected: true},
            Test {input: "false != true".to_string(), expected: true},
            Test {input: "(1 < 2) == true".to_string(), expected: true},
            Test {input: "(1 < 2) == false".to_string(), expected: false},
            Test {input: "(1 > 2) == true".to_string(), expected: false},
            Test {input: "(1 > 2) == false".to_string(), expected: true},
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = parser::Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(_) => panic!("Error parsing program"),
            };
            let evaluated = eval(program);
            assert_eq!(evaluated.inspect(), input.expected.to_string());
        }
    }
}
