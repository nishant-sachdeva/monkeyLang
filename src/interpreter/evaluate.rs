use crate::interpreter::{
    ast
};

pub mod object_system {
    pub enum Object {
        Integer(Integer),
        Boolean(Boolean),
        ReturnValue(ReturnValue),
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
                Object::ReturnValue(rv) => rv.inspect(),
                Object::Null => "0".to_string(),            
            }
        }

        pub fn object_type(&self) -> ObjectType {
            match self {
                Object::Integer(i) => i.object_type(),
                Object::Boolean(b) => b.object_type(),
                Object::ReturnValue(rv) => rv.object_type(),
                Object::Null => ObjectType::NULL,            
            }
        }
    }

    pub struct ReturnValue {
        pub value: Box<Object>,
    }

    impl ObjectInterface for ReturnValue {
        fn inspect(&self) -> String {
            self.value.inspect()
        }

        fn object_type(&self) -> ObjectType {
            self.value.object_type()
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
            format!("0")
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::NULL
        }
    }

}


pub fn eval(program: ast::Program) -> object_system::Object {
    eval_statements(program.statements)
}

pub fn eval_block_statement(block: ast::BlockStatement) -> object_system::Object {
    eval_statements(block.statements)
}

fn eval_statements(statements: Vec<ast::Statement>) -> object_system::Object {
    let mut result = object_system::Object::Null;
    for statement in statements {
        result = eval_statement(statement);
        if let object_system::Object::ReturnValue(_) = result {
            return result;
        }
    }
    result
}

fn eval_statement(statement: ast::Statement) -> object_system::Object {
    match statement {
        ast::Statement::ExpressionStatement(expression_statement) => {
            eval_expression(expression_statement.expression)
        }
        ast::Statement::ReturnStatement(return_statement) => {
            object_system::Object::ReturnValue(object_system::ReturnValue {
                value: Box::new(eval_expression(return_statement.return_value)),
            })
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
        ast::Expression::IfExpression(if_expression) => {
            eval_if_expression(if_expression)
        }
        _ => object_system::Object::Null,
    }
}

fn eval_if_expression(if_expression: ast::IfExpression) -> object_system::Object {
    let condition = eval_expression(*if_expression.condition);
    if match condition {
        object_system::Object::Boolean(b) => b.value,
        object_system::Object::Integer(_i) => true,
        _ => false,
    } {
        eval_block_statement(if_expression.consequence)
    } else if let Some(alternative) = if_expression.alternative {
        eval_block_statement(alternative)
    } else {
        object_system::Object::Null
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
    fn test_return_statements() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "return 10;".to_string(), expected: 10},
            Test {input: "return 10; 9;".to_string(), expected: 10},
            Test {input: "return 2 * 5; 9;".to_string(), expected: 10},
            Test {input: "9; return 2 * 5; 9;".to_string(), expected: 10},
            Test {input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }".to_string(), expected: 10},
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = parser::Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(_) => panic!("Parser returned None"),
            };

            // send program for evaluation
            let result = eval(program);
            assert_eq!(result.inspect(), input.expected.to_string());

        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "if (true) { 10 }".to_string(), expected: 10},
            Test {input: "if (false) { 10 }".to_string(), expected: 0},
            Test {input: "if (1) { 10 }".to_string(), expected: 10},
            Test {input: "if (1 < 2) { 10 }".to_string(), expected: 10},
            Test {input: "if (1 > 2) { 10 }".to_string(), expected: 0},
            Test {input: "if (1 > 2) { 10 } else { 20 }".to_string(), expected: 20},
            Test {input: "if (1 < 2) { 10 } else { 20 }".to_string(), expected: 10},
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
