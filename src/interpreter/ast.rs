use crate::interpreter::tokens::*;

/// Represents a node in the abstract syntax tree (AST) of the Monkey programming language.
/// 
/// # Examples
/// 
/// ```
/// use monkey_lang::interpreter::ast::{Program, Statement, LetStatement, Identifier, Expression, IntegerLiteral};
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
/// 
/// let program = Program {
///    statements: vec![
///       Statement::LetStatement(LetStatement {
///         token: Token::new(TokenType::LET, "let".to_string()),
///        name: Identifier {
///          token: Token::new(TokenType::IDENT, "myVar".to_string()),
///         value: "myVar".to_string(),
///       },
///      value: Expression::Identifier(Identifier {
///       token: Token::new(TokenType::IDENT, "anotherVar".to_string()),
///      value: "anotherVar".to_string(),
///    }),
/// }),
/// Statement::LetStatement(LetStatement {
///  token: Token::new(TokenType::LET, "let".to_string()),
/// name: Identifier {
/// token: Token::new(TokenType::IDENT, "anotherVar".to_string()),
/// value: "anotherVar".to_string(),
/// },
/// value: Expression::IntegerLiteral(IntegerLiteral {
///     token: Token::new(TokenType::INT, "5".to_string()),
///    value: 5,
/// }),
/// }),
/// ],
/// };
/// ```
pub struct Program {
    pub statements: Vec<Statement>,
}


impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    // method to print the AST
    pub fn print_program(&self) -> String {
        let mut output = String::new();

        for statement in &self.statements {
            let node_str = match statement {
                Statement::LetStatement(s) => format!("LetStatement {:?} \n", s),
                Statement::ReturnStatement(r) => format!("ReturnStatement {:?} \n", r),
                Statement::ExpressionStatement(e) => format!("ExpressionStatement {:?} \n", e),
            };
            output.push_str(&node_str);
        }
        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}


#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Precedence {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunction(X)
}

// define precedence for each operator
impl Precedence {
    pub fn get_precedence(operator: TokenType) -> Precedence {
        match operator {
            TokenType::EQ => Precedence::EQUALS,
            TokenType::NotEq => Precedence::EQUALS,
            TokenType::LT => Precedence::LESSGREATER,
            TokenType::GT => Precedence::LESSGREATER,
            TokenType::PLUS => Precedence::SUM,
            TokenType::MINUS => Precedence::SUM,
            TokenType::SLASH => Precedence::PRODUCT,
            TokenType::ASTERISK => Precedence::PRODUCT,
            TokenType::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}
