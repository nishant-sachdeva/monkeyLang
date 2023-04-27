use std::default;

use crate::interpreter::tokens::*;

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

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone)]
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
            TokenType::NOT_EQ => Precedence::EQUALS,
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
