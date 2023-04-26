use std::collections::HashMap;
use crate::interpreter::{ast, lexer, tokens};

pub struct Parser {
    lex: lexer::Lexer,
    cur_token: tokens::Token,
    peek_token: tokens::Token,
    errors: Vec<String>,

    prefixParseFunctions: HashMap<tokens::TokenType, fn(&Parser) -> ast::Expression>,
    infixParseFunctions: HashMap<tokens::TokenType, fn(&Parser) -> ast::Expression>,
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lex,
            cur_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            peek_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            errors: Vec::new(),
            prefixParseFunctions: HashMap::new(),
            infixParseFunctions: HashMap::new(),
        };

        parser.initialize_prefix_functions();

        parser.next_token();
        parser.next_token();

        parser
    }

    fn initialize_prefix_functions(&mut self) {
        self.register_prefix_function(tokens::TokenType::IDENT, Parser::parse_identifier);
        self.register_prefix_function(tokens::TokenType::INT, Parser::parse_integer);
    }

    fn register_prefix_function(&mut self, t: tokens::TokenType, f: fn(&Parser) -> ast::Expression) {
        self.prefixParseFunctions.insert(t, f);
    }

    fn register_infix_function(&mut self, t: tokens::TokenType, f: fn(&Parser) -> ast::Expression) {
        self.infixParseFunctions.insert(t, f);
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        let mut program = ast::Program::new();

        while self.cur_token.token_type != tokens::TokenType::EOF {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(e) => {
                    let error = format!("Error parsing statement: {}", e);
                    self.errors.push(error.clone());
                    return Err(error);
                }
            }
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.cur_token.token_type {
            tokens::TokenType::LET => Ok(
                ast::Statement::LetStatement(
                    match self.parse_let_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing let statement: {}", e)
                            );
                        }
                    }
                )),
            tokens::TokenType::RETURN => Ok(
                ast::Statement::ReturnStatement(
                    match self.parse_return_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing return statement: {}", e)
                            );
                        }
                    }
                )),
            _ => Ok(
                ast::Statement::ExpressionStatement(
                    match self.parse_expression_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing expression statement: {}", e)
                            );
                        }
                    }
                )),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement, String> {
        let stmt = ast::ExpressionStatement {
            token: self.cur_token.clone(),
            expression: match self.parse_expression(ast::Precedence::LOWEST) {
                Ok(exp) => exp,
                Err(e) => {
                    return Err(
                        format!("Error parsing expression: {}", e)
                    );
                }
            }
        };

        if self.peek_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression(&self, precedence: ast::Precedence) -> Result<ast::Expression, String> {
        let prefix = match self.prefixParseFunctions.get(&self.cur_token.token_type) {
            Some(f) => f,
            None => {
                let error = format!("No prefix parse function for {:?} found", self.cur_token.token_type);
                return Err(error);
            }
        };

        let mut left_exp = prefix(self);
        return Ok(left_exp);
    }

    fn parse_identifier(&self) -> ast::Expression {
        ast::Expression::Identifier(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }

    fn parse_integer(&self) -> ast::Expression {
        ast::Expression::IntegerLiteral(ast::IntegerLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.parse::<i64>().unwrap(),
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, String> {
        let mut statement = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            }),
        };

        self.next_token();

        // TODO :: parseExpression , followed by assert semicolon

        while !self.cur_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(statement)
    }
    
    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, String> {
        let mut stmt = ast::LetStatement {
            token: self.cur_token.clone(),
            name: ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            },
            value: ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            }),
        };

        match self.assert_peek(tokens::TokenType::IDENT) {
            Ok(_) => {},
            Err(e) => {
                return Err(
                    format!("{}", e)
                );
            }
        }

        stmt.name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        match self.assert_peek(tokens::TokenType::ASSIGN) {
            Ok(_) => {},
            Err(e) => {
                return Err(
                    format!("{}", e)
                );
            }
        }

        // TODO :: parseExpression , followed by assert semicolon

        while !self.cur_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = match self.lex.next_token() {
            Some(token) => token,
            None => tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
        };
    }
    
    fn assert_peek(&mut self, t: tokens::TokenType) -> Result<(), String> {
        if self.peek_token_is(t) {
            self.next_token();
            return Ok(());
        } else {
            let error = format!("Expected next token to be {:?}, got {:?} instead", t, self.peek_token.token_type);
            self.errors.push(error.clone());
            return Err(error);
        }
    }

    fn cur_token_is(&self, t: tokens::TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: tokens::TokenType) -> bool {
        self.peek_token.token_type == t
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = match program.statements[0].clone() {
            ast::Statement::ExpressionStatement(stmt) => stmt,
            _ => panic!("Expected ExpressionStatement, got {:?}", program.statements[0]),
        };

        let literal = match stmt.expression {
            ast::Expression::IntegerLiteral(literal) => literal,
            _ => panic!("Expected IntegerLiteral, got {:?}", stmt.expression),
        };

        assert_eq!(literal.value, 5);
        assert_eq!(literal.token.literal, "5");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = match program.statements[0].clone() {
            ast::Statement::ExpressionStatement(stmt) => stmt,
            _ => panic!("Expected ExpressionStatement, got {:?}", program.statements[0]),
        };

        let ident = match stmt.expression {
            ast::Expression::Identifier(ident) => ident,
            _ => panic!("Expected Identifier, got {:?}", stmt.expression),
        };

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token.literal, "foobar");
    }

    #[test]
    #[should_panic(expected = "Expected next token to be IDENT, got ASSIGN instead")]
    fn test_let_statement_missing_ident_negative() {
        let input = "let = 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }

    #[test]
    #[should_panic(expected = "Expected next token to be ASSIGN")]
    fn test_let_statement_missing_assign_negative() {
        let input = "let var 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }

    #[test]
    fn test_let_statement() {
        let input = "let five = 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };

        assert_eq!(program.statements.len(), 1);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::LetStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::LET);
                    assert_eq!(stmt.name.value, "five");
                    assert_eq!(stmt.name.token.token_type, tokens::TokenType::IDENT);

                    // confirm that the value is an illegal token
                    assert_eq!(stmt.value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Let Statement"),
            }

        }

    }
    
    
    #[test]
    fn test_multiple_let_statements() {
        let input = "let five = 5; let ten = 10; let result = 45;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };
        // confirm that the program has 3 statements
        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::LetStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::LET);
                    assert_eq!(stmt.name.token.token_type, tokens::TokenType::IDENT);

                    // confirm that the value is Illegal Token
                    assert_eq!(stmt.value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
    
                        })
                    );
                }
                _ => panic!("Expected Let Statement"),
            }

        }

    }


    #[test]
    fn test_return_statement() {
        let input = "return 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };

        // confirm that the program has 1 statement
        assert_eq!(program.statements.len(), 1);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::ReturnStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::RETURN);
                    assert_eq!(stmt.return_value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Return Statement"),
            }

        }
    }

    
    #[test]
    // #[should_panic(expected = "Expected next token to be EXPRESSION, got SEMICOLON instead")]
    fn test_return_statement_negative() {
        let input = "return ;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }


    #[test]
    fn test_multiple_return_statements() {
        let input = "return 5; return 10; return add(5,10);";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };
        // confirm that the program has 3 statements
        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::ReturnStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::RETURN);
                    assert_eq!(stmt.return_value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Return Statement"),
            }

        }

    }
   
}
