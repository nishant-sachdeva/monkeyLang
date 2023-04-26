use std::error::Error;

use crate::interpreter::{ast, lexer, tokens};

pub struct Parser {
    lex: lexer::Lexer,
    cur_token: tokens::Token,
    peek_token: tokens::Token,
    errors: Vec<String>
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lex,
            cur_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            peek_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    // function to return errors
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
            _ => {
                return Err(
                    format!("Expected LET, got {:?}", self.cur_token.token_type)
                );
            },
        }
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
                    format!("Error parsing let statement: {}", e)
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
                    format!("Error parsing let statement: {}", e)
                );
            }
        }

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
        if !self.expect_peek(t) {
            return Err(self.peek_error(t));
        }
        Ok(())
    }

    fn expect_peek(&mut self, t: tokens::TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn peek_error(&mut self, t: tokens::TokenType) -> String {
        let error = format!("Expected next token to be {:?}, got {:?} instead", t, self.peek_token.token_type);
        self.errors.push(error.clone());
        error
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
    use crate::interpreter::ast::LetStatement;

    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "let five = 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };
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

   
}
