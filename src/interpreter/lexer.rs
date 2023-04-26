// parse input text into a vector of tokens
// let tokens = lexer::tokenize(&input);
use crate::interpreter::tokens::*;

/// Lexer struct
/// input: the input string
/// position: the current position in the input (points to current char)
/// ch: the current character under examination

pub struct Lexer {
    input: String,
    position: usize,
    ch: char,
}

/// Lexer methods
/// new: create a new Lexer
/// tokenize: start the lexer code on a given input
/// next_token: read the next token in the input
/// read_char: read the next character in the input
/// peek_char: peek at the next character in the input
/// next_token: read the next token in the input
/// skip_whitespace: skip whitespace in the input
/// next_word: read the next word in the input
/// next_number: read the next number in the input
/// advance: advance the lexer one character
/// 
/// tokenize: start the lexer code on a given input
/// 
/// # Examples
/// 
/// ```
/// use monkey_lang::interpreter::lexer::Lexer;
/// let mut lexer = Lexer::new("let five = 5;".to_string());
/// 
/// assert_eq!(lexer.read_fields().0, "let five = 5;");
/// assert_eq!(lexer.read_fields().1, 0);
/// assert_eq!(lexer.read_fields().2, '\0');
/// 
/// ```
/// use monkey_lang::interpreter::lexer::Lexer;
/// 
/// let mut lexer = Lexer::new("let five = 5;".to_string());
/// let tokens = lexer.tokenize(&lexer.input);
/// 
/// assert_eq!(tokens[0].token_type, TokenType::LET);
/// assert_eq!(tokens[0].literal, "let");
/// 
/// assert_eq!(tokens[1].token_type, TokenType::IDENT);
/// assert_eq!(tokens[1].literal, "five");
/// 
/// assert_eq!(tokens[2].token_type, TokenType::ASSIGN);
/// assert_eq!(tokens[2].literal, "=");
/// 
/// assert_eq!(tokens[3].token_type, TokenType::INT);
/// assert_eq!(tokens[3].literal, "5");
/// 
/// assert_eq!(tokens[4].token_type, TokenType::SEMICOLON);
/// assert_eq!(tokens[4].literal, ";");
/// 
/// assert_eq!(tokens[5].token_type, TokenType::EOF);
/// assert_eq!(tokens[5].literal, "");
/// ```
impl Lexer {
    pub fn new (input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0, // current position in input (points to current char)
            ch: '\0' // current character under examination
        };
        return l;
    }

    pub fn read_fields(&self) -> (String, usize, char) {
        return (self.input.clone(), self.position, self.ch);
    }

    // start the lexer code on a given input
    pub fn tokenize(&mut self, input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);

            if tokens.last().unwrap().token_type == TokenType::EOF {
                break;
            }
        }
        return tokens;
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token = match self.read_char() {
            '=' => {
                // check if next char is '='
                if self.peek_char() == '=' {
                    self.advance();
                    self.advance();
                    Token::new(TokenType::EQ, self.input[self.position - 2..self.position].to_string())
                } else {
                    self.advance();
                    Token::new(TokenType::ASSIGN, self.ch.to_string())
                }
            }
            ';' => {
                self.advance();
                Token::new(TokenType::SEMICOLON, self.ch.to_string())
            }
            '(' => {
                self.advance();
                Token::new(TokenType::LPAREN, self.ch.to_string())
            }
            ')' => {
                self.advance();
                Token::new(TokenType::RPAREN, self.ch.to_string())
            }
            ',' => {
                self.advance();
                Token::new(TokenType::COMMA, self.ch.to_string())
            }
            '+' => {
                self.advance();
                Token::new(TokenType::PLUS, self.ch.to_string())
            }
            '-' => {
                self.advance();
                Token::new(TokenType::MINUS, self.ch.to_string())
            }
            '!' => {
                // check if next char is '='
                if self.peek_char() == '=' {
                    self.advance();
                    self.advance();
                    Token::new(TokenType::NotEq, self.input[self.position - 2..self.position].to_string())
                } else {
                    self.advance();
                    Token::new(TokenType::BANG, self.ch.to_string())
                }
            }
            '*' => {
                self.advance();
                Token::new(TokenType::ASTERISK, self.ch.to_string())
            }
            '/' => {
                self.advance();
                Token::new(TokenType::SLASH, self.ch.to_string())
            }
            '<' => {
                self.advance();
                Token::new(TokenType::LT, self.ch.to_string())
            }
            '>' => {
                self.advance();
                Token::new(TokenType::GT, self.ch.to_string())
            }
            '{' => {
                self.advance();
                Token::new(TokenType::LBRACE, self.ch.to_string())
            }
            '}' => {
                self.advance();
                Token::new(TokenType::RBRACE, self.ch.to_string())
            }
            '\0' => {
                self.advance();
                Token::new(TokenType::EOF, "".to_string())
            }
            _ => {
                if self.read_char().is_alphabetic() {
                    let ident = self.next_word();
                    let token_type = match ident.as_str() {
                        "let" => TokenType::LET,
                        "fn" => TokenType::FUNCTION,
                        "true" => TokenType::TRUE,
                        "false" => TokenType::FALSE,
                        "if" => TokenType::IF,
                        "else" => TokenType::ELSE,
                        "return" => TokenType::RETURN,
                        _ => TokenType::IDENT,
                    };
                    Token::new(token_type, ident)
                } else if self.read_char().is_numeric() {
                    let number = self.next_number();
                    Token::new(TokenType::INT, number)
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch.to_string())
                }
            }
        };
        return Some(token);
    }

    // read the next word in the input
    fn next_word(&mut self) -> String {
        let mut word = String::new();
        while self.read_char().is_alphanumeric() {
            word.push(self.read_char());
            self.advance();
        }
        return word;
    }

    // read the next number in the input
    fn next_number(&mut self) -> String {
        let mut number = String::new();
        while self.read_char().is_numeric() {
            number.push(self.read_char());
            self.advance();
        }
        return number;
    }
    // clear whitespce
    fn skip_whitespace(&mut self) {
        while self.read_char().is_whitespace() {
            self.advance();
        }
    }

    // read the next character in the input
    fn read_char(&mut self) -> char {
        if self.position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.position).unwrap();
        }
        self.ch
    }

    // peek at the next character in the input
    fn peek_char(&mut self) -> char {
        if self.position + 1 >= self.input.len() {
            return '\0';
        } else {
            return self.input.chars().nth(self.position + 1).unwrap();
        }
    }

    // advance the lexer one character
    fn advance(&mut self) {
        self.position += 1;
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol() {
        let input = "=";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_identifier() {
        let input = "hello";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::IDENT, "hello".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_number() {
        let input = "4 567";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::INT, "4".to_string()),
            Token::new(TokenType::INT, "567".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_next_token() {
        let input = "+(){},4567hhyy;";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::INT, "4567".to_string()),
            Token::new(TokenType::IDENT, "hhyy".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }
    
    #[test]
    fn test_lex_simple_token() {
        let input = "let five = 5; true false sdf if dfa else return";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::IDENT, "sdf".to_string()),
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::IDENT, "dfa".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_multi_char_token() {
        let input = "if 6 == 6 then true else if 5 == 5 then false else true";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::INT, "6".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "6".to_string()),
            Token::new(TokenType::IDENT, "then".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::IDENT, "then".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }
    
    #[test]
    fn test_lex_multiple_tokens() {
        let input = "let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten);!-/*5;5<5>5;";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let tokens = lexer.tokenize(input);
        assert_eq!(tokens, expected_tokens);
    }
}
