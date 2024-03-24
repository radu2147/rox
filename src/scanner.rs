use crate::types::Token;
use crate::ErrorHandler;
use uuid::Uuid;

pub struct Scanner<'a> {
    pub source: String,
    pub error_handler: &'a mut ErrorHandler,
    tokens: Vec<Token>,
    start: i32,
    current: i32,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens.push(Token::Eof("".to_string(), self.line));
        self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self
                .tokens
                .push(Token::LeftBrace("".to_string(), self.line)),
            ')' => self
                .tokens
                .push(Token::RightBrace("".to_string(), self.line)),
            '{' => self
                .tokens
                .push(Token::LeftParen("".to_string(), self.line)),
            '}' => self
                .tokens
                .push(Token::RightParen("".to_string(), self.line)),
            ',' => self.tokens.push(Token::Comma("".to_string(), self.line)),
            '.' => self.tokens.push(Token::Dot("".to_string(), self.line)),
            '+' => self.tokens.push(Token::Plus("".to_string(), self.line)),
            '-' => self.tokens.push(Token::Minus("".to_string(), self.line)),
            '*' => self.tokens.push(Token::Star("".to_string(), self.line)),
            ';' => self
                .tokens
                .push(Token::Semicolon("".to_string(), self.line)),
            '!' => {
                if self.match_char('=') {
                    self.tokens
                        .push(Token::BangEqual("".to_string(), self.line))
                } else {
                    self.tokens.push(Token::Bang("".to_string(), self.line))
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.tokens
                        .push(Token::EqualEqual("".to_string(), self.line))
                } else {
                    self.tokens.push(Token::Equal("".to_string(), self.line))
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.tokens
                        .push(Token::GraterEqual("".to_string(), self.line))
                } else {
                    self.tokens.push(Token::Grater("".to_string(), self.line))
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.tokens
                        .push(Token::LessEqual("".to_string(), self.line))
                } else {
                    self.tokens.push(Token::Less("".to_string(), self.line))
                }
            }
            '/' => {
                if self.match_char('/') {
                    while let Some(chr) = self.peek() {
                        if chr != '\n' {
                            break;
                        }
                        self.advance();
                    }
                } else {
                    self.tokens.push(Token::Slash("".to_string(), self.line))
                }
            }
            ' ' | '\t' | '\r' => {}
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut number_literal = String::new();
                number_literal.push(c.clone());
                while let Some(chr) = self.peek() {
                    if !Self::is_digit(&chr) {
                        break;
                    }
                    number_literal.push(chr);
                    self.advance();
                }
                if let Some(chr) = self.peek() {
                    if chr == '.' {
                        if let Some(chr_next) = self.peek_next() {
                            if Self::is_digit(&chr_next) {
                                number_literal.push(chr);
                                self.advance();
                                while let Some(chr) = self.peek() {
                                    if !Self::is_digit(&chr) {
                                        break;
                                    }
                                    number_literal.push(chr);
                                    self.advance();
                                }
                            }
                        }
                    }
                }
                self.tokens.push(Token::Number(
                    "".to_string(),
                    number_literal.parse::<f32>().unwrap(),
                    self.line,
                ))
            }
            'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G'
            | 'h' | 'H' | 'i' | 'I' | 'j' | 'J' | 'k' | 'K' | 'l' | 'L' | 'm' | 'M' | 'n' | 'N'
            | 'o' | 'O' | 'p' | 'P' | 'q' | 'Q' | 'r' | 'R' | 's' | 'S' | 't' | 'T' | 'u' | 'U'
            | 'v' | 'V' | 'w' | 'W' | 'x' | 'X' | 'y' | 'Y' | 'z' | 'Z' | '_' => {
                let mut identifier_string = String::new();
                identifier_string.push(c.clone());
                while let Some(chr) = self.peek() {
                    if Self::is_alpha(&chr) || Self::is_digit(&chr) {
                        identifier_string.push(chr.clone());
                        self.advance();
                    } else {
                        break;
                    }
                }
                if let Some(tok) = self.get_keyword_token(&identifier_string, self.line) {
                    self.tokens.push(tok)
                } else {
                    self.tokens.push(Token::Identifier(
                        identifier_string,
                        Uuid::new_v4().to_string(),
                        self.line,
                    ))
                }
            }
            '\n' => self.line += 1,
            '"' => {
                let mut string_literal = String::new();
                while let Some(chr) = self.peek() {
                    if chr == '"' {
                        break;
                    }
                    if chr == '\n' {
                        self.line += 1;
                    }
                    string_literal.push(chr.clone());
                    self.advance();
                }
                if self.is_at_end() {
                    self.error_handler
                        .error(self.line as u32, "Unterminated string");
                    return;
                }
                self.advance();
                self.tokens
                    .push(Token::String("".to_string(), string_literal, self.line));
            }
            _ => self
                .error_handler
                .error(self.line as u32, "Unexpected character."),
        };
    }

    fn get_keyword_token(&self, identifier_string: &str, line: i32) -> Option<Token> {
        match identifier_string {
            "if" => Some(Token::If("".to_string(), line)),
            "else" => Some(Token::Else("".to_string(), line)),
            "print" => Some(Token::Print("".to_string(), line)),
            "and" => Some(Token::And("".to_string(), line)),
            "or" => Some(Token::Or("".to_string(), line)),
            "class" => Some(Token::Class("".to_string(), line)),
            "nil" => Some(Token::Nil("".to_string(), line)),
            "for" => Some(Token::For("".to_string(), line)),
            "while" => Some(Token::While("".to_string(), line)),
            "return" => Some(Token::Return("".to_string(), line)),
            "super" => Some(Token::Super("".to_string(), line)),
            "this" => Some(Token::This("".to_string(), line)),
            "true" => Some(Token::True("".to_string(), line)),
            "false" => Some(Token::False("".to_string(), line)),
            "var" => Some(Token::Var("".to_string(), line)),
            "fun" => Some(Token::Fun("".to_string(), line)),
            "break" => Some(Token::Break(line)),
            _ => None,
        }
    }

    fn is_alpha(chr: &char) -> bool {
        return *chr >= 'a' && *chr <= 'z' || *chr >= 'A' && *chr <= 'Z' || *chr == '_';
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() as i32 {
            return None;
        }
        return Some(
            self.source
                .chars()
                .nth((self.current + 1) as usize)
                .unwrap(),
        );
    }

    fn is_digit(chr: &char) -> bool {
        *chr >= '0' && *chr <= '9'
    }

    fn match_char(&mut self, chr: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current as usize).unwrap() != chr {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }
        return Some(self.source.chars().nth(self.current as usize).unwrap());
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source
            .chars()
            .nth((self.current - 1) as usize)
            .unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len() as i32
    }

    pub fn new(source: String, error_handler: &'a mut ErrorHandler) -> Self {
        Self {
            source,
            error_handler,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }
}
