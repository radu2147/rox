use crate::errors::ParseError;
use crate::types::{Token, TokenType};
use uuid::Uuid;

pub struct Scanner {
    pub source: String,
    tokens: Vec<Token>,
    start: i32,
    current: i32,
    line: u128,
}

impl Scanner {
    pub fn scan_tokens(mut self) -> Result<Vec<Token>, ParseError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token {
            typ: TokenType::Eof,
            line: self.line,
        });
        Ok(self.tokens)
    }

    pub fn match_equal(
        &mut self,
        condition_met_token_type: TokenType,
        alternative_token_type: TokenType,
    ) {
        if self.match_char('=') {
            self.tokens.push(Token {
                typ: condition_met_token_type,
                line: self.line,
            })
        } else {
            self.tokens.push(Token {
                typ: alternative_token_type,
                line: self.line,
            })
        }
    }

    fn scan_token(&mut self) -> Result<(), ParseError> {
        let c = self.advance();
        match c {
            '(' => self.tokens.push(Token {
                typ: TokenType::LeftBrace,
                line: self.line,
            }),
            ')' => self.tokens.push(Token {
                typ: TokenType::RightBrace,
                line: self.line,
            }),
            '{' => self.tokens.push(Token {
                typ: TokenType::LeftParen,
                line: self.line,
            }),
            '}' => self.tokens.push(Token {
                typ: TokenType::RightParen,
                line: self.line,
            }),
            ',' => self.tokens.push(Token {
                typ: TokenType::Comma,
                line: self.line,
            }),
            '.' => self.tokens.push(Token {
                typ: TokenType::Dot,
                line: self.line,
            }),
            '+' => self.tokens.push(Token {
                typ: TokenType::Plus,
                line: self.line,
            }),
            '-' => self.tokens.push(Token {
                typ: TokenType::Minus,
                line: self.line,
            }),
            '*' => self.tokens.push(Token {
                typ: TokenType::Star,
                line: self.line,
            }),
            ';' => self.tokens.push(Token {
                typ: TokenType::Semicolon,
                line: self.line,
            }),
            '!' => {
                self.match_equal(TokenType::BangEqual, TokenType::Bang);
            }
            '=' => {
                self.match_equal(TokenType::EqualEqual, TokenType::Equal);
            }
            '>' => {
                self.match_equal(TokenType::GraterEqual, TokenType::Grater);
            }
            '<' => {
                self.match_equal(TokenType::LessEqual, TokenType::Less);
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
                    self.tokens.push(Token {
                        typ: TokenType::Slash,
                        line: self.line,
                    })
                }
            }
            ' ' | '\t' | '\r' => {}
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut number_literal = String::new();
                number_literal.push(c);
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
                self.tokens.push(Token {
                    typ: TokenType::Number(number_literal.parse::<f32>().unwrap()),
                    line: self.line,
                })
            }
            'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G'
            | 'h' | 'H' | 'i' | 'I' | 'j' | 'J' | 'k' | 'K' | 'l' | 'L' | 'm' | 'M' | 'n' | 'N'
            | 'o' | 'O' | 'p' | 'P' | 'q' | 'Q' | 'r' | 'R' | 's' | 'S' | 't' | 'T' | 'u' | 'U'
            | 'v' | 'V' | 'w' | 'W' | 'x' | 'X' | 'y' | 'Y' | 'z' | 'Z' | '_' => {
                let mut identifier_string = String::new();
                identifier_string.push(c);
                while let Some(chr) = self.peek() {
                    if Self::is_alpha(&chr) || Self::is_digit(&chr) {
                        identifier_string.push(chr);
                        self.advance();
                    } else {
                        break;
                    }
                }
                if let Some(tok) = self.get_keyword_token(&identifier_string, self.line) {
                    self.tokens.push(tok)
                } else {
                    self.tokens.push(Token {
                        typ: TokenType::Identifier(identifier_string, Uuid::new_v4().to_string()),
                        line: self.line,
                    })
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
                    string_literal.push(chr);
                    self.advance();
                }
                if self.is_at_end() {
                    return Err(ParseError {
                        message: "Unterminated string.".to_string(),
                        line: self.line,
                    });
                }
                self.advance();
                self.tokens.push(Token {
                    typ: TokenType::String(string_literal),
                    line: self.line,
                });
            }
            _ => {
                return Err(ParseError {
                    message: "Unexpected character.".to_string(),
                    line: self.line,
                })
            }
        };
        Ok(())
    }

    fn get_keyword_token(&self, identifier_string: &str, line: u128) -> Option<Token> {
        match identifier_string {
            "if" => Some(Token {
                typ: TokenType::If,
                line,
            }),
            "else" => Some(Token {
                typ: TokenType::Else,
                line,
            }),
            "print" => Some(Token {
                typ: TokenType::Print,
                line,
            }),
            "and" => Some(Token {
                typ: TokenType::And,
                line,
            }),
            "or" => Some(Token {
                typ: TokenType::Or,
                line,
            }),
            "class" => Some(Token {
                typ: TokenType::Class,
                line,
            }),
            "nil" => Some(Token {
                typ: TokenType::Nil,
                line,
            }),
            "for" => Some(Token {
                typ: TokenType::For,
                line,
            }),
            "while" => Some(Token {
                typ: TokenType::While,
                line,
            }),
            "return" => Some(Token {
                typ: TokenType::Return,
                line,
            }),
            "this" => Some(Token {
                typ: TokenType::This(Uuid::new_v4().to_string()),
                line,
            }),
            "true" => Some(Token {
                typ: TokenType::True,
                line,
            }),
            "false" => Some(Token {
                typ: TokenType::False,
                line,
            }),
            "var" => Some(Token {
                typ: TokenType::Var,
                line,
            }),
            "fun" => Some(Token {
                typ: TokenType::Fun,
                line,
            }),
            "break" => Some(Token {
                typ: TokenType::Break,
                line,
            }),
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

    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }
}
