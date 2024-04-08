use crate::ast_types::Operator;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub line: u128,
    pub typ: TokenType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Grater,
    GraterEqual,
    Less,
    LessEqual,
    Identifier(String, String),
    String(String),
    Number(f32),
    And,
    Or,
    Class,
    If,
    Else,
    Fun,
    For,
    Nil,
    Print,
    Return,
    This(String),
    True,
    False,
    Var,
    While,
    Break,
    Eof,
}

impl Into<Operator> for Token {
    fn into(self) -> Operator {
        match self.typ {
            TokenType::Plus => Operator::Plus,
            TokenType::Minus => Operator::Minus,
            TokenType::Star => Operator::Mul,
            TokenType::Slash => Operator::Div,
            TokenType::GraterEqual => Operator::GE,
            TokenType::LessEqual => Operator::LE,
            TokenType::Less => Operator::Less,
            TokenType::Grater => Operator::Greater,
            TokenType::EqualEqual => Operator::EQ,
            TokenType::BangEqual => Operator::NotEqual,
            TokenType::Bang => Operator::Not,
            TokenType::And => Operator::And,
            TokenType::Or => Operator::Or,
            _ => panic!("Unknown operator {:?}", self),
        }
    }
}

impl Token {
    pub fn get_variable(&self) -> Variable {
        match &self.typ {
            TokenType::Identifier(name, id) => Variable {
                name: name.clone(),
                line: self.line,
                id: id.clone(),
            },
            TokenType::This(name) => Variable {
                name: name.clone(),
                line: self.line,
                id: "1".to_string(),
            },
            _ => panic!("Not a variable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub line: u128,
    pub id: String,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub(crate) from: u128,
    pub(crate) to: u128,
}
