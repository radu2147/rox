use crate::environment::Environment;
use crate::expression_visitor::Visitor;
use crate::types::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub from: u128,
    pub to: u128,
    pub typ: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    CallExpression(Box<CallExpression>),
    Binary(Box<BinaryExpression>),
    Unary(Box<UnaryExpression>),
    BoolLiteral(bool),
    NumberLiteral(f32),
    StringLiteral(String),
    NilLiteral,
    Identifier(Identifier),
    Group(Box<GroupExpression>),
    Assignment(Box<AssignmentExpression>),
    Member(Box<MemberExpression>),
    ThisExpression(Box<ThisExpression>),
}

impl Expression {
    pub fn get_identifier_name(&self) -> String {
        match &self.typ {
            Expr::Identifier(ident) => ident.name.clone(),
            _ => panic!("Shouldn't be reachable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThisExpression {
    pub keyword: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub name: Token,
    pub object: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub name: Token,
    pub asignee: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub raw_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Expression,
    pub operator: Operator,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: Operator,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpression {
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Div,
    Mul,
    GE,
    LE,
    EQ,
    Greater,
    Less,
    Not,
    NotEqual,
    And,
    Or,
}

impl Operator {
    pub fn from(token: &Token) -> Self {
        match token.typ {
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
            _ => panic!("Unknown operator {:?}", token),
        }
    }
}

impl Expression {
    pub fn accept<T: Visitor<V, E>, V, E>(
        &mut self,
        visitor: &mut T,
        env: &mut Environment,
    ) -> Result<V, E> {
        visitor.visit_expression(self, env)
    }

    pub fn get_name(&self) -> String {
        match &self.typ {
            Expr::Identifier(ref ident) => ident.name.clone(),
            _ => panic!("Not an identifier"),
        }
    }
}
