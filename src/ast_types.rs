use crate::environment::Environment;
use crate::expression_visitor::Visitor;
use crate::types::Token;

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
    pub fn get_identifier_name(&self) -> &str {
        match &self.typ {
            Expr::Identifier(ident) => &ident.name,
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
