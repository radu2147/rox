use crate::ast_types::Expression;
use crate::environment::Environment;
use crate::statement_visitor::StatementVisitor;
use crate::types::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    BreakStatement,
    ReturnStatement(ReturnStatement),
    PrintStatement(PrintStatement),
    ExpressionStatement(ExpressionStatement),
    VarDeclarationStatement(VariableDeclarationStatement),
    BlockStatement(BlockStatement),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub name: String,
    pub raw_token: Token,
    pub methods: Vec<Stmt>,
    pub fields: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub params: Vec<Expression>,
    pub body: Box<Stmt>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationStatement {
    pub initializer: Expression,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_expression: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub expression: Expression,
    pub statement: Box<Stmt>,
    pub else_statement: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub expression: Expression,
    pub statement: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Stmt>,
}

impl Stmt {
    pub fn accept<T: StatementVisitor<V, E>, V, E>(
        &mut self,
        visitor: &mut T,
        env: &mut Environment,
    ) -> Result<V, E> {
        visitor.visit_statement(self, env)
    }
}
