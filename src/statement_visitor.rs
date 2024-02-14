use crate::environment::Environment;
use crate::stmt::{
    BlockStatement, ExpressionStatement, FunctionDeclaration, IfStatement, PrintStatement, Stmt,
    VariableDeclarationStatement, WhileStatement,
};

pub trait StatementVisitor<V, E> {
    fn visit_statement(&mut self, expr: &mut Stmt, env: &mut Environment) -> Result<V, E> {
        match expr {
            Stmt::ExpressionStatement(expr) => self.visit_statement_expression(expr, env),
            Stmt::PrintStatement(expr) => self.visit_print_statement(expr, env),
            Stmt::VarDeclarationStatement(decl) => self.visit_declaration_statement(decl, env),
            Stmt::BlockStatement(blck) => self.visit_block_statement(blck, env),
            Stmt::IfStatement(if_stmt) => self.visit_if_statement(if_stmt, env),
            Stmt::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt, env),
            Stmt::FunctionDeclaration(func_dec) => self.visit_function_declaration(func_dec, env),
        }
    }

    fn visit_if_statement(
        &mut self,
        if_statement: &mut IfStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_function_declaration(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_while_statement(
        &mut self,
        while_statement: &mut WhileStatement,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_declaration_statement(
        &mut self,
        decl: &VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_block_statement(
        &mut self,
        decl: &mut BlockStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_statement_expression(
        &mut self,
        expr: &ExpressionStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_print_statement(
        &mut self,
        expr: &PrintStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
}
