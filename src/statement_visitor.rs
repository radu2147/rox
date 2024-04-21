use crate::environment::Environment;
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Statement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::Location;

pub trait StatementVisitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_statement(&mut self, expr: &mut Statement, env: &mut Environment) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match &mut expr.typ {
            Stmt::ExpressionStatement(expr) => self.visit_statement_expression(expr, env),
            Stmt::PrintStatement(expr) => self.visit_print_statement(expr, env),
            Stmt::VarDeclarationStatement(decl) => self.visit_declaration_statement(decl, env),
            Stmt::BlockStatement(blck) => self.visit_block_statement(blck, env),
            Stmt::IfStatement(if_stmt) => self.visit_if_statement(if_stmt, env),
            Stmt::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt, env),
            Stmt::FunctionDeclaration(func_dec) => self.visit_function_declaration(func_dec, env),
            Stmt::ReturnStatement(return_stmt) => self.visit_return_statement(return_stmt, env),
            Stmt::ClassDeclaration(class_decl) => self.visit_class_declaration(class_decl, env),
            Stmt::BreakStatement => self.visit_break_statement(env),
        }
    }

    fn visit_break_statement(&mut self, env: &mut Environment) -> Result<V, E>;

    fn visit_class_declaration(
        &mut self,
        class_declaration: &mut ClassDeclaration,
        env: &mut Environment,
    ) -> Result<V, E>;

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
    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_while_statement(
        &mut self,
        while_statement: &mut WhileStatement,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_declaration_statement(
        &mut self,
        decl: &mut VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_block_statement(
        &mut self,
        decl: &mut BlockStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_statement_expression(
        &mut self,
        expr: &mut ExpressionStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_print_statement(
        &mut self,
        expr: &mut PrintStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
}

pub trait OwnedStatementVisitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_statement(&mut self, expr: Statement, env: &mut Environment) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match expr.typ {
            Stmt::ExpressionStatement(expr) => self.visit_statement_expression(expr, env),
            Stmt::PrintStatement(expr) => self.visit_print_statement(expr, env),
            Stmt::VarDeclarationStatement(decl) => self.visit_declaration_statement(decl, env),
            Stmt::BlockStatement(blck) => self.visit_block_statement(blck, env),
            Stmt::IfStatement(if_stmt) => self.visit_if_statement(if_stmt, env),
            Stmt::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt, env),
            Stmt::FunctionDeclaration(func_dec) => self.visit_function_declaration(func_dec, env),
            Stmt::ReturnStatement(return_stmt) => self.visit_return_statement(return_stmt, env),
            Stmt::ClassDeclaration(class_decl) => self.visit_class_declaration(class_decl, env),
            Stmt::BreakStatement => self.visit_break_statement(env),
        }
    }

    fn visit_break_statement(&mut self, env: &mut Environment) -> Result<V, E>;

    fn visit_class_declaration(
        &mut self,
        class_declaration: ClassDeclaration,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_if_statement(
        &mut self,
        if_statement: IfStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_function_declaration(
        &mut self,
        func_dec: FunctionDeclaration,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_return_statement(
        &mut self,
        return_stmt: ReturnStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_while_statement(
        &mut self,
        while_statement: WhileStatement,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_declaration_statement(
        &mut self,
        decl: VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_block_statement(
        &mut self,
        decl: BlockStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_statement_expression(
        &mut self,
        expr: ExpressionStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_print_statement(
        &mut self,
        expr: PrintStatement,
        env: &mut Environment,
    ) -> Result<V, E>;
}
