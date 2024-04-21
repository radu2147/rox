use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Statement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::Location;

pub trait StatementVisitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_statement(&mut self, expr: &mut Statement) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match &mut expr.typ {
            Stmt::ExpressionStatement(expr) => self.visit_statement_expression(expr),
            Stmt::PrintStatement(expr) => self.visit_print_statement(expr),
            Stmt::VarDeclarationStatement(decl) => self.visit_declaration_statement(decl),
            Stmt::BlockStatement(blck) => self.visit_block_statement(blck),
            Stmt::IfStatement(if_stmt) => self.visit_if_statement(if_stmt),
            Stmt::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt),
            Stmt::FunctionDeclaration(func_dec) => self.visit_function_declaration(func_dec),
            Stmt::ReturnStatement(return_stmt) => self.visit_return_statement(return_stmt),
            Stmt::ClassDeclaration(class_decl) => self.visit_class_declaration(class_decl),
            Stmt::BreakStatement => self.visit_break_statement(),
        }
    }

    fn visit_break_statement(&mut self) -> Result<V, E>;

    fn visit_class_declaration(&mut self, class_declaration: &mut ClassDeclaration)
        -> Result<V, E>;

    fn visit_if_statement(&mut self, if_statement: &mut IfStatement) -> Result<V, E>;
    fn visit_function_declaration(&mut self, func_dec: &mut FunctionDeclaration) -> Result<V, E>;
    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> Result<V, E>;
    fn visit_while_statement(&mut self, while_statement: &mut WhileStatement) -> Result<V, E>;

    fn visit_declaration_statement(
        &mut self,
        decl: &mut VariableDeclarationStatement,
    ) -> Result<V, E>;
    fn visit_block_statement(&mut self, decl: &mut BlockStatement) -> Result<V, E>;
    fn visit_statement_expression(&mut self, expr: &mut ExpressionStatement) -> Result<V, E>;
    fn visit_print_statement(&mut self, expr: &mut PrintStatement) -> Result<V, E>;
}

pub trait OwnedStatementVisitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_statement(&mut self, expr: Statement) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match expr.typ {
            Stmt::ExpressionStatement(expr) => self.visit_statement_expression(expr),
            Stmt::PrintStatement(expr) => self.visit_print_statement(expr),
            Stmt::VarDeclarationStatement(decl) => self.visit_declaration_statement(decl),
            Stmt::BlockStatement(blck) => self.visit_block_statement(blck),
            Stmt::IfStatement(if_stmt) => self.visit_if_statement(if_stmt),
            Stmt::WhileStatement(while_stmt) => self.visit_while_statement(while_stmt),
            Stmt::FunctionDeclaration(func_dec) => self.visit_function_declaration(func_dec),
            Stmt::ReturnStatement(return_stmt) => self.visit_return_statement(return_stmt),
            Stmt::ClassDeclaration(class_decl) => self.visit_class_declaration(class_decl),
            Stmt::BreakStatement => self.visit_break_statement(),
        }
    }

    fn visit_break_statement(&mut self) -> Result<V, E>;

    fn visit_class_declaration(&mut self, class_declaration: ClassDeclaration) -> Result<V, E>;

    fn visit_if_statement(&mut self, if_statement: IfStatement) -> Result<V, E>;
    fn visit_function_declaration(&mut self, func_dec: FunctionDeclaration) -> Result<V, E>;
    fn visit_return_statement(&mut self, return_stmt: ReturnStatement) -> Result<V, E>;
    fn visit_while_statement(&mut self, while_statement: WhileStatement) -> Result<V, E>;

    fn visit_declaration_statement(&mut self, decl: VariableDeclarationStatement) -> Result<V, E>;
    fn visit_block_statement(&mut self, decl: BlockStatement) -> Result<V, E>;
    fn visit_statement_expression(&mut self, expr: ExpressionStatement) -> Result<V, E>;
    fn visit_print_statement(&mut self, expr: PrintStatement) -> Result<V, E>;
}
