use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expr, Expression, GroupExpression,
    Identifier, MemberExpression, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;
use crate::types::Location;

pub trait Visitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_expression(&mut self, expr: &mut Expression, env: &mut Environment) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match &mut expr.typ {
            Expr::Binary(expr) => self.visit_binary_expression(expr, env),
            Expr::Unary(expr) => self.visit_unary_expression(expr, env),
            Expr::Group(expr) => self.visit_group_expression(expr, env),
            Expr::NumberLiteral(expr) => Ok(self.visit_number_literal(expr)),
            Expr::StringLiteral(expr) => Ok(self.visit_string_literal(expr)),
            Expr::BoolLiteral(expr) => Ok(self.visit_bool_literal(expr)),
            Expr::NilLiteral => Ok(self.visit_nil_literal()),
            Expr::Identifier(expr) => self.visit_identifier_expression(expr, env),
            Expr::Assignment(assign) => self.visit_assignement_expression(assign, env),
            Expr::CallExpression(call) => self.visit_call_expression(call, env),
            Expr::Member(member) => self.visit_member_expression(member, env),
            Expr::ThisExpression(this_expr) => self.visit_this_expression(this_expr, env),
        }
    }

    fn visit_this_expression(
        &mut self,
        expr: &mut ThisExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_call_expression(
        &mut self,
        expr: &mut CallExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_member_expression(
        &mut self,
        expr: &mut MemberExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_assignement_expression(
        &mut self,
        expr: &mut AssignmentExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_binary_expression(
        &mut self,
        expr: &mut BinaryExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_identifier_expression(&mut self, expr: &Identifier, env: &Environment)
        -> Result<V, E>;
    fn visit_unary_expression(
        &mut self,
        expr: &mut UnaryExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_group_expression(
        &mut self,
        expr: &mut GroupExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_nil_literal(&self) -> V;
    fn visit_string_literal(&self, expr: &String) -> V;
    fn visit_number_literal(&self, expr: &f32) -> V;
    fn visit_bool_literal(&self, expr: &bool) -> V;
}

pub trait OwnedVisitor<V, E> {
    fn set_current_location(&mut self, location: Location);
    fn visit_expression(&mut self, expr: Expression, env: &mut Environment) -> Result<V, E> {
        let location = Location {
            from: expr.from,
            to: expr.to,
        };
        self.set_current_location(location);
        match expr.typ {
            Expr::Binary(expr) => self.visit_binary_expression(*expr, env),
            Expr::Unary(expr) => self.visit_unary_expression(*expr, env),
            Expr::Group(expr) => self.visit_group_expression(*expr, env),
            Expr::NumberLiteral(expr) => Ok(self.visit_number_literal(expr)),
            Expr::StringLiteral(expr) => Ok(self.visit_string_literal(expr)),
            Expr::BoolLiteral(expr) => Ok(self.visit_bool_literal(expr)),
            Expr::NilLiteral => Ok(self.visit_nil_literal()),
            Expr::Identifier(expr) => self.visit_identifier_expression(expr, env),
            Expr::Assignment(assign) => self.visit_assignement_expression(*assign, env),
            Expr::CallExpression(call) => self.visit_call_expression(*call, env),
            Expr::Member(member) => self.visit_member_expression(*member, env),
            Expr::ThisExpression(this_expr) => self.visit_this_expression(*this_expr, env),
        }
    }

    fn visit_this_expression(
        &mut self,
        expr: ThisExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_call_expression(
        &mut self,
        expr: CallExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_member_expression(
        &mut self,
        expr: MemberExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_assignement_expression(
        &mut self,
        expr: AssignmentExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_binary_expression(
        &mut self,
        expr: BinaryExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_identifier_expression(&mut self, expr: Identifier, env: &Environment) -> Result<V, E>;
    fn visit_unary_expression(
        &mut self,
        expr: UnaryExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_group_expression(
        &mut self,
        expr: GroupExpression,
        env: &mut Environment,
    ) -> Result<V, E>;
    fn visit_nil_literal(&self) -> V;
    fn visit_string_literal(&self, expr: String) -> V;
    fn visit_number_literal(&self, expr: f32) -> V;
    fn visit_bool_literal(&self, expr: bool) -> V;
}
