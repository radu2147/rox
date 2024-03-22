use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, MemberExpression, SetMemberExpression, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;

pub trait Visitor<V, E> {
    fn visit_expression(&mut self, expr: &mut Expression, env: &mut Environment) -> Result<V, E> {
        match expr {
            Expression::Binary(expr) => self.visit_binary_expression(expr, env),
            Expression::Unary(expr) => self.visit_unary_expression(expr, env),
            Expression::Group(expr) => self.visit_group_expression(expr, env),
            Expression::NumberLiteral(expr) => Ok(self.visit_number_literal(expr)),
            Expression::StringLiteral(expr) => Ok(self.visit_string_literal(expr)),
            Expression::BoolLiteral(expr) => Ok(self.visit_bool_literal(expr)),
            Expression::NilLiteral => Ok(self.visit_nil_literal()),
            Expression::Identifier(expr) => self.visit_identifier_expression(expr, env),
            Expression::Assignment(assign) => self.visit_assignement_expression(assign, env),
            Expression::CallExpression(call) => self.visit_call_expression(call, env),
            Expression::Member(member) => self.visit_member_expression(member, env),
            Expression::SetMember(member) => self.visit_set_member_expression(member, env),
            Expression::ThisExpression(this_expr) => self.visit_this_expression(this_expr, env),
        }
    }

    fn visit_this_expression(
        &mut self,
        expr: &mut ThisExpression,
        env: &mut Environment,
    ) -> Result<V, E>;

    fn visit_set_member_expression(
        &mut self,
        expr: &mut SetMemberExpression,
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
