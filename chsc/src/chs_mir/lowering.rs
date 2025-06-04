use crate::{
    chs_ast::{
        ast::{Expression, ReturnStatement, Statement},
        hir::{HIRModule, HIRModuleItem},
    },
    chs_mir::{MIRBuilder, MIRFunction, MIRModule, Operand, Terminator}, chs_types::CHSType,
};

use super::{Constant, MIROperation, RValue};

impl MIRModule {
    pub fn from_hir(m: HIRModule) -> Self {
        let mut module = Self::new();
        let mut b = MIRBuilder::new();

        for item in m.items {
            match item {
                HIRModuleItem::Function(hir_f) => {
                    let f = MIRFunction::new(hir_f.name, hir_f.return_type);
                    let idx = module.add_function(f);
                    b.set_current_function(idx);
                    for param in hir_f.params {
                        b.add_name_local(&mut module, param.param_type, param.name);
                    }
                    let block = b.add_block(&mut module).unwrap();
                    b.set_terminator(&mut module, Terminator::Goto(block));
                    b.set_current_block(block);
                    lower_stmt(&mut module, &mut b, hir_f.body);
                }
                HIRModuleItem::ExternFunction(hirextern_function) => todo!(),
            }
        }
        module
    }
}

fn lower_block(module: &mut MIRModule, b: &mut MIRBuilder, stmts: Vec<Statement>) {
    for stmt in stmts {
        lower_stmt(module, b, stmt);
    }
}

fn lower_stmt(module: &mut MIRModule, b: &mut MIRBuilder, stmt: Statement) {
    match stmt {
        Statement::ReturnStatement(r) => match r {
            ReturnStatement::Return => {
                b.set_terminator(module, Terminator::Return(None));
            }
            ReturnStatement::ReturnExpression(expression) => {
                let operand = b.lower_expression(module, expression);
                b.set_terminator(module, Terminator::Return(Some(operand)));
            }
        },
        Statement::BlockStatement(statements) => {
            let block = b.add_block(module).unwrap();
            b.set_terminator(module, Terminator::Goto(block));
            b.set_current_block(block);
            lower_block(module, b, statements);
        }
        Statement::LetStatement(l) => {

        }
        Statement::ExpressionStatement(expression) => todo!(),
        Statement::IfStatement(if_statement) => todo!(),
        Statement::WhileStatement(while_statement) => todo!(),
        Statement::ForStatement(for_statement) => todo!(),
        Statement::AssignmentStatement(assignment_statement) => todo!(),
        Statement::Empty => todo!(),
    }
}

impl MIRBuilder {
    fn lower_expression(&mut self, m: &mut MIRModule, expression: Expression) -> Operand {
        match expression {
            Expression::Nil => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::Int(token) => {
                Operand::Constant(Constant::Int(token.source.parse().unwrap()))
            }
            Expression::Float(token) => todo!(),
            Expression::String(token) => todo!(),
            Expression::Char(token) => todo!(),
            Expression::Identifier(token) => todo!(),
            Expression::BinaryOp {
                left,
                operator,
                right,
            } => {
                let left  = self.lower_expression(m, *left);
                let right = self.lower_expression(m, *right);
                let target = self.add_local(m, CHSType::I32, None).unwrap();
                self.add_operation(
                    m,
                    MIROperation::Assign {
                        target,
                        value: RValue::BinaryOp { op: operator, left, right },
                    },
                );
                Operand::Local(target)
            }
            Expression::UnaryOp { operator, operand } => todo!(),
            Expression::FunctionCall { callee, args } => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::MemberAccess { object, member } => todo!(),
            Expression::ArrayLiteral(expressions) => todo!(),
            Expression::StructLiteral { name, fields } => todo!(),
            Expression::TernaryOp {
                condition,
                true_expr,
                false_expr,
            } => todo!(),
            Expression::Parenthesized(expression) => todo!(),
            Expression::Cast { expr, target_type } => todo!(),
        }
    }
}
