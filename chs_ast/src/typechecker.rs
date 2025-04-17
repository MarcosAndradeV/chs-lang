use chs_lexer::Span;
use chs_types::{CHSType, TypeEnv};
use chs_util::{return_chs_error, CHSResult};
use std::rc::Rc;

use crate::RawModule;

use crate::hir::{HIRExpr, HIRBlock, HIRFunction, HIRLiteral, HIRModule, HIRModuleItem};

pub struct TypeChecker<'src> {
    env: TypeEnv,
    raw_module: &'src RawModule,
}

impl<'src> TypeChecker<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        Self {
            env: TypeEnv::new(),
            raw_module,
        }
    }

    pub fn env(&self) -> &TypeEnv {
        &self.env
    }

    fn get_span_str(&self, span: &Span<String>) -> &'src str {
        &self.raw_module[span]
    }

    pub fn check_module(&mut self, module: &HIRModule) -> CHSResult<()> {
        for item in &module.items {
            match item {
                HIRModuleItem::Function(func) => self.check_function(func)?,
                HIRModuleItem::ExternFunction(func) => {
                    let name = self.get_span_str(&func.name);
                    self.env.global_insert(name, Rc::new(func.fn_type.clone()));
                }
            }
        }
        Ok(())
    }

    fn check_function(&mut self, func: &HIRFunction) -> CHSResult<()> {
        // Register function in global scope
        let name = self.get_span_str(&func.name);
        self.env.global_insert(name, Rc::new(func.fn_type.clone()));

        // Create new scope for function body
        self.env.locals_new();

        // Add parameters to local scope
        for param in &func.params {
            let param_name = self.get_span_str(&param.name);
            self.env.locals_insert(param_name, Rc::new(param.param_type.clone()));
        }

        // Type check body
        let mut last_type = CHSType::Void;
        for expr in &func.body {
            last_type = self.check_expr(expr)?;
        }

        // Verify return type matches
        self.env.unify(&last_type, &func.return_type)?;

        // Pop function scope
        self.env.locals.pop();

        Ok(())
    }

    fn check_expr(&mut self, expr: &HIRExpr) -> CHSResult<CHSType> {
        match expr {
            HIRExpr::Literal(lit) => self.check_literal(lit),
            HIRExpr::Identifier(name) => {
                let name_str = self.get_span_str(name);
                if let Some(ty) = self.env.get(name_str) {
                    Ok((**ty).clone())
                } else {
                    return_chs_error!("Undefined variable: {}", name_str)
                }
            }
            HIRExpr::Binary { op, lhs, rhs } => {
                let lhs_type = self.check_expr(lhs)?;
                let rhs_type = self.check_expr(rhs)?;

                match op {
                    // Arithmetic operators
                    crate::nodes::Operator::Plus |
                    crate::nodes::Operator::Minus |
                    crate::nodes::Operator::Mult |
                    crate::nodes::Operator::Div |
                    crate::nodes::Operator::Mod => {
                        self.env.unify(&lhs_type, &rhs_type)?;
                        match lhs_type {
                            CHSType::Int | CHSType::UInt => Ok(lhs_type),
                            _ => return_chs_error!("Expected numeric type for arithmetic operation"),
                        }
                    }
                    // Comparison operators
                    crate::nodes::Operator::Lt |
                    crate::nodes::Operator::Gt => {
                        self.env.unify(&lhs_type, &rhs_type)?;
                        match lhs_type {
                            CHSType::Int | CHSType::UInt => Ok(CHSType::Boolean),
                            _ => return_chs_error!("Expected numeric type for comparison"),
                        }
                    }
                    // Equality operators
                    crate::nodes::Operator::Eq |
                    crate::nodes::Operator::NEq => {
                        self.env.unify(&lhs_type, &rhs_type)?;
                        Ok(CHSType::Boolean)
                    }
                    // Logical operators
                    crate::nodes::Operator::LAnd |
                    crate::nodes::Operator::LOr => {
                        self.env.unify(&lhs_type, &CHSType::Boolean)?;
                        self.env.unify(&rhs_type, &CHSType::Boolean)?;
                        Ok(CHSType::Boolean)
                    }
                    // Bitwise operators
                    crate::nodes::Operator::And |
                    crate::nodes::Operator::Or => {
                        self.env.unify(&lhs_type, &rhs_type)?;
                        match lhs_type {
                            CHSType::Int | CHSType::UInt => Ok(lhs_type),
                            _ => return_chs_error!("Expected numeric type for bitwise operation"),
                        }
                    }
                    crate::nodes::Operator::Assign => {
                        self.env.unify(&lhs_type, &rhs_type)?;
                        Ok(lhs_type)
                    }
                    // These operators should not appear in binary expressions
                    crate::nodes::Operator::Negate |
                    crate::nodes::Operator::LNot |
                    crate::nodes::Operator::Refer |
                    crate::nodes::Operator::Deref => {
                        return_chs_error!("Unary operator used in binary expression")
                    }
                }
            }
            HIRExpr::Unary { op, operand } => {
                let operand_type = self.check_expr(operand)?;
                match op {
                    crate::nodes::Operator::Negate => {
                        match operand_type {
                            CHSType::Int | CHSType::UInt => Ok(operand_type),
                            _ => return_chs_error!("Expected numeric type for negation"),
                        }
                    }
                    crate::nodes::Operator::LNot => {
                        self.env.unify(&operand_type, &CHSType::Boolean)?;
                        Ok(CHSType::Boolean)
                    }
                    crate::nodes::Operator::Deref => {
                        match operand_type {
                            CHSType::Pointer(inner) => Ok(*inner),
                            _ => return_chs_error!("Can only dereference pointer types"),
                        }
                    }
                    crate::nodes::Operator::Refer => {
                        Ok(CHSType::Pointer(Box::new(operand_type)))
                    }
                    _ => return_chs_error!("Invalid unary operator"),
                }
            }
            HIRExpr::Call { callee, args } => {
                let callee_type = self.check_expr(callee)?;
                match callee_type {
                    CHSType::Function(param_types, return_type) => {
                        if args.len() != param_types.len() {
                            return_chs_error!("Wrong number of arguments");
                        }
                        for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, expected_type)?;
                        }
                        Ok(*return_type)
                    }
                    _ => return_chs_error!("Called expression is not a function"),
                }
            }
            HIRExpr::Cast { expr, to_type } => {
                let expr_type = self.check_expr(expr)?;
                // For now, only allow numeric casts
                match (&expr_type, to_type) {
                    (CHSType::Int, CHSType::UInt) |
                    (CHSType::UInt, CHSType::Int) => Ok(to_type.clone()),
                    _ => return_chs_error!("Invalid cast between {:?} and {:?}", expr_type, to_type),
                }
            }
            HIRExpr::Index { base, index } => {
                let base_type = self.check_expr(base)?;
                let index_type = self.check_expr(index)?;

                // Check that index is numeric
                match index_type {
                    CHSType::Int | CHSType::UInt => {},
                    _ => return_chs_error!("Array index must be numeric"),
                }

                // Get element type from array type
                match base_type {
                    CHSType::Slice(elem_type) => Ok(*elem_type),
                    _ => return_chs_error!("Cannot index into non-array type"),
                }
            }
            HIRExpr::Assign { target, value } => {
                let target_type = self.check_expr(target)?;
                let value_type = self.check_expr(value)?;
                self.env.unify(&target_type, &value_type)?;
                Ok(target_type)
            }
            HIRExpr::VarDecl { name, ty, value } => {
                let value_type = self.check_expr(value)?;
                let var_type = if let Some(explicit_type) = ty {
                    self.env.unify(&value_type, explicit_type)?;
                    explicit_type.clone()
                } else {
                    value_type.clone()
                };
                let name_str = self.get_span_str(name);
                self.env.locals_insert(name_str, Rc::new(var_type.clone()));
                Ok(var_type)
            }
            HIRExpr::Block(block) => {
                self.env.locals_new();
                let mut last_type = CHSType::Void;
                for expr in &block.expressions {
                    last_type = self.check_expr(expr)?;
                }
                self.env.locals.pop();
                Ok(last_type)
            }
            HIRExpr::If { condition, then_branch, else_branch } => {
                let cond_type = self.check_expr(condition)?;
                self.env.unify(&cond_type, &CHSType::Boolean)?;

                let then_type = self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    let else_type = self.check_block(else_branch)?;
                    self.env.unify(&then_type, &else_type)?;
                    Ok(then_type)
                } else {
                    Ok(CHSType::Void)
                }
            }
            HIRExpr::While { condition, body } => {
                let cond_type = self.check_expr(condition)?;
                self.env.unify(&cond_type, &CHSType::Boolean)?;
                self.check_block(body)?;
                Ok(CHSType::Void)
            }
            HIRExpr::Syscall { arity: _, args } => {
                for arg in args {
                    self.check_expr(arg)?;
                }
                Ok(CHSType::Int)
            }
            HIRExpr::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr)
                } else {
                    Ok(CHSType::Void)
                }
            }
        }
    }

    fn check_block(&mut self, block: &HIRBlock) -> CHSResult<CHSType> {
        self.env.locals_new();
        let mut last_type = CHSType::Void;
        for expr in &block.expressions {
            last_type = self.check_expr(expr)?;
        }
        self.env.locals.pop();
        Ok(last_type)
    }

    fn check_literal(&self, lit: &HIRLiteral) -> CHSResult<CHSType> {
        Ok(match lit {
            HIRLiteral::Int(_) => CHSType::Int,
            HIRLiteral::Bool(_) => CHSType::Boolean,
            HIRLiteral::Str(_) => CHSType::String,
            HIRLiteral::Char(_) => CHSType::Char,
            HIRLiteral::Void => CHSType::Void,
        })
    }
}
