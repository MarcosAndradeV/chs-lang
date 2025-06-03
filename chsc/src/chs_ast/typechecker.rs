use std::{collections::HashMap, rc::Rc};

use crate::{chs_types::CHSType, chs_util::*, return_chs_error};

use super::{
    ast::{Statement, Expression}, hir::{HIRFunction, HIRModule, HIRModuleItem}, RawModule
};

pub struct TypeChecker<'src> {
    env: TypeEnv,
    raw_module: &'src RawModule,
    curr_ret_type: CHSType,
}

impl<'src> TypeChecker<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        Self {
            env: TypeEnv::new(),
            raw_module,
            curr_ret_type: CHSType::Never,
        }
    }

    pub fn check_module(&mut self, module: &mut HIRModule) -> CHSResult<()> {
        for item in &mut module.items {
            match item {
                HIRModuleItem::Function(func) => {
                    let name = &func.name.source;
                    if let Some(_) = self.env.global_get(name) {
                        return_chs_error!(
                            "{}:{}: Duplicate function declaration: {}",
                            self.raw_module.file_path,
                            func.name.loc,
                            name
                        );
                    }
                    self.env.global_insert(
                        name,
                        Rc::new(CHSType::Function(
                            func.params.iter().map(|p| p.param_type.clone()).collect(),
                            func.return_type.clone().unwrap_or(CHSType::Void).into(),
                        )),
                    );
                }
                HIRModuleItem::ExternFunction(func) => {
                    let name = &func.name.source;
                    if let Some(_) = self.env.global_get(name) {
                        return_chs_error!(
                            "{}:{}: Duplicate function declaration: {}",
                            self.raw_module.file_path,
                            func.name.loc,
                            name
                        );
                    }
                    self.env.global_insert(name, Rc::new(func.fn_type.clone()));
                }
            }
        }
        for item in &mut module.items {
            match item {
                HIRModuleItem::Function(func) => self.check_function(func)?,
                HIRModuleItem::ExternFunction(_) => {}
            }
        }
        if let Some(CHSType::Function(args, ret)) = self.env.global_get("main") {
            if args.is_empty() && **ret == CHSType::I32 {
                // Main function is valid
            } else {
                return_chs_error!("`main` function must have no arguments and return an i32")
            }
        }
        Ok(())
    }

    fn check_function(&mut self, func: &mut HIRFunction) -> CHSResult<()> {
        self.env.locals_new();

        // Add parameters to local scope
        for param in &func.params {
            let param_name = &param.name.source;
            self.env
                .locals_insert(param_name, Rc::new(param.param_type.clone()));
        }

        // Type check body
        self.curr_ret_type = func.return_type.clone().unwrap_or(CHSType::Void);
        let return_flow = self.check_stmt(&func.body)?;
        self.curr_ret_type = CHSType::Never;

        if return_flow != ReturnFlow::Always {
            return_chs_error!(
                "{}:{} Function `{}` does not return on all paths",
                self.raw_module.file_path,
                func.name.loc,
                func.name
            )
        }

        // Verify return type matches
        // self.env.unify(&last_type, &func.return_type).map_err(|_| {
        //     chs_error!(
        //         "{} Type mismatch in function `{}`: expected `{}`, found `{}`",
        //         &func.name.loc,
        //         name,
        //         func.return_type,
        //         last_type
        //     )
        // })?;

        // Pop function scope
        self.env.locals.pop();

        Ok(())
    }

    fn check_expr(&mut self, expr: &mut Expression) -> CHSResult<CHSType> {
        match expr {
            Expression::Nil => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::Int(token) => todo!(),
            Expression::Float(token) => todo!(),
            Expression::String(token) => todo!(),
            Expression::Char(token) => todo!(),
            Expression::Identifier(token) => todo!(),
            Expression::BinaryOp { left, operator, right } => todo!(),
            Expression::UnaryOp { operator, operand } => todo!(),
            Expression::FunctionCall { callee, args } => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::MemberAccess { object, member } => todo!(),
            Expression::ArrayLiteral(vec) => todo!(),
            Expression::StructLiteral { name, fields } => todo!(),
            Expression::TernaryOp { condition, true_expr, false_expr } => todo!(),
            Expression::Parenthesized(expression) => todo!(),
            Expression::Cast { expr, target_type } => todo!(),
        }
    }

    fn check_stmt(&mut self, stmt: & Statement) -> CHSResult<ReturnFlow> {
        match stmt {
            Statement::ReturnStatement(..) => Ok(ReturnFlow::Always),
            Statement::BlockStatement(block) => self.check_block(block),
            Statement::LetStatement(let_statement) => todo!(),
            Statement::ExpressionStatement(expression) => todo!(),
            Statement::IfStatement(if_statement) => todo!(),
            Statement::WhileStatement(while_statement) => todo!(),
            Statement::ForStatement(for_statement) => todo!(),
            Statement::AssignmentStatement(assignment_statement) => todo!(),
            Statement::Empty => todo!(),
        }
    }

    fn check_block(&mut self, block: &Vec<Statement>) -> CHSResult<ReturnFlow> {
        self.env.locals_new();
        let mut return_flow = ReturnFlow::Never;
        for stmt in block {
            let flow = self.check_stmt(stmt)?;
            return_flow = ReturnFlow::combine(return_flow, flow);
            if flow == ReturnFlow::Always {
                break;
            }
        }
        self.env.locals.pop();
        Ok(return_flow)
    }
}

/// Type environment for managing type declarations and scoping
#[derive(Debug)]
pub struct TypeEnv {
    /// Global types declarations
    pub globals: HashMap<String, Rc<CHSType>>,
    pub locals: Vec<HashMap<String, Rc<CHSType>>>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
        }
    }

    /// Attempts to unify two types
    pub fn unify(&mut self, t1: &CHSType, t2: &CHSType) -> CHSResult<()> {
        match (t1, t2) {
            (CHSType::Function(args1, ret1), CHSType::Function(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return_chs_error!("Function argument count mismatch");
                }
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2)?;
                }
                self.unify(ret1, ret2)
            }
            (CHSType::Any, _) | (_, CHSType::Any) => Ok(()),
            (a, b) if a == b => Ok(()),
            (a, b) => return_chs_error!("Types cannot be unified {} {}", a, b),
        }
    }

    /// Adds a type declaration
    pub fn global_insert(&mut self, k: impl ToString, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.globals.insert(k.to_string(), v)
    }

    // Get a global type declaration
    pub fn global_get(&self, k: &str) -> Option<&CHSType> {
        self.globals.get(k).map(|v| v.as_ref())
    }

    /// Gets a type declaration
    pub fn get(&self, k: &str) -> Option<&Rc<CHSType>> {
        for scope in self.locals.iter().rev() {
            if let Some(t) = scope.get(k) {
                return Some(t);
            }
        }
        self.globals.get(k)
    }

    /// Inserts a type into the local scope
    pub fn locals_insert(&mut self, k: impl ToString, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .insert(k.to_string(), v)
    }

    /// Extends the local scope with new types
    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (impl ToString, Rc<CHSType>)>) {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .extend(iter.map(|(k, v)| (k.to_string(), v)));
    }

    /// Creates a new local scope
    pub fn locals_new(&mut self) {
        self.locals.push(HashMap::new());
    }
}

pub trait CHSInfer {
    // Infers the type
    fn infer(&self) -> CHSType;
    // Automatic casts the type to the given type. Errors if the type cannot be cast
    fn cast(&mut self, ty: CHSType) -> CHSResult<()>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnFlow {
    Always,
    Never,
    Sometimes,
}

impl ReturnFlow {
    pub fn combine(self, other: ReturnFlow) -> ReturnFlow {
        match (self, other) {
            (ReturnFlow::Always, _) | (_, ReturnFlow::Always) => ReturnFlow::Always,
            (ReturnFlow::Never, _) | (_, ReturnFlow::Never) => ReturnFlow::Never,
            _ => ReturnFlow::Sometimes,
        }
    }
}
