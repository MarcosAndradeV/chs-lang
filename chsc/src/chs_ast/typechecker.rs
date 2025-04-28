use std::{collections::HashMap, rc::Rc};

use crate::{chs_error, chs_lexer::Span, chs_types::CHSType, chs_util::*, return_chs_error};

use super::{
    ModuleImpl, RawModule,
    hir::{HIRBlock, HIRExpr, HIRFunction, HIRModule, HIRModuleItem, HIRStmt},
    nodes::OperatorKind,
};

pub struct TypeChecker<'src> {
    env: TypeEnv,
    raw_module: &'src RawModule,
    curr_ret_type: CHSType,
}

impl<'src> ModuleImpl<'src> for TypeChecker<'src> {
    fn get_span_str<T>(&self, span: &Span<T>) -> &'src str {
        &self.raw_module[span]
    }

    fn get_token_str(&self, token: &crate::chs_lexer::Token) -> &'src str {
        &self.raw_module[token]
    }

    fn get_file_path(&self) -> &'src str {
        &self.raw_module.file_path
    }
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
                    let name = self.get_span_str(&func.name);
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
                HIRModuleItem::ExternFunction(func) => {
                    let name = self.get_span_str(&func.name);
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
            } else {
                return_chs_error!("`main` function must have no arguments and return an i32")
            }
        }
        Ok(())
    }

    fn check_function(&mut self, func: &mut HIRFunction) -> CHSResult<()> {
        self.env.locals_new();

        for param in &func.params {
            let param_name = self.get_span_str(&param.name);
            self.env
                .locals_insert(param_name, Rc::new(param.param_type.clone()));
        }

        self.curr_ret_type = func.return_type.clone();
        let return_flow = self.check_block(&mut func.body)?;
        self.curr_ret_type = CHSType::Never;

        if return_flow != ReturnFlow::Always {
            return_chs_error!(
                "{}:{} Function `{}` does not return on all paths",
                self.raw_module.file_path,
                func.name.loc,
                self.get_span_str(&func.name)
            )
        }

        self.env.locals.pop();

        Ok(())
    }

    fn check_expr(&mut self, expr: &mut HIRExpr) -> CHSResult<CHSType> {
        match expr {
            HIRExpr::Literal(..) => Ok(expr.infer()),
            HIRExpr::Identifier(name, ty) => {
                let name_str = self.get_span_str(name);
                let t: CHSType = if let Some(ty) = self.env.get(name_str) {
                    (**ty).clone()
                } else {
                    return_chs_error!(
                        "{}:{} Undefined variable: {}",
                        self.raw_module.file_path,
                        name.loc,
                        name_str
                    )
                };
                *ty = Some(t.clone());
                Ok(t)
            }
            HIRExpr::Binary { ty, op, lhs, rhs } => {
                let lhs_type = self.check_expr(lhs)?;
                let rhs_type = self.check_expr(rhs)?;

                let expr_ty = match op.kind {
                    OperatorKind::Plus | OperatorKind::Minus => match (&lhs_type, &rhs_type) {
                        (CHSType::Pointer(_), rhs_type) if rhs_type.is_numeric() => {
                            lhs_type.clone()
                        }

                        (lhs_type, CHSType::Pointer(_))
                            if op.kind == OperatorKind::Plus && lhs_type.is_numeric() =>
                        {
                            rhs_type.clone()
                        }

                        (CHSType::Pointer(ptr_type1), CHSType::Pointer(ptr_type2))
                            if op.kind == OperatorKind::Minus =>
                        {
                            if ptr_type1 == ptr_type2 {
                                CHSType::I64
                            } else {
                                return_chs_error!(
                                    "{} Cannot subtract pointers of different types: {} and {}",
                                    op.span.loc,
                                    lhs_type,
                                    rhs_type
                                );
                            }
                        }

                        _ => {
                            self.ensure_numeric_type(&lhs_type, lhs.span())?;
                            self.ensure_numeric_type(&rhs_type, rhs.span())?;
                            self.unify_numeric_types(
                                &lhs_type,
                                &rhs_type,
                                lhs,
                                rhs,
                                op.span.to_span(),
                            )?
                        }
                    },
                    OperatorKind::Mult | OperatorKind::Div | OperatorKind::Mod => {
                        self.ensure_numeric_type(&lhs_type, lhs.span())?;
                        self.ensure_numeric_type(&rhs_type, rhs.span())?;

                        let result_type = self.unify_numeric_types(
                            &lhs_type,
                            &rhs_type,
                            lhs,
                            rhs,
                            op.span.to_span(),
                        )?;

                        result_type
                    }

                    OperatorKind::Le | OperatorKind::Ge | OperatorKind::Lt | OperatorKind::Gt => {
                        self.ensure_comparable_type(&lhs_type, lhs.span())?;
                        self.ensure_comparable_type(&rhs_type, rhs.span())?;

                        self.unify_numeric_types(
                            &lhs_type,
                            &rhs_type,
                            lhs,
                            rhs,
                            op.span.to_span(),
                        )?;

                        CHSType::Boolean
                    }

                    OperatorKind::Eq | OperatorKind::NEq => {
                        self.unify_equality_types(
                            &lhs_type,
                            &rhs_type,
                            lhs,
                            rhs,
                            op.span.to_span(),
                        )?;

                        CHSType::Boolean
                    }

                    OperatorKind::LAnd | OperatorKind::LOr => {
                        self.ensure_boolean_type(&lhs_type, op.span.to_span())?;
                        self.ensure_boolean_type(&rhs_type, op.span.to_span())?;

                        CHSType::Boolean
                    }

                    OperatorKind::BitAnd
                    | OperatorKind::BitXor
                    | OperatorKind::Shl
                    | OperatorKind::Shr
                    | OperatorKind::BitOr => {
                        self.ensure_integer_type(&lhs_type, lhs.span())?;
                        self.ensure_integer_type(&rhs_type, rhs.span())?;

                        if matches!(op.kind, OperatorKind::Shl | OperatorKind::Shr) {
                            lhs_type.clone()
                        } else {
                            self.unify_integer_types(
                                &lhs_type,
                                &rhs_type,
                                lhs,
                                rhs,
                                op.span.to_span(),
                            )?
                        }
                    }

                    _ => {
                        return_chs_error!(
                            "{} Operador '{}' não é suportado em expressões binárias",
                            op.span.loc,
                            op
                        );
                    }
                };

                *ty = Some(expr_ty.clone());
                Ok(expr_ty)
            }
            HIRExpr::Unary { ty, op, operand } => {
                let operand_type = self.check_expr(operand)?;
                let expr_ty = match op.kind {
                    OperatorKind::Negate => match operand_type {
                        CHSType::I32 => CHSType::I32,
                        CHSType::U32 => CHSType::I32,
                        CHSType::I64 => CHSType::I64,
                        CHSType::U64 => CHSType::I64,
                        _ => {
                            return_chs_error!("{} Expected numeric type for negation", op.span.loc)
                        }
                    },
                    OperatorKind::LNot => {
                        self.env.unify(&operand_type, &CHSType::Boolean)?;
                        CHSType::Boolean
                    }
                    OperatorKind::Deref => match operand_type {
                        CHSType::Pointer(inner) => *inner,
                        _ => {
                            return_chs_error!(
                                "{}:{} Can only dereference pointer types",
                                self.get_file_path(),
                                op.span.loc
                            )
                        }
                    },
                    OperatorKind::Refer => CHSType::Pointer(Box::new(operand_type)),
                    _ => unreachable!("Invalid unary operator"),
                };
                *ty = Some(expr_ty.clone());
                Ok(expr_ty)
            }
            HIRExpr::Call {
                ty,
                span,
                callee,
                args,
            } => {
                let callee_type = self.check_expr(callee)?;
                match callee_type {
                    CHSType::Function(param_types, return_type) => {
                        if args.len() < param_types.len() {
                            return_chs_error!("{} Wrong number of arguments", span.loc);
                        }
                        for (arg, expected_type) in args.iter_mut().zip(param_types.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, expected_type)?;
                        }
                        *ty = Some((*return_type).clone());
                        Ok(ty.clone().unwrap())
                    }
                    CHSType::VariadicFunction(param_types, return_type) => {
                        if args.len() < param_types.len() {
                            return_chs_error!("{} Wrong number of arguments", span.loc);
                        }
                        for (arg, expected_type) in args.iter_mut().zip(param_types.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, expected_type)?;
                        }
                        for arg in args.iter_mut() {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, &CHSType::AnyOpaque)?;
                        }
                        *ty = Some((*return_type).clone());
                        Ok(ty.clone().unwrap())
                    }
                    _ => return_chs_error!("{} Called expression is not a function", span.loc),
                }
            }
            HIRExpr::Cast {
                span,
                expr,
                to_type,
            } => {
                let expr_type = self.check_expr(expr)?;

                match (&expr_type, &to_type) {
                    (a, b) if a == *b => Ok(to_type.clone()),
                    (CHSType::I32, CHSType::U32) | (CHSType::U32, CHSType::I32) => {
                        Ok(to_type.clone())
                    }
                    (CHSType::Pointer(_), CHSType::Pointer(any))
                    | (CHSType::Pointer(any), CHSType::Pointer(_))
                        if **any == CHSType::AnyOpaque =>
                    {
                        Ok(to_type.clone())
                    }
                    _ => return_chs_error!(
                        "{} Invalid cast between {:?} and {:?}",
                        span.loc,
                        expr_type,
                        to_type
                    ),
                }
            }
            HIRExpr::Index { span, base, index } => {
                let base_type = self.check_expr(base)?;
                let index_type = self.check_expr(index)?;

                match index_type {
                    CHSType::U64 => {}
                    _ => return_chs_error!("{} Array index must be u64", span.loc),
                }

                match base_type {
                    CHSType::Slice(elem_type) => Ok(*elem_type.clone()),
                    CHSType::Pointer(elem_type) => Ok(*elem_type.clone()),
                    _ => return_chs_error!("{} Cannot index into non-array type", span.loc),
                }
            }
            HIRExpr::Syscall {
                span: _,
                arity: _,
                args,
            } => {
                for arg in args {
                    self.check_expr(arg)?;
                }
                Ok(CHSType::Int)
            }
        }
    }

    fn check_stmt(&mut self, stmt: &mut HIRStmt) -> CHSResult<ReturnFlow> {
        match stmt {
            HIRStmt::If {
                span,
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expr(condition)?;
                self.env
                    .unify(&cond_type, &CHSType::Boolean)
                    .map_err(|_| chs_error!("{} Expected boolean condition", span.loc))?;

                let then_flow = self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    let flow = self.check_block(else_branch)?;
                    Ok(ReturnFlow::combine(then_flow, flow))
                } else {
                    Ok(ReturnFlow::combine(then_flow, ReturnFlow::Never))
                }
            }
            HIRStmt::While {
                span,
                condition,
                body,
            } => {
                let cond_type = self.check_expr(condition)?;
                self.env
                    .unify(&cond_type, &CHSType::Boolean)
                    .map_err(|_| chs_error!("{} Expected boolean condition", span.loc))?;
                let flow = self.check_block(body)?;
                Ok(flow)
            }
            HIRStmt::Return { span, expr } => {
                if let Some(expr) = expr {
                    let ty = self.check_expr(expr)?;
                    self.env
                        .unify(&self.curr_ret_type, &ty)
                        .or_else(|_| expr.cast(self.curr_ret_type.clone()))
                        .map_err(|_| {
                            chs_error!(
                                "{}:{} Expected return type {} found {}",
                                self.raw_module.file_path,
                                span.loc,
                                self.curr_ret_type,
                                ty
                            )
                        })?
                } else {
                    self.env
                        .unify(&self.curr_ret_type, &CHSType::Void)
                        .map_err(|_| {
                            chs_error!(
                                "{}:{} Expected return type {} found {}",
                                self.raw_module.file_path,
                                span.loc,
                                self.curr_ret_type,
                                CHSType::Void
                            )
                        })?
                }
                Ok(ReturnFlow::Always)
            }
            HIRStmt::Assign {
                span,
                target,
                value: lhs,
                ..
            } => {
                let target_type = self.check_expr(target)?;
                let lhs_type = self.check_expr(lhs)?;
                self.ensure_assignable(target)?;

                self.check_assignment_compatibility(&lhs_type, &target_type, span.to_span())?;
                Ok(ReturnFlow::Never)
            }
            HIRStmt::VarDecl { name, ty, value } => {
                let value_type = self.check_expr(value)?;
                let var_type = if let Some(explicit_type) = ty {
                    self.check_assignment_compatibility(
                        &value_type,
                        &explicit_type,
                        name.to_span(),
                    )?;
                    explicit_type.clone()
                } else {
                    value_type.clone()
                };
                let name_str = self.get_span_str(name);
                *ty = Some(var_type.clone());
                self.env.locals_insert(name_str, Rc::new(var_type.clone()));
                Ok(ReturnFlow::Never)
            }
            HIRStmt::Funcall {
                ty,
                span,
                callee,
                args,
            } => {
                let callee_type = self.check_expr(callee)?;
                match callee_type {
                    CHSType::Function(param_types, return_type) => {
                        if args.len() < param_types.len() {
                            return_chs_error!("{} Wrong number of arguments", span.loc);
                        }
                        for (arg, expected_type) in args.iter_mut().zip(param_types.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, expected_type)?;
                        }
                        *ty = Some((*return_type).clone());
                        Ok(ReturnFlow::Never)
                    }
                    CHSType::VariadicFunction(param_types, return_type) => {
                        if args.len() < param_types.len() {
                            return_chs_error!("{} Wrong number of arguments", span.loc);
                        }
                        for (arg, expected_type) in args.iter_mut().zip(param_types.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, expected_type)?;
                        }
                        for arg in args.iter_mut() {
                            let arg_type = self.check_expr(arg)?;
                            self.env.unify(&arg_type, &CHSType::AnyOpaque)?;
                        }
                        *ty = Some((*return_type).clone());
                        Ok(ReturnFlow::Never)
                    }
                    _ => return_chs_error!("{} Called expression is not a function", span.loc),
                }
            }
            HIRStmt::ExprStmt { span: _, value } => {
                self.check_expr(value)?;
                Ok(ReturnFlow::Never)
            }
        }
    }

    fn check_block(&mut self, block: &mut HIRBlock) -> CHSResult<ReturnFlow> {
        self.env.locals_new();
        let mut return_flow = ReturnFlow::Never;
        for stmt in &mut block.statements {
            let flow = self.check_stmt(stmt)?;
            return_flow = ReturnFlow::combine(return_flow, flow);
            if flow == ReturnFlow::Always {
                break;
            }
        }
        self.env.locals.pop();
        Ok(return_flow)
    }

    pub fn env(&self) -> &TypeEnv {
        &self.env
    }

    fn ensure_numeric_type(&self, ty: &CHSType, span: Span<String>) -> Result<(), CHSError> {
        if !ty.is_numeric() {
            return_chs_error!("{} Expected {{numeric}} type but found {}", span.loc, ty)
        }
        Ok(())
    }

    fn ensure_integer_type(&self, ty: &CHSType, span: Span<String>) -> Result<(), CHSError> {
        match ty {
            CHSType::I32 | CHSType::U32 | CHSType::I64 | CHSType::U64 => Ok(()),
            _ => return_chs_error!("{} Expected {{interger}} type but found {}", span.loc, ty),
        }
    }

    fn ensure_comparable_type(&self, ty: &CHSType, span: Span<String>) -> Result<(), CHSError> {
        match ty {
            CHSType::I32 | CHSType::U32 | CHSType::I64 | CHSType::U64 => Ok(()),
            _ => return_chs_error!("{} Type {} cannot be compared", span.loc, ty),
        }
    }

    fn ensure_boolean_type(&self, ty: &CHSType, span: Span<String>) -> Result<(), CHSError> {
        match ty {
            CHSType::Boolean => Ok(()),
            _ => return_chs_error!("{} Expected boolean found {}", span.loc, ty),
        }
    }

    fn ensure_assignable(&self, expr: &HIRExpr) -> Result<(), CHSError> {
        match expr {
            HIRExpr::Identifier { .. } | HIRExpr::Index { .. } => Ok(()),
            HIRExpr::Unary { op, operand, .. } if op.is_deref() && operand.is_identifier() => {
                Ok(())
            }
            _ => return_chs_error!(
                "{} Left hand side of assignment must be variable, index or field",
                expr.span().loc
            ),
        }
    }

    fn unify_numeric_types(
        &self,
        lhs_type: &CHSType,
        rhs_type: &CHSType,
        lhs: &mut HIRExpr,
        rhs: &mut HIRExpr,
        span: Span<String>,
    ) -> Result<CHSType, CHSError> {
        if lhs_type == rhs_type {
            return Ok(lhs_type.clone());
        }

        match (lhs_type, rhs_type) {
            (CHSType::I32, CHSType::U32) => Ok(CHSType::I32),
            (CHSType::I32, CHSType::I64) | (CHSType::U32, CHSType::I64) => Ok(CHSType::I64),
            (CHSType::I32, CHSType::U64) | (CHSType::U32, CHSType::U64) => Ok(CHSType::I64),
            (CHSType::U32, CHSType::I32) => Ok(CHSType::I32),
            (CHSType::I64, CHSType::I32) | (CHSType::I64, CHSType::U32) => Ok(CHSType::I64),
            (CHSType::U64, CHSType::I32) | (CHSType::U64, CHSType::U32) => Ok(CHSType::I64),

            _ => {
                if let Ok(_) = lhs.cast(rhs_type.clone()) {
                    Ok(rhs_type.clone())
                } else if let Ok(_) = rhs.cast(lhs_type.clone()) {
                    Ok(lhs_type.clone())
                } else {
                    return_chs_error!("{} Cannot unify {} and {}", span.loc, lhs_type, rhs_type)
                }
            }
        }
    }

    fn unify_integer_types(
        &self,
        lhs_type: &CHSType,
        rhs_type: &CHSType,
        lhs: &mut HIRExpr,
        rhs: &mut HIRExpr,
        span: Span<String>,
    ) -> Result<CHSType, CHSError> {
        if lhs_type == rhs_type {
            return Ok(lhs_type.clone());
        }

        match (lhs_type, rhs_type) {
            (CHSType::I32, CHSType::I64) => Ok(CHSType::I64),
            (CHSType::U32, CHSType::U64) => Ok(CHSType::U64),

            (CHSType::I32, CHSType::U32) => Ok(CHSType::I32),
            (CHSType::I32, CHSType::U64) => Ok(CHSType::I64),
            (CHSType::I64, CHSType::U32) => Ok(CHSType::I64),
            (CHSType::I64, CHSType::U64) => Ok(CHSType::I64),

            (CHSType::I64, CHSType::I32) => Ok(CHSType::I64),
            (CHSType::U64, CHSType::U32) => Ok(CHSType::U64),
            (CHSType::U32, CHSType::I32) => Ok(CHSType::I32),
            (CHSType::U64, CHSType::I32) => Ok(CHSType::I64),
            (CHSType::U32, CHSType::I64) => Ok(CHSType::I64),
            (CHSType::U64, CHSType::I64) => Ok(CHSType::I64),

            _ => {
                if let Ok(_) = lhs.cast(rhs_type.clone()) {
                    Ok(rhs_type.clone())
                } else if let Ok(_) = rhs.cast(lhs_type.clone()) {
                    Ok(lhs_type.clone())
                } else {
                    return_chs_error!(
                        "{} bit operations bettewen {} and {} are not suported.",
                        span.loc,
                        lhs_type,
                        rhs_type
                    )
                }
            }
        }
    }

    fn unify_equality_types(
        &self,
        lhs_type: &CHSType,
        rhs_type: &CHSType,
        lhs: &mut HIRExpr,
        rhs: &mut HIRExpr,
        span: Span<String>,
    ) -> Result<(), CHSError> {
        if lhs_type == rhs_type {
            return Ok(());
        }

        if lhs_type.is_numeric() && rhs_type.is_numeric() {
            return Ok(());
        }

        match (lhs_type, rhs_type) {
            (CHSType::Pointer(_), CHSType::Pointer(any))
            | (CHSType::Pointer(any), CHSType::Pointer(_))
                if **any == CHSType::AnyOpaque =>
            {
                Ok(())
            }

            _ => {
                if let Ok(_) = self.env.unify(lhs_type, rhs_type) {
                    Ok(())
                } else if let Ok(_) = lhs.cast(rhs_type.clone()) {
                    Ok(())
                } else if let Ok(_) = rhs.cast(lhs_type.clone()) {
                    Ok(())
                } else {
                    return_chs_error!("{} Cannot compare {} and {}", span.loc, lhs_type, rhs_type)
                }
            }
        }
    }

    fn check_assignment_compatibility(
        &self,
        target_type: &CHSType,
        value_type: &CHSType,
        span: Span<String>,
    ) -> Result<(), CHSError> {
        if target_type == value_type {
            return Ok(());
        }

        match (target_type, value_type) {
            (CHSType::I64, CHSType::I32) | (CHSType::I64, CHSType::U32) => Ok(()),
            (CHSType::U64, CHSType::U32) => Ok(()),

            _ => {
                if let Ok(_) = self.env.unify(target_type, value_type) {
                    Ok(())
                } else {
                    return_chs_error!(
                        "{} Cannot assign variable {} to type {}",
                        span.loc,
                        value_type,
                        target_type
                    )
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv {
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

    pub fn unify(&self, t1: &CHSType, t2: &CHSType) -> CHSResult<()> {
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
            (CHSType::AnyOpaque, _) | (_, CHSType::AnyOpaque) => Ok(()),
            (a, b) if a == b => Ok(()),
            (a, b) => return_chs_error!("Types cannot be unified {} {}", a, b),
        }
    }

    pub fn global_insert(&mut self, k: impl ToString, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.globals.insert(k.to_string(), v)
    }

    pub fn global_get(&self, k: &str) -> Option<&CHSType> {
        self.globals.get(k).map(|v| v.as_ref())
    }

    pub fn get(&self, k: &str) -> Option<&Rc<CHSType>> {
        for scope in self.locals.iter().rev() {
            if let Some(t) = scope.get(k) {
                return Some(t);
            }
        }
        self.globals.get(k)
    }

    pub fn locals_insert(&mut self, k: impl ToString, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .insert(k.to_string(), v)
    }

    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (impl ToString, Rc<CHSType>)>) {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .extend(iter.map(|(k, v)| (k.to_string(), v)));
    }

    pub fn locals_new(&mut self) {
        self.locals.push(HashMap::new());
    }
}

pub trait CHSInfer {
    fn infer(&self) -> CHSType;

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
