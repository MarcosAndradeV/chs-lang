use crate::{
    chs_lexer::{Span, Token},
    chs_types::CHSType,
    chs_util::*,
    return_chs_error,
};

use super::{
    nodes::{self, Operator}, typechecker::CHSInfer, ModuleImpl, RawModule
};

#[derive(Debug)]
pub struct HIROperator {
    pub span: Span<String>,
    pub op: Operator,
}

impl HIROperator {
    fn from_ast_op(token: Token, op: Operator) -> HIROperator {
        Self {
            span: Span::from(token),
            op,
        }
    }

    pub fn get_type_of_op(&self, lty: &CHSType, rty: &CHSType) -> CHSResult<CHSType> {
        self.op.get_type_of_op(lty, rty)
    }
}

#[derive(Debug)]
pub struct HIRModule<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<HIRModuleItem>,
}

impl<'src> ModuleImpl<'src> for HIRModule<'src> {
    fn get_span_str<T>(&self, span: &Span<T>) -> &'src str {
        &self.raw_module[span]
    }

    fn get_token_str(&self, token: &Token) -> &'src str {
        &self.raw_module[token]
    }

    fn get_file_path(&self) -> &'src str {
        &self.raw_module.file_path
    }
}

impl<'src> HIRModule<'src> {
    pub fn from_ast(ast: nodes::ASTModule<'src>) -> Self {
        let raw_module = ast.raw_module;
        let items = ast
            .items
            .into_iter()
            .map(|item| match item {
                nodes::ModuleItem::Function(func) => {
                    HIRModuleItem::Function(HIRFunction::from_ast_function(func))
                }
                nodes::ModuleItem::ExternFunction(func) => {
                    HIRModuleItem::ExternFunction(HIRExternFunction::from_ast_extern_function(func))
                }
                nodes::ModuleItem::MacroCall(..) => todo!(),
            })
            .collect();
        Self { raw_module, items }
    }
}

#[derive(Debug)]
pub enum HIRModuleItem {
    Function(HIRFunction),
    ExternFunction(HIRExternFunction),
}

#[derive(Debug)]
pub struct HIRFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
    pub params: Vec<HIRParam>,
    pub return_type: CHSType,
    pub body: HIRBlock,
}
impl HIRFunction {
    fn from_ast_function(func: nodes::FunctionDecl) -> HIRFunction {
        Self {
            name: func.name.source,
            fn_type: func.fn_type,
            params: func
                .params
                .into_iter()
                .map(HIRParam::from_ast_param)
                .collect(),
            return_type: func.ret_type,
            body: HIRBlock {
                statements: func.body.into_iter().map(HIRStmt::from_ast_expr).collect(),
            },
        }
    }
}

#[derive(Debug)]
pub struct HIRExternFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
}
impl HIRExternFunction {
    fn from_ast_extern_function(func: nodes::ExternFunctionDecl) -> HIRExternFunction {
        Self {
            name: func.name.source,
            fn_type: func.fn_type,
        }
    }
}

#[derive(Debug)]
pub struct HIRParam {
    pub name: Span<String>,
    pub param_type: CHSType,
}

impl HIRParam {
    fn from_ast_param(param: nodes::Param) -> HIRParam {
        Self {
            name: param.name.source,
            param_type: param.ty,
        }
    }
}

#[derive(Debug)]
pub struct HIRBlock {
    pub statements: Vec<HIRStmt>,
}

impl IntoIterator for HIRBlock {
    type Item = HIRStmt;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }

    type IntoIter = std::vec::IntoIter<HIRStmt>;
}

#[derive(Debug)]
pub enum HIRStmt {
    Assign {
        span: Span<()>,
        target: Box<HIRExpr>,
        value: Box<HIRExpr>,
    },
    VarDecl {
        name: Span<String>,
        ty: Option<CHSType>,
        value: Box<HIRExpr>,
    },
    If {
        span: Span<()>,
        condition: Box<HIRExpr>,
        then_branch: HIRBlock,
        else_branch: Option<HIRBlock>,
    },
    While {
        span: Span<()>,
        condition: Box<HIRExpr>,
        body: HIRBlock,
    },
    Return {
        span: Span<()>,
        expr: Option<Box<HIRExpr>>,
    },
    ExprStmt {
        // span: Span<()>, TODO: Implement span for ExprStmt
        value: HIRExpr,
    },
}

impl HIRStmt {
    pub fn from_ast_expr(expr: nodes::Expression) -> Self {
        match expr {
            nodes::Expression::VarDecl(v) => Self::VarDecl {
                name: v.token.source,
                ty: v.ty,
                value: HIRExpr::from_ast_expr(v.value).into(),
            },
            nodes::Expression::Assign(a) => Self::Assign {
                span: Span::from(a.token),
                target: HIRExpr::from_ast_expr(a.target).into(),
                value: HIRExpr::from_ast_expr(a.value).into(),
            },
            nodes::Expression::IfExpression(i) => Self::If {
                span: Span::from(i.token),
                condition: HIRExpr::from_ast_expr(i.cond).into(),
                then_branch: HIRBlock {
                    statements: i.body.into_iter().map(Self::from_ast_expr).collect(),
                },
                else_branch: None,
            },
            nodes::Expression::IfElseExpression(i) => Self::If {
                span: Span::from(i.token),
                condition: HIRExpr::from_ast_expr(i.cond).into(),
                then_branch: HIRBlock {
                    statements: i.body.into_iter().map(Self::from_ast_expr).collect(),
                },
                else_branch: Some(HIRBlock {
                    statements: i.else_body.into_iter().map(Self::from_ast_expr).collect(),
                }),
            },
            nodes::Expression::WhileExpression(w) => Self::While {
                span: Span::from(w.token),
                condition: HIRExpr::from_ast_expr(w.cond).into(),
                body: HIRBlock {
                    statements: w.body.into_iter().map(Self::from_ast_expr).collect(),
                },
            },
            nodes::Expression::ReturnExpression(r) => Self::Return {
                span: Span::from(r.token),
                expr: r.expr.map(|e| HIRExpr::from_ast_expr(e).into()),
            },
            e => HIRStmt::ExprStmt {
                value: HIRExpr::from_ast_expr(e).into(),
            },
        }
    }
}

#[derive(Debug)]
pub enum HIRExpr {
    Literal(HIRLiteral, Option<CHSType>),
    Identifier(Span<String>, Option<CHSType>),
    Binary {
        ty: Option<CHSType>,
        op: HIROperator,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
    },
    Unary {
        ty: Option<CHSType>,
        op: HIROperator,
        operand: Box<HIRExpr>,
    },
    Call {
        ty: Option<CHSType>,
        span: Span<()>,
        callee: Box<HIRExpr>,
        args: Vec<HIRExpr>,
    },
    Cast {
        span: Span<()>,
        expr: Box<HIRExpr>,
        to_type: CHSType,
    },
    Index {
        span: Span<()>,
        base: Box<HIRExpr>,
        index: Box<HIRExpr>,
    },
    Syscall {
        span: Span<()>,
        arity: usize,
        args: Vec<HIRExpr>,
    },
}

impl CHSInfer for HIRExpr {
    fn cast(&mut self, ty: CHSType) -> CHSResult<()> {
        let actual_ty = self.infer();
        match self {
            HIRExpr::Literal(_, lit_ty_opt @ None) => match (&actual_ty, &ty) {
                (CHSType::Int, CHSType::I32)
                | (CHSType::Int, CHSType::U32)
                | (CHSType::Int, CHSType::I64)
                | (CHSType::Int, CHSType::U64) => {
                    *lit_ty_opt = Some(ty);
                    Ok(())
                }
                (CHSType::I32, CHSType::Int)
                | (CHSType::U32, CHSType::Int)
                | (CHSType::I64, CHSType::Int)
                | (CHSType::U64, CHSType::Int) => {
                    *lit_ty_opt = Some(ty);
                    Ok(())
                }
                _ => {
                    return_chs_error!(
                        "Automatic casting literal of type {} to incompatible type {}.",
                        actual_ty,
                        ty
                    );
                }
            },
            _ => {
                return_chs_error!(
                    "Automatic casting expression of type {} to incompatible type {}.",
                    actual_ty,
                    ty
                );
            }
        }
    }
    fn infer(&self) -> CHSType {
        match self {
            HIRExpr::Literal(l, None) => match l {
                HIRLiteral::Int(_) => CHSType::Int,
                HIRLiteral::Bool(_) => CHSType::Boolean,
                HIRLiteral::Str(_) => CHSType::String,
                HIRLiteral::Char(_) => CHSType::Char,
                HIRLiteral::Void => CHSType::Void,
            },
            HIRExpr::Literal(_, Some(ty)) => ty.clone(),
            HIRExpr::Identifier(_, ty) => ty.clone().unwrap_or(CHSType::Never),
            HIRExpr::Binary { ty, .. } => ty.clone().unwrap_or(CHSType::Never),
            HIRExpr::Unary { ty, .. } => ty.clone().unwrap_or(CHSType::Never),
            HIRExpr::Call { ty, .. } => ty.clone().unwrap_or(CHSType::Never),
            HIRExpr::Cast { to_type, .. } => to_type.clone(),
            HIRExpr::Index { .. } => todo!(),
            HIRExpr::Syscall { .. } => todo!(),
        }
    }
}

impl HIRExpr {
    pub fn from_ast_expr(expr: nodes::Expression) -> Self {
        match expr {
            nodes::Expression::ConstExpression(cexp) => match cexp {
                nodes::ConstExpression::IntegerDefault(s) => {
                    HIRExpr::Literal(HIRLiteral::Int(s), None)
                }
                nodes::ConstExpression::IntegerLiteral(s) => {
                    // HIRExpr::Literal(HIRLiteral::I32(s), None)
                    HIRExpr::Literal(HIRLiteral::Int(s), Some(CHSType::I32))
                }
                nodes::ConstExpression::UnsignedIntegerLiteral(s) => {
                    // HIRExpr::Literal(HIRLiteral::U32(s), None)
                    HIRExpr::Literal(HIRLiteral::Int(s), Some(CHSType::U32))
                }
                nodes::ConstExpression::LongIntegerLiteral(s) => {
                    // HIRExpr::Literal(HIRLiteral::I64(s), None)
                    HIRExpr::Literal(HIRLiteral::Int(s), Some(CHSType::I64))
                }
                nodes::ConstExpression::LongUnsignedIntegerLiteral(s) => {
                    // HIRExpr::Literal(HIRLiteral::U64(s), None)
                    HIRExpr::Literal(HIRLiteral::Int(s), Some(CHSType::U64))
                }
                nodes::ConstExpression::BooleanLiteral(s) => {
                    HIRExpr::Literal(HIRLiteral::Bool(s), None)
                }
                nodes::ConstExpression::Identifier(s) => HIRExpr::Identifier(s, None),
                nodes::ConstExpression::StringLiteral(s) => {
                    HIRExpr::Literal(HIRLiteral::Str(s), None)
                }
                nodes::ConstExpression::CharLiteral(s) => {
                    HIRExpr::Literal(HIRLiteral::Char(s), None)
                }
            },
            nodes::Expression::Binop(b) => HIRExpr::Binary {
                ty: None,
                op: HIROperator::from_ast_op(b.token, b.op),
                lhs: HIRExpr::from_ast_expr(b.left).into(),
                rhs: HIRExpr::from_ast_expr(b.right).into(),
            },
            nodes::Expression::Unop(u) => HIRExpr::Unary {
                ty: None,
                op: HIROperator::from_ast_op(u.token, u.op),
                operand: HIRExpr::from_ast_expr(u.operand).into(),
            },
            nodes::Expression::Call(c) => HIRExpr::Call {
                ty: None,
                span: Span::from(c.token),
                callee: HIRExpr::from_ast_expr(c.callee).into(),
                args: c.args.into_iter().map(HIRExpr::from_ast_expr).collect(),
            },
            nodes::Expression::Group(e) => HIRExpr::from_ast_expr(*e),
            nodes::Expression::Cast(c) => HIRExpr::Cast {
                span: Span::from(c.token),
                expr: HIRExpr::from_ast_expr(c.casted).into(),
                to_type: c.to_type,
            },
            nodes::Expression::Index(i) => HIRExpr::Index {
                span: Span::from(i.token),
                base: HIRExpr::from_ast_expr(i.base).into(),
                index: HIRExpr::from_ast_expr(i.index).into(),
            },
            nodes::Expression::Syscall(s) => HIRExpr::Syscall {
                span: Span::from(s.token),
                arity: s.arity,
                args: s.args.into_iter().map(HIRExpr::from_ast_expr).collect(),
            },
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum HIRLiteral {
    Int(Span<u64>),
    // I32(Span<i32>),
    // U32(Span<u32>),
    // I64(Span<i64>),
    // U64(Span<u64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
    Void,
}
