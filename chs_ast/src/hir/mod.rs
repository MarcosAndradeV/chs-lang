use chs_lexer::{Span, Token};
use chs_types::CHSType;
use chs_util::CHSResult;

use crate::typechecker::{CHSInfer, TypeEnv};
use crate::{nodes, RawModule};

use crate::nodes::Operator;

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

impl<'src> HIRModule<'src> {
    pub fn from_ast(ast: nodes::Module<'src>) -> Self {
        Self {
            raw_module: ast.raw_module,
            items: ast
                .items
                .into_iter()
                .map(|item| match item {
                    nodes::ModuleItem::Function(func) => {
                        HIRModuleItem::Function(HIRFunction::from_ast_function(func))
                    }
                    nodes::ModuleItem::ExternFunction(func) => HIRModuleItem::ExternFunction(
                        HIRExternFunction::from_ast_extern_function(func),
                    ),
                })
                .collect(),
        }
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
    pub body: Vec<HIRExpr>,
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
            body: func.body.into_iter().map(HIRExpr::from_ast_expr).collect(),
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
    pub expressions: Vec<HIRExpr>,
}

impl CHSInfer for HIRBlock {
    fn infer(&self, env: &TypeEnv) -> CHSType {
        match self.expressions.last() {
            Some(e) => e.infer(env),
            None => CHSType::Void,
        }
    }
}

#[derive(Debug)]
pub enum HIRExpr {
    Literal(HIRLiteral),
    Identifier(Span<String>),
    Binary {
        op: HIROperator,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
    },
    Unary {
        op: HIROperator,
        operand: Box<HIRExpr>,
    },
    Call {
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
    Block(HIRBlock),
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
    Syscall {
        span: Span<()>,
        arity: usize,
        args: Vec<HIRExpr>,
    },
    Return {
        span: Span<()>,
        expr: Option<Box<HIRExpr>>,
    },
}

impl CHSInfer for HIRExpr {
    fn infer(&self, env: &TypeEnv) -> CHSType {
        match self {
            HIRExpr::Literal(l) => match l {
                HIRLiteral::Int(_) => CHSType::Int,
                HIRLiteral::Bool(_) => CHSType::Boolean,
                HIRLiteral::Str(_) => CHSType::String,
                HIRLiteral::Char(_) => CHSType::Char,
                HIRLiteral::Void => CHSType::Void,
            },
            HIRExpr::Identifier(_) => todo!(),
            HIRExpr::Binary { .. } => todo!(),
            HIRExpr::Unary { .. } => todo!(),
            HIRExpr::Call { .. } => todo!(),
            HIRExpr::Cast { .. } => todo!(),
            HIRExpr::Index { .. } => todo!(),
            HIRExpr::Assign { .. } => todo!(),
            HIRExpr::VarDecl { .. } => todo!(),
            HIRExpr::Block(..) => todo!(),
            HIRExpr::If {
                else_branch: Some(else_branch),
                ..
            } => else_branch.infer(env),
            HIRExpr::If { .. } => CHSType::Void,
            HIRExpr::While { .. } => todo!(),
            HIRExpr::Syscall { .. } => todo!(),
            HIRExpr::Return {
                span: _,
                expr: Some(e),
            } => e.infer(env),
            HIRExpr::Return {
                span: _,
                expr: None,
            } => CHSType::Void,
        }
    }
}

impl HIRExpr {
    fn from_ast_expr(expr: nodes::Expression) -> Self {
        match expr {
            nodes::Expression::ConstExpression(cexp) => match cexp {
                nodes::ConstExpression::IntegerLiteral(s) => HIRExpr::Literal(HIRLiteral::Int(s)),
                nodes::ConstExpression::BooleanLiteral(s) => HIRExpr::Literal(HIRLiteral::Bool(s)),
                nodes::ConstExpression::Identifier(s) => HIRExpr::Identifier(s),
                nodes::ConstExpression::StringLiteral(s) => HIRExpr::Literal(HIRLiteral::Str(s)),
                nodes::ConstExpression::CharLiteral(s) => HIRExpr::Literal(HIRLiteral::Char(s)),
            },
            nodes::Expression::Binop(b) => HIRExpr::Binary {
                op: HIROperator::from_ast_op(b.token, b.op),
                lhs: HIRExpr::from_ast_expr(b.left).into(),
                rhs: HIRExpr::from_ast_expr(b.right).into(),
            },
            nodes::Expression::Unop(u) => HIRExpr::Unary {
                op: HIROperator::from_ast_op(u.token, u.op),
                operand: HIRExpr::from_ast_expr(u.operand).into(),
            },
            nodes::Expression::Call(c) => HIRExpr::Call {
                span: Span::from(c.token),
                callee: HIRExpr::from_ast_expr(c.callee).into(),
                args: c.args.into_iter().map(HIRExpr::from_ast_expr).collect(),
            },
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
            nodes::Expression::VarDecl(v) => HIRExpr::VarDecl {
                name: v.token.source,
                ty: v.ty,
                value: HIRExpr::from_ast_expr(v.value).into(),
            },
            nodes::Expression::Assign(a) => HIRExpr::Assign {
                span: Span::from(a.token),
                target: HIRExpr::from_ast_expr(a.target).into(),
                value: HIRExpr::from_ast_expr(a.value).into(),
            },
            nodes::Expression::Group(e) => HIRExpr::from_ast_expr(*e),
            nodes::Expression::IfExpression(i) => HIRExpr::If {
                span: Span::from(i.token),
                condition: HIRExpr::from_ast_expr(i.cond).into(),
                then_branch: HIRBlock {
                    expressions: i.body.into_iter().map(HIRExpr::from_ast_expr).collect(),
                },
                else_branch: None,
            },
            nodes::Expression::IfElseExpression(i) => HIRExpr::If {
                span: Span::from(i.token),
                condition: HIRExpr::from_ast_expr(i.cond).into(),
                then_branch: HIRBlock {
                    expressions: i.body.into_iter().map(HIRExpr::from_ast_expr).collect(),
                },
                else_branch: Some(HIRBlock {
                    expressions: i
                        .else_body
                        .into_iter()
                        .map(HIRExpr::from_ast_expr)
                        .collect(),
                }),
            },
            nodes::Expression::WhileExpression(w) => HIRExpr::While {
                span: Span::from(w.token),
                condition: HIRExpr::from_ast_expr(w.cond).into(),
                body: HIRBlock {
                    expressions: w.body.into_iter().map(HIRExpr::from_ast_expr).collect(),
                },
            },
            nodes::Expression::ReturnExpression(r) => HIRExpr::Return {
                span: Span::from(r.token),
                expr: r.expr.map(|e| HIRExpr::from_ast_expr(e).into()),
            },
        }
    }
}

#[derive(Debug)]
pub enum HIRLiteral {
    Int(Span<i64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
    Void,
}
