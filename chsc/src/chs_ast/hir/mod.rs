use crate::{chs_ast::ast, chs_types::CHSType, chs_util::*, return_chs_error};

use super::{
    RawModule,
    ast::*,
    typechecker::CHSInfer,
};

use chslexer::*;


#[derive(Debug)]
pub struct HIRModule {
    pub items: Vec<HIRModuleItem>,
}

impl HIRModule {
    pub fn from_ast(ast: Ast) -> Self {
        let items = ast
            .into_iter()
            .map(|item| match item {
                AstNode::FunctionDecl(func) => {
                    HIRModuleItem::Function(HIRFunction::from_ast_function(func))
                }
                AstNode::StructDecl(struct_decl) => todo!(),
                AstNode::EnumDecl(enum_decl) => todo!(),
            })
            .collect();
        Self { items }
    }
}

#[derive(Debug)]
pub enum HIRModuleItem {
    Function(HIRFunction),
    ExternFunction(HIRExternFunction),
}

#[derive(Debug)]
pub struct HIRFunction {
    pub name: Token,
    pub params: Vec<HIRParam>,
    pub return_type: Option<CHSType>,
    pub body: Statement,
}

impl HIRFunction {
    fn from_ast_function(func: FunctionDecl) -> HIRFunction {
        Self {
            name: func.name,
            params: func
                .parameters
                .into_iter()
                .map(HIRParam::from_ast_param)
                .collect(),
            return_type: func.return_type,
            body: func.body,
        }
    }
}

#[derive(Debug)]
pub struct HIRExternFunction {
    pub name: Token,
    pub fn_type: CHSType,
}
impl HIRExternFunction {
    fn from_ast_extern_function() -> HIRExternFunction {
        todo!()
    }
}

#[derive(Debug)]
pub struct HIRParam {
    pub name: Token,
    pub param_type: CHSType,
}

impl HIRParam {
    fn from_ast_param(param: Param) -> HIRParam {
        Self {
            name: param.name,
            param_type: param.type_,
        }
    }
}

impl CHSInfer for Expression {
    fn cast(&mut self, ty: CHSType) -> CHSResult<()> {
        todo!()
    }
    fn infer(&self) -> CHSType {
        match self {
            Expression::Nil => CHSType::Nil,
            Expression::Bool(_) => todo!(),
            Expression::Int(token) => CHSType::I32,
            Expression::Float(token) => todo!(),
            Expression::String(token) => todo!(),
            Expression::Char(token) => todo!(),
            Expression::Identifier(token) => todo!(),
            Expression::BinaryOp {
                left,
                operator,
                right,
            } => left.infer(),
            Expression::UnaryOp { operator, operand } => todo!(),
            Expression::FunctionCall { callee, args } => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::MemberAccess { object, member } => todo!(),
            Expression::ArrayLiteral(vec) => todo!(),
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
