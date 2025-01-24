use chs_lexer::Token;
use chs_util::{chs_error, CHSError, Loc};

use chs_types::CHSType;

#[derive(Debug, Default)]
pub struct Module {
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub const_decls: Vec<ConstDecl>,
}

#[derive(Debug)]
pub enum ConstExpression {
    Symbol(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    Void,
}

#[derive(Debug)]
pub enum Expression {
    ConstExpression(ConstExpression),
    ExpressionList(Vec<Self>),
    Binop(Box<Binop>),
    Unop(Box<Unop>),
    Call(Box<Call>),
    VarDecl(Box<VarDecl>),
    Assign(Box<Assign>),
    Group(Box<Self>),
    IfExpression(Box<IfExpression>),
    IfElseExpression(Box<IfElseExpression>),
    WhileExpression(Box<WhileExpression>),
}

impl chs_types::InferType for Expression {
    fn infer(&self) -> CHSType {
        todo!()
    }
}

impl Expression {
    pub fn from_literal_token(token: Token) -> Result<Self, CHSError> {
        use chs_lexer::TokenKind::*;
        match token.kind {
            Interger => {
                let value: i64 = token
                    .value
                    .parse::<i64>()
                    .expect("No interger token. Probably a lexer error.");
                Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                    value,
                )))
            }
            Keyword if token.val_eq("true") => {
                Ok(Self::ConstExpression(ConstExpression::BooleanLiteral(true)))
            }
            Keyword if token.val_eq("false") => Ok(Self::ConstExpression(
                ConstExpression::BooleanLiteral(false),
            )),
            Ident => Ok(Self::ConstExpression(ConstExpression::Symbol(token.value))),
            String => Ok(Self::ConstExpression(ConstExpression::StringLiteral(
                token.value,
            ))),
            _ => chs_error!("{} Unsuported literal", token.loc),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub loc: Loc,
    pub name: String,
    pub args: Vec<(String, CHSType)>,
    pub ret_type: CHSType,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct ConstDecl {
    pub loc: Loc,
    pub name: String,
    pub value: ConstExpression,
    pub ttype: CHSType,
}

#[derive(Debug)]
pub struct TypeDecl(pub Loc, pub String, pub CHSType);

#[derive(Debug)]
pub struct Assign {
    pub loc: Loc,
    pub assined: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct IfExpression {
    pub loc: Loc,
    pub cond: Expression,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct IfElseExpression {
    pub loc: Loc,
    pub cond: Expression,
    pub body: Vec<Expression>,
    pub else_body: Vec<Expression>,
}

#[derive(Debug)]
pub struct WhileExpression {
    pub loc: Loc,
    pub cond: Expression,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub loc: Loc,
    pub name: String,
    pub value: Expression,
    pub ttype: Option<CHSType>,
}

#[derive(Debug)]
pub struct Call {
    pub loc: Loc,
    pub caller: Expression,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct Binop {
    pub loc: Loc,
    pub op: Operator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug)]
pub struct Unop {
    pub loc: Loc,
    pub op: Operator,
    pub left: Expression,
}

#[derive(Debug)]
pub enum Operator {
    // Binary
    Plus,
    Minus,
    Div,
    Mult,
    Eq,
    NEq,
    Gt,
    Lt,
    // Unary
    Negate,
    LNot,
    Refer,
    Deref,
}

impl Operator {
    pub fn from_token(token: &Token, unary: bool) -> Result<Self, CHSError> {
        use chs_lexer::TokenKind::*;
        match token.kind {
            Minus if unary => Ok(Self::Negate),
            Bang if unary => Ok(Self::LNot),
            Asterisk if unary => Ok(Self::Deref),
            Ampersand if unary => Ok(Self::Refer),
            Plus => Ok(Self::Plus),
            Minus => Ok(Self::Minus),
            Asterisk => Ok(Self::Mult),
            Slash => Ok(Self::Div),
            Eq => Ok(Self::Eq),
            NotEq => Ok(Self::NEq),
            _ => chs_error!("{} Unsuported operator", token.loc),
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            Operator::Plus | Operator::Minus => Precedence::Sum,
            Operator::Mult | Operator::Div => Precedence::Product,
            Operator::Lt | Operator::Gt => Precedence::LessGreater,
            Operator::Eq | Operator::NEq => Precedence::Equals,
            Operator::Negate | Operator::LNot => Precedence::Prefix,
            Operator::Refer | Operator::Deref => Precedence::Prefix,
            // _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
