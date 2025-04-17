use chs_lexer::{Span, Token};
use chs_util::{return_chs_error, CHSError};
use std::fmt;

use chs_types::CHSType;

use crate::RawModule;

#[derive(Debug)]
pub struct Module<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug)]
pub enum ModuleItem {
    Function(FunctionDecl),
    ExternFunction(ExternFunctionDecl),
}

#[derive(Debug)]
pub enum ConstExpression {
    Identifier(Span<String>),
    IntegerLiteral(Span<i64>),
    BooleanLiteral(Span<bool>),
    StringLiteral(Span<String>),
    CharLiteral(Span<char>),
}

#[derive(Debug)]
pub enum Expression {
    ConstExpression(ConstExpression),
    Binop(Box<Binop>),
    Unop(Box<Unop>),
    Call(Box<Call>),
    Cast(Box<Cast>),
    Index(Box<Index>),
    Syscall(Box<Syscall>),
    VarDecl(Box<VarDecl>),
    Assign(Box<Assign>),
    Group(Box<Self>),
    IfExpression(Box<IfExpression>),
    IfElseExpression(Box<IfElseExpression>),
    WhileExpression(Box<WhileExpression>),
}

impl Expression {
    pub fn from_literal_token(token: Token) -> Result<Self, CHSError> {
        use chs_lexer::TokenKind::*;
        match token.kind {
            IntegerNumber => {
                Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                    Span::from(token)
                )))
            }
            CharacterLiteral => {
                Ok(Self::ConstExpression(ConstExpression::CharLiteral(
                    Span::from(token)
                )))
            }
            _ => return_chs_error!("{} Unsupported literal", token.loc),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Token,
    pub fn_type: CHSType,
    pub params: Vec<Param>,
    pub ret_type: CHSType,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct Param {
    pub name: Token,
    pub ty: CHSType,
}

#[derive(Debug)]
pub struct ExternFunctionDecl {
    pub name: Token,
    pub fn_type: CHSType,
}

#[derive(Debug)]
pub struct Assign {
    pub token: Token,
    pub target: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub cond: Expression,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct IfElseExpression {
    pub token: Token,
    pub cond: Expression,
    pub body: Vec<Expression>,
    pub else_body: Vec<Expression>,
}

#[derive(Debug)]
pub struct WhileExpression {
    pub token: Token,
    pub cond: Expression,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub token: Token,
    pub value: Expression,
    pub ty: Option<CHSType>,
}

#[derive(Debug)]
pub struct Cast {
    pub token: Token,
    pub to_type: CHSType,
    pub casted: Expression,
}

#[derive(Debug)]
pub struct Index {
    pub token: Token,
    pub base: Expression,
    pub index: Expression,
}

#[derive(Debug)]
pub struct Call {
    pub token: Token,
    pub callee: Expression,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct Syscall {
    pub token: Token,
    pub arity: usize,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct Binop {
    pub token: Token,
    pub op: Operator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug)]
pub struct Unop {
    pub token: Token,
    pub op: Operator,
    pub operand: Expression,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    // Binary
    Plus,
    Minus,
    Div,
    Mult,
    Mod,
    LOr,
    LAnd,
    Or,
    And,
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Negate | Operator::Minus => write!(f, "-"),
            Operator::Div => write!(f, "/"),
            Operator::Deref | Operator::Mult => write!(f, "*"),
            Operator::Mod => write!(f, "%"),
            Operator::LOr => write!(f, "||"),
            Operator::LAnd => write!(f, "&&"),
            Operator::Or => write!(f, "|"),
            Operator::Refer | Operator::And => write!(f, "&"),
            Operator::Eq => write!(f, "=="),
            Operator::NEq => write!(f, "!="),
            Operator::Gt => write!(f, ">"),
            Operator::Lt => write!(f, "<"),
            Operator::LNot => write!(f, "!"),
        }
    }
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
            Gt => Ok(Self::Gt),
            Lt => Ok(Self::Lt),
            Mod => Ok(Self::Mod),
            DoubleAmpersand => Ok(Self::LAnd),
            DoublePipe => Ok(Self::LOr),
            Ampersand => Ok(Self::Or),
            Pipe => Ok(Self::And),
            _ => return_chs_error!("{} Unsupported operator", token.loc),
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            Operator::Plus | Operator::Minus => Precedence::Sum,
            Operator::Mult | Operator::Div | Operator::Mod => Precedence::Product,
            Operator::Lt | Operator::Gt => Precedence::RelatonalGteLte,
            Operator::Eq | Operator::NEq => Precedence::Equals,
            Operator::LOr => Precedence::LOr,
            Operator::LAnd => Precedence::LAnd,
            Operator::Or => Precedence::Or,
            Operator::And => Precedence::And,
            Operator::Negate | Operator::LNot => Precedence::Prefix,
            Operator::Refer | Operator::Deref => Precedence::Prefix,
            // _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    LOr,
    LAnd,
    Or,
    Xor,
    And,
    Equals,
    RelatonalGteLte,
    BitWise,
    Sum,
    Product,
    Prefix,
    Call,
}
