use core::fmt;

use crate::{
    chs_lexer::{Span, Token, TokenKind},
    chs_types::CHSType,
    chs_util::{CHSError, CHSResult},
    return_chs_error,
};

use super::RawModule;

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
    ReturnExpression(Box<ReturnExpression>),
}

impl Expression {
    pub fn from_literal_token(token: Token) -> Result<Self, CHSError> {
        use TokenKind::*;
        match token.kind {
            IntegerNumber => Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                Span::from(token),
            ))),
            CharacterLiteral => Ok(Self::ConstExpression(ConstExpression::CharLiteral(
                Span::from(token),
            ))),
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
pub struct ReturnExpression {
    pub token: Token,
    pub expr: Option<Expression>,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    // Binary
    Plus,
    Minus,
    Div,
    Mult,
    Mod,
    LOr,
    LAnd,
    BitOr,
    BitAnd,
    Eq,
    NEq,
    Gt,
    Lt,
    Assign,
    Le,
    Ge,
    Shl,
    Shr,
    BitXor,
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
            Operator::BitOr => write!(f, "|"),
            Operator::Refer | Operator::BitAnd => write!(f, "&"),
            Operator::Eq => write!(f, "=="),
            Operator::NEq => write!(f, "!="),
            Operator::Gt => write!(f, ">"),
            Operator::Lt => write!(f, "<"),
            Operator::LNot => write!(f, "!"),
            Operator::Assign => write!(f, "="),
            Operator::Le => write!(f, "<="),
            Operator::Ge => write!(f, ">="),
            Operator::Shl => write!(f, "<<"),
            Operator::Shr => write!(f, ">>"),
            Operator::BitXor => write!(f, "^"),
        }
    }
}

impl Operator {
    pub fn from_token(token: &Token, unary: bool) -> Result<Self, CHSError> {
        use TokenKind::*;
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
            Ampersand => Ok(Self::BitOr),
            Pipe => Ok(Self::BitAnd),
            Assign => Ok(Self::Assign),
            _ => return_chs_error!("{} Unsupported operator", token.loc),
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            Operator::Plus | Operator::Minus => Precedence::Sum,
            Operator::Mult | Operator::Div | Operator::Mod => Precedence::Product,
            Operator::Lt | Operator::Le | Operator::Ge | Operator::Gt => {
                Precedence::RelatonalGteLte
            }
            Operator::Eq | Operator::NEq => Precedence::Equals,
            Operator::LOr => Precedence::LOr,
            Operator::LAnd => Precedence::LAnd,
            Operator::BitOr => Precedence::BitOr,
            Operator::BitAnd => Precedence::BitAnd,
            Operator::Negate | Operator::LNot => Precedence::Prefix,
            Operator::Refer | Operator::Deref => Precedence::Prefix,
            Operator::Assign => Precedence::Assign,
            Operator::Shl | Operator::Shr => Precedence::ShlShr,
            Operator::BitXor => Precedence::BitXor,
            // _ => Precedence::Lowest,
        }
    }

    pub fn get_type_of_op(&self, lty: &CHSType, rty: &CHSType) -> CHSResult<CHSType> {
        match self {
            // Arithmetic operators
            Operator::Plus | Operator::Minus | Operator::Mult | Operator::Div | Operator::Mod => {
                // Handle pointer arithmetic
                match (lty, rty) {
                    (CHSType::Pointer(inner), CHSType::Int)
                    | (CHSType::Int, CHSType::Pointer(inner)) => {
                        // Pointer + int or int + pointer results in a pointer
                        Ok(CHSType::Pointer(inner.clone()))
                    }
                    (CHSType::Pointer(inner1), CHSType::Pointer(inner2)) => {
                        // Pointer - pointer results in an integer (offset)
                        if inner1 == inner2 {
                            Ok(CHSType::Int)
                        } else {
                            return_chs_error!("Cannot subtract pointers of different types")
                        }
                    }
                    // Regular numeric arithmetic
                    (CHSType::Int, CHSType::Int) => Ok(CHSType::Int),
                    (CHSType::UInt, CHSType::UInt) => Ok(CHSType::UInt),
                    _ => return_chs_error!(
                        "Arithmetic operators require numeric types of the same kind"
                    ),
                }
            }
            // Comparison operators
            Operator::Lt
            | Operator::Le
            | Operator::Gt
            | Operator::Ge
            | Operator::Eq
            | Operator::NEq => {
                if lty == rty {
                    Ok(CHSType::Boolean)
                } else {
                    return_chs_error!("Comparison operators require operands of the same type")
                }
            }
            // Logical operators
            Operator::LAnd | Operator::LOr => {
                if *lty == CHSType::Boolean && *rty == CHSType::Boolean {
                    Ok(CHSType::Boolean)
                } else {
                    return_chs_error!("Logical operators require boolean operands")
                }
            }
            // Bitwise operators
            Operator::BitAnd
            | Operator::BitOr
            | Operator::BitXor
            | Operator::Shl
            | Operator::Shr => match (lty, rty) {
                (CHSType::Int, CHSType::Int) => Ok(CHSType::Int),
                (CHSType::UInt, CHSType::UInt) => Ok(CHSType::UInt),
                _ => {
                    return_chs_error!("Bitwise operators require numeric types of the same kind")
                }
            },
            // Unary operators (should not be used in binary expressions)
            Operator::Negate | Operator::LNot | Operator::Refer | Operator::Deref => {
                return_chs_error!("Unary operator used in binary expression")
            }
            Operator::Assign => {
                unreachable!("Assignment should be handled by Assign Expression")
            }
        }
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    LOr,
    LAnd,
    BitOr,
    BitXor,
    BitAnd,
    Equals,
    RelatonalGteLte,
    ShlShr,
    Sum,
    Product,
    Assign,
    Prefix,
    Call,
}
