use core::fmt;
use std::str::FromStr;

use crate::{
    chs_error,
    // chs_lexer::{Loc, Span, Token, TokenKind},
    chs_types::CHSType,
    chs_util::{CHSError, CHSResult},
    return_chs_error,
};

use chslexer::*;

use super::RawModule;

#[derive(Debug)]
pub struct ASTModule<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<ModuleItem>,
}

impl<'src> ASTModule<'src> {
    pub fn imports(&self) -> Vec<(&Token, &ImportSyntax)> {
        self.items
            .iter()
            .filter_map(|item| match item {
                ModuleItem::MacroCall(MacroCall::Use(token, import_syntax)) => {
                    Some((token, import_syntax))
                }
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug)]
pub enum ModuleItem {
    Function(FunctionDecl),
    ExternFunction(ExternFunctionDecl),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub enum ConstExpression {
    Identifier(Token),
    IntegerDefault(Token),
    IntegerLiteral(Token),
    UnsignedIntegerLiteral(Token),
    LongIntegerLiteral(Token),
    LongUnsignedIntegerLiteral(Token),
    BooleanLiteral(Token),
    StringLiteral(Token),
    CharLiteral(Token),
}

impl ConstExpression {
    pub fn loc(&self) -> &Loc {
        match self {
            ConstExpression::Identifier(span) => &span.loc,
            ConstExpression::IntegerDefault(span) => &span.loc,
            ConstExpression::IntegerLiteral(span) => &span.loc,
            ConstExpression::UnsignedIntegerLiteral(span) => &span.loc,
            ConstExpression::LongIntegerLiteral(span) => &span.loc,
            ConstExpression::LongUnsignedIntegerLiteral(span) => &span.loc,
            ConstExpression::BooleanLiteral(span) => &span.loc,
            ConstExpression::StringLiteral(span) => &span.loc,
            ConstExpression::CharLiteral(span) => &span.loc,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    MacroCall(MacroCall),
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
    pub fn loc(&self) -> &Loc {
        match self {
            Expression::MacroCall(MacroCall::Use(token, ..)) => &token.loc,
            Expression::MacroCall(MacroCall::TypeOf(token, .. )) => &token.loc,
            Expression::ConstExpression(e) => e.loc(),
            Expression::Binop(e) => &e.token.loc,
            Expression::Unop(e) => &e.token.loc,
            Expression::Call(e) => &e.token.loc,
            Expression::Cast(e) => &e.token.loc,
            Expression::Index(e) => &e.token.loc,
            Expression::Syscall(e) => &e.token.loc,
            Expression::VarDecl(e) => &e.token.loc,
            Expression::Assign(e) => &e.token.loc,
            Expression::Group(e) => &e.loc(),
            Expression::IfExpression(e) => &e.token.loc,
            Expression::IfElseExpression(e) => &e.token.loc,
            Expression::WhileExpression(e) => &e.token.loc,
            Expression::ReturnExpression(e) => &e.token.loc,
        }
    }
}

impl Expression {
    pub fn from_literal_token(token: Token) -> Result<Self, CHSError> {
        use TokenKind::*;
        match token.kind {
            UnknowIntergerLiteral => Ok(Self::ConstExpression(ConstExpression::IntegerDefault(
                token,
            ))),
            IntegerNumber => Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                token,
            ))),
            U32Number => Ok(Self::ConstExpression(
                ConstExpression::UnsignedIntegerLiteral(token),
            )),
            I32Number => Ok(Self::ConstExpression(ConstExpression::LongIntegerLiteral(
                token,
            ))),
            U64Number => Ok(Self::ConstExpression(
                ConstExpression::LongUnsignedIntegerLiteral(token),
            )),
            CharacterLiteral => Ok(Self::ConstExpression(ConstExpression::CharLiteral(
                token,
            ))),
            _ => return_chs_error!("{} Unsupported literal", token.loc),
        }
    }
}

#[derive(Debug)]
pub enum MacroCall {
    Use(Token, ImportSyntax),
    TypeOf(Token, String),
}

impl MacroCall {
    pub fn macro_use(token: Token, import_syntax: ImportSyntax) -> Self {
        Self::Use(token, import_syntax)
    }
    pub fn macro_type_of(token: Token, arg: String) -> Self {
        Self::TypeOf(token, arg)
    }
}

#[derive(Debug)]
pub enum ImportSyntax {
    Logical(Vec<String>, Option<String>), // std::io [as name]
    Path(String, Option<String>),         // "std/io" [as name]
}

impl FromStr for ImportSyntax {
    type Err = CHSError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();

        // handle use!("std/io" as alias)
        if s.starts_with('"') && s.ends_with('"') {
            // Remove surrounding quotes
            let inner = &s[1..s.len() - 1];
            let (path, alias) = split_alias(inner)?;
            return Ok(ImportSyntax::Path(path.to_string(), alias));
        }

        let (path_part, alias) = split_alias(s)?;
        let parts = path_part
            .split("::")
            .map(|s| s.trim().to_string())
            .collect::<Vec<_>>();

        if parts.is_empty() {
            return Err(chs_error!("Empty logical module path"));
        }

        Ok(ImportSyntax::Logical(parts, alias))
    }
}

fn split_alias(s: &str) -> Result<(&str, Option<String>), CHSError> {
    let parts = s.splitn(2, " as ").collect::<Vec<_>>();
    let path = parts[0].trim();
    let alias = parts.get(1).map(|s| s.trim().to_string());

    if path.is_empty() {
        return Err(chs_error!("Import path cannot be empty"));
    }

    Ok((path, alias))
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Token,
    pub fn_type: CHSType,
    pub is_variadic: bool,
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
                    (CHSType::I32, CHSType::I32) => Ok(CHSType::I32),
                    (CHSType::U32, CHSType::U32) => Ok(CHSType::U32),
                    (CHSType::I64, CHSType::I64) => Ok(CHSType::I64),
                    (CHSType::U64, CHSType::U64) => Ok(CHSType::U64),
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
                (CHSType::I32, CHSType::I32) => Ok(CHSType::I32),
                (CHSType::U32, CHSType::U32) => Ok(CHSType::U32),
                (CHSType::I64, CHSType::I64) => Ok(CHSType::I64),
                (CHSType::U64, CHSType::U64) => Ok(CHSType::U64),
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
    Assign,
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
    Prefix,
    Call,
}
