use core::fmt;
use std::str::FromStr;

use crate::{
    chs_error,
    chs_lexer::{Loc, Span, Token, TokenKind},
    chs_types::CHSType,
    chs_util::{CHSError, CHSResult},
    return_chs_error,
};

use super::{ModuleImpl, RawModule};

#[derive(Debug)]
pub struct ASTModule<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<ModuleItem>,
}

impl<'src> ModuleImpl<'src> for ASTModule<'src> {
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
    Identifier(Span<String>),
    IntegerDefault(Span<u64>),
    IntegerLiteral(Span<u64>),
    UnsignedIntegerLiteral(Span<u64>),
    LongIntegerLiteral(Span<u64>),
    LongUnsignedIntegerLiteral(Span<u64>),
    BooleanLiteral(Span<bool>),
    StringLiteral(Span<String>),
    CharLiteral(Span<char>),
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

    fn span<T>(&self) -> Span<T> {
        match self {
            ConstExpression::Identifier(s) => s.to_span(),
            ConstExpression::IntegerDefault(s) => s.to_span(),
            ConstExpression::IntegerLiteral(s) => s.to_span(),
            ConstExpression::UnsignedIntegerLiteral(s) => s.to_span(),
            ConstExpression::LongIntegerLiteral(s) => s.to_span(),
            ConstExpression::LongUnsignedIntegerLiteral(s) => s.to_span(),
            ConstExpression::BooleanLiteral(s) => s.to_span(),
            ConstExpression::StringLiteral(s) => s.to_span(),
            ConstExpression::CharLiteral(s) => s.to_span(),
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
            Expression::MacroCall(MacroCall::TypeOf(token, ..)) => &token.loc,
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

    pub fn span<T>(&self) -> Span<T> {
        match self {
            Expression::MacroCall(e) => e.span(),
            Expression::ConstExpression(e) => e.span(),
            Expression::Binop(e) => e.token.source.to_span(),
            Expression::Unop(e) => e.token.source.to_span(),
            Expression::Call(e) => e.token.source.to_span(),
            Expression::Cast(e) => e.token.source.to_span(),
            Expression::Index(e) => e.token.source.to_span(),
            Expression::Syscall(e) => e.token.source.to_span(),
            Expression::VarDecl(e) => e.token.source.to_span(),
            Expression::Assign(e) => e.token.source.to_span(),
            Expression::Group(e) => e.span(),
            Expression::IfExpression(e) => e.token.source.to_span(),
            Expression::IfElseExpression(e) => e.token.source.to_span(),
            Expression::WhileExpression(e) => e.token.source.to_span(),
            Expression::ReturnExpression(e) => e.token.source.to_span(),
        }
    }
}

impl Expression {
    pub fn from_literal_token(token: Token) -> Result<Self, CHSError> {
        use TokenKind::*;
        match token.kind {
            Integer => Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                Span::from(token),
            ))),
            IntegerNumber => Ok(Self::ConstExpression(ConstExpression::IntegerLiteral(
                Span::from(token),
            ))),
            UnsignedIntegerNumber => Ok(Self::ConstExpression(
                ConstExpression::UnsignedIntegerLiteral(Span::from(token)),
            )),
            LongIntegerNumber => Ok(Self::ConstExpression(ConstExpression::LongIntegerLiteral(
                Span::from(token),
            ))),
            LongUnsignedIntegerNumber => Ok(Self::ConstExpression(
                ConstExpression::LongUnsignedIntegerLiteral(Span::from(token)),
            )),
            CharacterLiteral => Ok(Self::ConstExpression(ConstExpression::CharLiteral(
                Span::from(token),
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

    fn span<T>(&self) -> Span<T> {
        match self {
            MacroCall::Use(token, ..) => token.source.to_span(),
            MacroCall::TypeOf(token, ..) => token.source.to_span(),
        }
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

#[derive(Debug)]
pub struct Operator {
    pub span: Span<String>,
    pub kind: OperatorKind,
}
impl Operator {
    pub fn from_token(token: &Token, unary: bool) -> CHSResult<Self> {
        Ok(Self {
            kind: OperatorKind::from_token(token, unary)?,
            span: token.source.to_span(),
        })
    }

    pub fn precedence(&self) -> Precedence {
        self.kind.precedence()
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperatorKind {
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

impl fmt::Display for OperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Negate | Self::Minus => write!(f, "-"),
            Self::Div => write!(f, "/"),
            Self::Deref | Self::Mult => write!(f, "*"),
            Self::Mod => write!(f, "%"),
            Self::LOr => write!(f, "||"),
            Self::LAnd => write!(f, "&&"),
            Self::BitOr => write!(f, "|"),
            Self::Refer | Self::BitAnd => write!(f, "&"),
            Self::Eq => write!(f, "=="),
            Self::NEq => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::LNot => write!(f, "!"),
            Self::Assign => write!(f, "="),
            Self::Le => write!(f, "<="),
            Self::Ge => write!(f, ">="),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::BitXor => write!(f, "^"),
        }
    }
}

impl OperatorKind {
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
            Self::Plus | Self::Minus => Precedence::Sum,
            Self::Mult | Self::Div | Self::Mod => Precedence::Product,
            Self::Lt | Self::Le | Self::Ge | Self::Gt => Precedence::RelatonalGteLte,
            Self::Eq | Self::NEq => Precedence::Equals,
            Self::LOr => Precedence::LOr,
            Self::LAnd => Precedence::LAnd,
            Self::BitOr => Precedence::BitOr,
            Self::BitAnd => Precedence::BitAnd,
            Self::Negate | Self::LNot => Precedence::Prefix,
            Self::Refer | Self::Deref => Precedence::Prefix,
            Self::Assign => Precedence::Assign,
            Self::Shl | Self::Shr => Precedence::ShlShr,
            Self::BitXor => Precedence::BitXor,
            // _ => Precedence::Lowest,
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
