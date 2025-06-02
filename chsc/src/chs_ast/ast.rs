use core::fmt;

use chslexer::Token;

use crate::chs_types::CHSType;

use super::typechecker::CHSInfer;

pub type Ast = Vec<AstNode>;

#[derive(Debug)]
pub enum AstNode {
    FunctionDecl(FunctionDecl),
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Identifier,
    pub parameters: Params,
    pub return_type: Option<CHSType>,
    pub body: Statement,
}

#[derive(Debug)]
pub struct StructDecl {
    pub name: Identifier,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: Identifier,
    pub type_: CHSType,
}

#[derive(Debug)]
pub struct EnumDecl {
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Identifier,
    pub fields: Option<Vec<CHSType>>,
}

pub type Params = Vec<Param>;
pub type Identifier = Token;

#[derive(Debug)]
pub struct Param {
    pub name: Identifier,
    pub type_: CHSType,
}

#[derive(Debug)]
pub enum Statement {
    ReturnStatement(ReturnStatement),
    BlockStatement(Vec<Self>),
    LetStatement(LetStatement),
    ExpressionStatement(Expression),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    AssignmentStatement(AssignmentStatement),
    Empty,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
    pub else_body: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct AssignmentStatement {
    pub target: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub enum ReturnStatement {
    Return,
    ReturnExpression(Expression),
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    pub type_: Option<CHSType>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,
    Assignment = 1,     // =, +=, -=, etc.
    LogicalOr = 2,      // ||
    LogicalAnd = 3,     // &&
    BitwiseOr = 4,      // |
    BitwiseXor = 5,     // ^
    BitwiseAnd = 6,     // &
    Equality = 7,       // ==, !=
    Comparison = 8,     // <, >, <=, >=
    Shift = 9,          // <<, >>
    Addition = 10,      // +, -
    Multiplication = 11, // *, /, %
    Unary = 12,         // !, -, +, ~, &, *
    Call = 13,          // function calls, array access, member access
    Primary = 14,       // literals, identifiers, parentheses
}

impl Precedence {
    pub fn from_binary_op(op: &BinaryOperator) -> Self {
        match op {
            BinaryOperator::Assign => Precedence::Assignment,
            BinaryOperator::LogicalOr => Precedence::LogicalOr,
            BinaryOperator::LogicalAnd => Precedence::LogicalAnd,
            BinaryOperator::BitwiseOr => Precedence::BitwiseOr,
            BinaryOperator::BitwiseXor => Precedence::BitwiseXor,
            BinaryOperator::BitwiseAnd => Precedence::BitwiseAnd,
            BinaryOperator::Equal | BinaryOperator::NotEqual => Precedence::Equality,
            BinaryOperator::Less | BinaryOperator::Greater |
            BinaryOperator::LessEqual | BinaryOperator::GreaterEqual => Precedence::Comparison,
            BinaryOperator::LeftShift | BinaryOperator::RightShift => Precedence::Shift,
            BinaryOperator::Add | BinaryOperator::Subtract => Precedence::Addition,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => Precedence::Multiplication,
        }
    }

    pub fn from_unary_op(_op: &UnaryOperator) -> Self {
        Precedence::Unary
    }

    pub fn is_right_associative(&self) -> bool {
        matches!(self, Precedence::Assignment | Precedence::Unary)
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // Comparison
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Logical
    LogicalAnd,
    LogicalOr,

    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,

    // Assignment
    Assign,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    AddressOf,
    Dereference,
}

#[derive(Debug)]
pub enum Expression {
    // Literals
    Nil,
    Bool(bool),
    Int(Token),
    Float(Token),
    String(Token),
    Char(Token),

    // Identifiers
    Identifier(Identifier),

    // Operations
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },

    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },

    // Function calls and access
    FunctionCall {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },

    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },

    MemberAccess {
        object: Box<Expression>,
        member: Identifier,
    },

    // Other expressions
    ArrayLiteral(Vec<Expression>),
    StructLiteral {
        name: Identifier,
        fields: Vec<StructFieldInit>,
    },

    TernaryOp {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },

    Parenthesized(Box<Expression>),

    // Type casting
    Cast {
        expr: Box<Expression>,
        target_type: CHSType,
    },
}

impl fmt::Display for Expression {
    #[allow(unused)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Nil => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::Int(token) => todo!(),
            Expression::Float(token) => todo!(),
            Expression::String(token) => todo!(),
            Expression::Char(token) => todo!(),
            Expression::Identifier(token) => write!(f, "{}", token.source),
            Expression::BinaryOp { left, operator, right } => todo!(),
            Expression::UnaryOp { operator, operand } => todo!(),
            Expression::FunctionCall { callee, args } => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::MemberAccess { object, member } => write!(f, "{object}.{}", member.source),
            Expression::ArrayLiteral(vec) => todo!(),
            Expression::StructLiteral { name, fields } => todo!(),
            Expression::TernaryOp { condition, true_expr, false_expr } => todo!(),
            Expression::Parenthesized(expression) => todo!(),
            Expression::Cast { expr, target_type } => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct StructFieldInit {
    pub name: Identifier,
    pub value: Expression,
}

impl Expression {
    pub fn precedence(&self) -> Precedence {
        match self {
            Expression::BinaryOp { operator, .. } => Precedence::from_binary_op(operator),
            Expression::UnaryOp { operator, .. } => Precedence::from_unary_op(operator),
            Expression::FunctionCall { .. } |
            Expression::ArrayAccess { .. } |
            Expression::MemberAccess { .. } => Precedence::Call,
            Expression::TernaryOp { .. } => Precedence::Assignment, // Ternary has same precedence as assignment
            _ => Precedence::Primary,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        matches!(self, Expression::BinaryOp { .. })
    }

    pub fn is_unary_op(&self) -> bool {
        matches!(self, Expression::UnaryOp { .. })
    }

    pub fn is_literal(&self) -> bool {
        matches!(self,
            Expression::Nil |
            Expression::Bool(_) |
            Expression::Int(_) |
            Expression::Float(_) |
            Expression::String(_) |
            Expression::Char(_) |
            Expression::ArrayLiteral(_) |
            Expression::StructLiteral { .. }
        )
    }
}

// Helper functions for precedence parsing
impl BinaryOperator {
    pub fn is_left_associative(&self) -> bool {
        !matches!(self, BinaryOperator::Assign)
    }

    pub fn is_comparison_op(&self) -> bool {
        matches!(self,
            BinaryOperator::Equal | BinaryOperator::NotEqual |
            BinaryOperator::Less | BinaryOperator::Greater |
            BinaryOperator::LessEqual | BinaryOperator::GreaterEqual
        )
    }

    pub fn is_arithmetic_op(&self) -> bool {
        matches!(self,
            BinaryOperator::Add | BinaryOperator::Subtract |
            BinaryOperator::Multiply | BinaryOperator::Divide |
            BinaryOperator::Modulo
        )
    }
}

impl UnaryOperator {
    pub fn is_prefix(&self) -> bool {
        true
    }
}

impl CHSInfer for Expression {
    fn infer(&self) -> CHSType {
        todo!()
    }

    fn cast(&mut self, _ty: CHSType) -> crate::chs_util::CHSResult<()> {
        todo!()
    }
}
