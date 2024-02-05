use std::fmt;

use crate::chs_vm::value::Value;

#[derive(Debug)]
pub enum Operation {
    Pop,
    Dup,
    Dup2,
    Swap,
    Over,

    Add,
    Minus,
    Mul,
    Div,
    Mod,

    Eq,
    Neq,
    Gt,
    Gte,
    Lte,
    Lt,

    Land,
    Lor,

    Shl,
    Shr,
    Bitand,
    Bitor,
}

#[derive(Debug)]
pub enum BuildinOp {
    IdxGet,
    IdxSet,
    Len,
    Println,
    Print,
    Debug,
    Fill,
    ReadLine,
    Range,
    Exit,
}

impl From<&BuildinOp> for usize {
    fn from(value: &BuildinOp) -> Self {
        match value {
            BuildinOp::IdxGet => 0,
            BuildinOp::IdxSet => 1,
            BuildinOp::Len => 2,
            BuildinOp::Println => 3,
            BuildinOp::Print => 4,
            BuildinOp::Debug => 5,
            BuildinOp::Fill => 6,
            BuildinOp::ReadLine => 7,
            BuildinOp::Range => 8,
            BuildinOp::Exit => 9,
        }
    }
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Expressions,
    pub if_branch: Expressions,
    pub else_branch: Option<Expressions>,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub cond: Expressions,
    pub while_block: Expressions,
}

#[derive(Debug)]
pub struct VarExpr {
    pub name: String,
    pub value: Expressions,
}

#[derive(Debug)]
pub struct PeekExpr {
    pub names: Vec<String>,
    pub body: Expressions,
}

#[derive(Debug)]
pub struct FnExpr {
    pub name: String,
    pub body: Expressions,
}

#[derive(Debug)]
pub enum Expr {
    Op(Box<Operation>),
    Buildin(Box<BuildinOp>),
    IntExpr(Box<String>),
    StrExpr(Box<String>),
    BoolExpr(Box<String>),
    ListExpr(Box<Vec<Value>>),
    IdentExpr(Box<String>),
    Assigin(Box<String>),
    NilExpr,
    If(Box<IfExpr>),
    Whlie(Box<WhileExpr>),
    Var(Box<VarExpr>),
    Peek(Box<PeekExpr>),
    Fn(Box<FnExpr>),
    Set,
    IndexExpr,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Op(_) => write!(f, "Op"),
            Expr::Buildin(_) => write!(f, "Buildin"),
            Expr::IntExpr(_) => write!(f, "IntExpr"),
            Expr::StrExpr(_) => write!(f, "StrExpr"),
            Expr::BoolExpr(_) => write!(f, "BoolExpr"),
            Expr::NilExpr => write!(f, "NilExpr"),
            Expr::If(_) => write!(f, "If"),
            Expr::Whlie(_) => write!(f, "Whlie"),
            Expr::Var(_) => write!(f, "Var"),
            Expr::Peek(_) => write!(f, "Peek"),
            Expr::ListExpr(_) => write!(f, "ListExpr"),
            Expr::IdentExpr(_) => write!(f, "Identifier"),
            Expr::Assigin(_) => write!(f, "Assigin"),
            Expr::Set => write!(f, "Set"),
            Expr::IndexExpr => write!(f, "IndexExpr"),
            Expr::Fn(_) => write!(f, "Fn"),
        }
    }
}

type Expressions = Vec<Expr>;
pub struct Program {
    pub exprs: Expressions,
}

impl IntoIterator for Program {
    type Item = Expr;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.exprs.into_iter()
    }
}