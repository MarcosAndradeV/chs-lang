use chs_lexer::Token;
use chs_util::{chs_error, CHSError, CHSResult, Loc};

use chs_types::{CHSType, InferType, TypeEnv};

#[derive(Debug, Default)]
pub struct Module {
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub const_decls: Vec<ConstDecl>, // WTF?
}

#[derive(Debug)]
pub struct TypedModule {
    pub function_decls: Vec<FunctionDecl>,
}

impl TypedModule {
    pub fn from_module(m: Module) -> CHSResult<TypedModule> {
        let Module {
            type_decls,
            mut function_decls,
            const_decls: _,
        } = m;
        let mut env = TypeEnv::new(type_decls.iter().map(|t: &TypeDecl| (&t.1, &t.2)));

        for f in &mut function_decls {
            env.locals_new();
            env.locals_extend(f.args.iter().map(|(n, a)| (n, a)));
            if f.body.len() == 0 {
                let expect = &f.ret_type;
                let actual = CHSType::Void;
                if !expect.equivalent(&actual, &env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                continue;
            }
            let len = f.body.len() - 1;
            for (i, expr) in f.body.iter_mut().enumerate() {
                let t = expr.infer(&env)?;
                match expr {
                    Expression::VarDecl(v) => {
                        if v.ttype.is_none() {
                            v.ttype = Some(t);
                        }
                        env.locals_insert(&v.name, &v.ttype.as_ref().unwrap());
                    }
                    // TODO: Return() should be handle here like VarDecl.
                    _ => {
                        if i >= len {
                            let expect = &f.ret_type;
                            let actual = t;
                            if !expect.equivalent(&actual, &env) {
                                chs_error!(
                                    "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                                    expect,
                                    actual
                                );
                            }
                        }
                    }
                }
            }

            env.locals_pop();
        }

        Ok(TypedModule { function_decls })
    }
}

#[derive(Debug)]
pub enum ConstExpression {
    Symbol(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    Void,
}

impl chs_types::InferType for ConstExpression {
    fn infer(&self, env: &chs_types::TypeEnv) -> CHSResult<CHSType> {
        match self {
            ConstExpression::Symbol(sym) => match env.get(sym) {
                Some(t) => Ok((*t).clone()),
                None => chs_error!("Symbol not found {}", sym),
            },
            ConstExpression::IntegerLiteral(_) => Ok(CHSType::Int),
            ConstExpression::BooleanLiteral(_) => Ok(CHSType::Boolean),
            ConstExpression::StringLiteral(_) => todo!("string buildin type"),
            ConstExpression::Void => Ok(CHSType::Void),
        }
    }
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
    fn infer(&self, env: &chs_types::TypeEnv) -> CHSResult<CHSType> {
        match self {
            Expression::ExpressionList(_) => todo!("type infer for expression lists"),
            Expression::ConstExpression(e) => e.infer(env),
            Expression::Group(e) => e.infer(env),
            Expression::Call(call) => call.caller.infer(env)?.call(env, call.args.iter()),
            Expression::VarDecl(e) => {
                if let Some(ref expect) = e.ttype {
                    let actual = e.value.infer(env)?;
                    if !expect.equivalent(&actual, env) {
                        chs_error!(
                            "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                            expect,
                            actual
                        );
                    }
                    Ok(CHSType::Void)
                } else {
                    let infered = e.value.infer(env)?;
                    Ok(infered)
                }
            }
            Expression::Assign(e) => {
                let expect = e.assined.infer(env)?;
                let actual = e.value.infer(env)?;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                Ok(CHSType::Void)
            }
            Expression::IfExpression(e) => {
                let expect = e.cond.infer(env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                e.body.last().map_or(Ok(CHSType::Void), |t| t.infer(env))
            }
            Expression::IfElseExpression(e) => {
                let expect = e.cond.infer(env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                let expect = e.body.last().map_or(Ok(CHSType::Void), |t| t.infer(env))?;
                let actual = e.body.last().map_or(Ok(CHSType::Void), |t| t.infer(env))?;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                Ok(expect)
            }
            Expression::WhileExpression(e) => {
                let expect = e.cond.infer(env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                Ok(CHSType::Void)
            }
            Expression::Binop(e) => {
                let expect = e.left.infer(env)?;
                let actual = e.right.infer(env)?;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                        expect,
                        actual
                    );
                }
                match e.op {
                    Operator::Eq | Operator::NEq | Operator::Gt | Operator::Lt => {
                        Ok(CHSType::Boolean)
                    }
                    Operator::Plus | Operator::Minus | Operator::Div | Operator::Mult => Ok(expect),
                    _ => unreachable!("Not a binary operator"),
                }
            }
            Expression::Unop(e) => {
                let expect = e.left.infer(env)?;
                match e.op {
                    Operator::LNot => {
                        let actual = CHSType::Boolean;
                        if !expect.equivalent(&actual, env) {
                            chs_error!(
                                "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                                expect,
                                actual
                            );
                        }
                        return Ok(actual);
                    }
                    Operator::Negate => return Ok(expect),
                    Operator::Deref => {
                        if let CHSType::Pointer(ptr) = expect {
                            return Ok(*ptr);
                        } else {
                            chs_error!("Cannot deref `{:?}` type", expect)
                        }
                    }
                    Operator::Refer => Ok(CHSType::Pointer(Box::new(expect))),
                    _ => unreachable!("Not a unary operator"),
                }
            }
        }
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
