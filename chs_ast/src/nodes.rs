use std::{fmt, path::PathBuf};

use chs_lexer::Token;
use chs_util::{chs_error, CHSError, CHSResult, Loc};

use chs_types::{CHSType, InferType, TypeEnv, TypeMap};

#[derive(Debug, Default)]
pub struct Module {
    pub file_path: PathBuf,
    pub imported_modules: Vec<UseModuleDecl>,
    pub type_decls: Vec<TypeDecl>,
    pub global_decls: Vec<GlobalDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub const_decls: Vec<ConstDecl>, // WTF?
}

#[derive(Debug)]
pub struct TypedModule {
    pub file_path: PathBuf,
    pub function_decls: Vec<FunctionDecl>,
    pub type_defs: TypeMap,
}

impl TypedModule {
    pub fn from_module(m: Module) -> CHSResult<TypedModule> {
        let Module {
            file_path,
            imported_modules: _,
            type_decls,
            global_decls,
            mut function_decls,
            const_decls: _,
        } = m;
        // TODO: Look for type redefinition
        let mut env = TypeEnv::new();
        for decl in &type_decls {
            if env.type_decls_insert(&decl.name, &decl.ttype).is_some() {
                chs_error!("{} Redefinition of {}", decl.loc, decl.name)
            }
        }
        let var_name = "print_int".to_string();
        let function = CHSType::Function(vec![CHSType::Int], Box::new(CHSType::Void));
        env.globals_insert(&var_name, &function);

        for decl in &global_decls {
            if env.globals_insert(&decl.name, &decl.ttype).is_some() {
                chs_error!("{} Redefinition of {}", decl.loc, decl.name)
            }
        }

        for f in &mut function_decls {
            env.locals_new();
            env.locals_extend(f.args.iter().map(|(n, a)| (n, a)));
            if f.body.is_empty() {
                let expect = &f.ret_type;
                let actual = CHSType::Void;
                if !expect.equivalent(&actual, &env) {
                    chs_error!(
                        "Return type mismatch. Expect: {}  Actual: {}",
                        expect,
                        actual
                    );
                }
                continue;
            }
            let len = f.body.len() - 1;
            for (i, expr) in f.body.iter_mut().enumerate() {
                if i >= len {
                    let t = expr.infer(Some(&f.ret_type), &mut env)?;
                    let expect = &f.ret_type;
                    let actual = t;
                    if !expect.equivalent(&actual, &env) {
                        chs_error!(
                            "Return type mismatch. Expect: {}  Actual: {}",
                            expect,
                            actual
                        );
                    }
                } else {
                    expr.infer(None, &mut env)?;
                }
            }
            env.locals_pop();
        }

        let type_defs = env.into_type_defs();
        Ok(TypedModule {
            file_path,
            function_decls,
            type_defs,
        })
    }
}

#[derive(Debug)]
pub enum ConstExpression {
    Symbol(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    CharLiteral(u8),
    Void,
}

impl chs_types::InferType for ConstExpression {
    fn infer(
        &mut self,
        hint: Option<&CHSType>,
        env: &mut chs_types::TypeEnv,
    ) -> CHSResult<CHSType> {
        _ = hint;
        match self {
            ConstExpression::Symbol(sym) => match env.get(sym) {
                Some(t) => Ok((*t).clone()),
                None => chs_error!("variable not found {}", sym),
            },
            ConstExpression::IntegerLiteral(_) => Ok(CHSType::Int),
            ConstExpression::BooleanLiteral(_) => Ok(CHSType::Boolean),
            ConstExpression::StringLiteral(_) => Ok(CHSType::String),
            ConstExpression::CharLiteral(_) => Ok(CHSType::Char),
            ConstExpression::Void => Ok(CHSType::Void),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    ConstExpression(ConstExpression),
    ExpressionList(ExpressionList),
    Binop(Box<Binop>),
    Unop(Box<Unop>),
    Call(Box<Call>),
    Cast(Box<Cast>),
    Index(Box<Index>),
    Syscall(Box<Syscall>),
    Array(Box<Array>),
    Len(Box<Expression>),
    VarDecl(Box<VarDecl>),
    Assign(Box<Assign>),
    Group(Box<Self>),
    IfExpression(Box<IfExpression>),
    IfElseExpression(Box<IfElseExpression>),
    WhileExpression(Box<WhileExpression>),
}

impl chs_types::InferType for Expression {
    fn infer<'a>(
        &'a mut self,
        hint: Option<&CHSType>,
        env: &mut chs_types::TypeEnv<'a>,
    ) -> CHSResult<CHSType> {
        match self {
            Expression::ExpressionList(e) => {
                chs_error!(
                    "{} Cannot infer the type of expression list. Not implemented yet :(",
                    e.loc
                )
            }
            Expression::ConstExpression(e) => e.infer(hint, env),
            Expression::Group(e) => e.infer(hint, env),
            Expression::Cast(e) => {
                e.casted.infer(Some(&e.ttype), env)?;
                Ok(e.ttype.clone())
            }
            Expression::Index(e) => {
                let actual = e.left.infer(None, env)?;
                if actual.is_void_pointer() {
                    chs_error!("{} Cannot deref a *void", e.loc)
                }
                if !actual.is_pointer() {
                    chs_error!("TDO")
                }

                let index = e.index.infer(None, env)?;
                if !index.equivalent(&CHSType::Int, env) {
                    chs_error!("TDO")
                }

                if let CHSType::Pointer(ptr) = actual {
                    e.ttype = Some(*ptr.clone());
                    Ok(*ptr)
                } else {
                    chs_error!("TDO")
                }
            }
            Expression::Syscall(e) => {
                if e.arity > 7 && e.arity == 0 {
                    chs_error!("{} Syscall arity mismatch", e.loc);
                }
                for (i, actual) in e.args.iter_mut().enumerate() {
                    let actual = actual.infer(hint, env)?;
                    if i == 0 {
                        let expect = CHSType::Int;
                        if !expect.equivalent(&actual, env) {
                            chs_error!(
                                "{} Syscall first argument type mismatch. Expect: {}  Actual: {}",
                                e.loc,
                                expect,
                                actual
                            );
                        }
                    }
                }
                Ok(CHSType::Int)
            }
            Expression::Call(call) => match call.caller.infer(hint, env)? {
                CHSType::Function(ref mut fn_args, ret_type) => {
                    if fn_args.len() != call.args.len() {
                        chs_error!("{} Arity mismatch function call", call.loc);
                    }
                    for (expect, actual) in fn_args.iter_mut().zip(call.args.iter_mut()) {
                        let actual = actual.infer(hint, env)?;
                        if !expect.equivalent(&actual, env) {
                            chs_error!(
                                "{} Argument type mismatch. Expect: {}  Actual: {}",
                                call.loc,
                                expect,
                                actual
                            );
                        }
                    }
                    Ok(*ret_type.clone())
                }
                c => chs_error!("{} Cannot call {}", call.loc, c),
            },
            Expression::Len(e) => {
                let arg = e.infer(hint, env)?;
                match arg {
                    CHSType::String => {}
                    arg => chs_error!("Cannot get the len of {}", arg),
                }
                Ok(CHSType::Int)
            }
            Expression::Array(e) => {
                assert!(e.size > 0);
                Ok(CHSType::Pointer(Box::new(e.ttype.clone())))
            }
            Expression::VarDecl(e) => {
                if let Some(ref expect) = e.ttype {
                    let actual = e.value.infer(Some(expect), env)?;
                    if !expect.equivalent(&actual, env) {
                        chs_error!(
                            "{} Type of variable `{}` not match. Expect: {} Actual: {}",
                            e.loc,
                            e.name,
                            expect,
                            actual
                        );
                    }
                } else {
                    e.ttype = Some(e.value.infer(None, env)?);
                }
                env.locals_insert(&e.name, e.ttype.as_ref().unwrap());
                Ok(CHSType::Void)
            }
            Expression::Assign(e) => {
                let expect = e.assigned.infer(hint, env)?;
                let actual = e.value.infer(hint, env)?;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "{} Argument type mismatch. Expect: {}  Actual: {}",
                        e.loc,
                        expect,
                        actual
                    );
                }
                e.ttype = Some(expect);
                Ok(CHSType::Void)
            }
            Expression::IfExpression(e) => {
                let expect = e.cond.infer(hint, env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "{} Argument type mismatch. Expect: {}  Actual: {}",
                        e.loc,
                        expect,
                        actual
                    );
                }
                env.locals_new();
                for expr in &mut e.body {
                    expr.infer(hint, env)?;
                }
                env.locals_pop();
                Ok(CHSType::Void)
            }
            Expression::IfElseExpression(e) => {
                let expect = e.cond.infer(hint, env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "{} Argument type mismatch. Expect: {}  Actual: {}",
                        e.loc,
                        expect,
                        actual
                    );
                }
                env.locals_new();
                for expr in &mut e.body {
                    expr.infer(hint, env)?;
                }
                env.locals_pop();
                env.locals_new();
                for expr in &mut e.else_body {
                    expr.infer(hint, env)?;
                }
                env.locals_pop();
                Ok(CHSType::Void)
            }
            Expression::WhileExpression(e) => {
                let expect = e.cond.infer(hint, env)?;
                let actual = CHSType::Boolean;
                if !expect.equivalent(&actual, env) {
                    chs_error!(
                        "{} Argument type mismatch. Expect: {}  Actual: {}",
                        e.loc,
                        expect,
                        actual
                    );
                }
                env.locals_new();
                for expr in &mut e.body {
                    expr.infer(hint, env)?;
                }
                env.locals_pop();
                Ok(CHSType::Void)
            }
            Expression::Binop(e) => match e.op {
                Operator::Eq | Operator::NEq | Operator::Gt | Operator::Lt => {
                    let left = e.left.infer(hint, env)?;
                    let right = e.right.infer(hint, env)?;
                    if !left.equivalent(&right, env) {
                        chs_error!(
                            "{} Argument type mismatch. Expect: {}  Actual: {}",
                            e.loc,
                            left,
                            right
                        );
                    }
                    e.ttype = Some(left);
                    Ok(CHSType::Boolean)
                }
                Operator::LAnd | Operator::LOr => {
                    let left = e.left.infer(hint, env)?;
                    let right = e.right.infer(hint, env)?;
                    if !left.equivalent(&right, env) {
                        chs_error!("{} {} {} {} is not defined.", e.loc, left, e.op, right);
                    }
                    e.ttype = Some(CHSType::Boolean);
                    Ok(CHSType::Boolean)
                }
                Operator::Plus | Operator::Minus => {
                    let left = e.left.infer(hint, env)?;
                    let right = e.right.infer(hint, env)?;
                    match (&left, &right) {
                        (CHSType::Pointer(..), CHSType::Pointer(..)) => {
                            chs_error!("{} {} {} {} is not defined.", e.loc, left, e.op, right);
                        }
                        (CHSType::Int, CHSType::Pointer(..)) => {
                            e.ttype = Some(right.clone());
                            Ok(right)
                        }
                        (CHSType::Pointer(..), CHSType::Int) => {
                            e.ttype = Some(left.clone());
                            Ok(left)
                        }
                        (CHSType::String, CHSType::Int) => {
                            e.ttype = Some(CHSType::Pointer(Box::new(CHSType::Char)));
                            Ok(CHSType::Pointer(Box::new(CHSType::Char)))
                        }
                        (CHSType::Int, CHSType::String) => {
                            e.ttype = Some(CHSType::Pointer(Box::new(CHSType::Char)));
                            Ok(CHSType::Pointer(Box::new(CHSType::Char)))
                        }
                        (CHSType::Int, CHSType::Int) => {
                            e.ttype = Some(left.clone());
                            Ok(left)
                        }
                        (CHSType::UInt, CHSType::UInt) => {
                            e.ttype = Some(left.clone());
                            Ok(left)
                        }
                        _ => chs_error!("{} {} {} {}  is not defined.", e.loc, left, e.op, right),
                    }
                }
                Operator::Div | Operator::Mod | Operator::Mult => {
                    let left = e.left.infer(hint, env)?;
                    let right = e.right.infer(hint, env)?;
                    match (&left, &right) {
                        (a, b) if a.is_pointer() || b.is_pointer() => {
                            chs_error!("{} {} {:?} {}  is not defined.", e.loc, left, e.op, right)
                        }
                        (CHSType::Int, CHSType::Int) => {
                            e.ttype = Some(left.clone());
                            Ok(left)
                        }
                        (CHSType::UInt, CHSType::UInt) => {
                            e.ttype = Some(left.clone());
                            Ok(left)
                        }
                        _ => chs_error!("{} {} {} {}  is not defined.", e.loc, left, e.op, right),
                    }
                }
                Operator::Or => chs_error!("{} {} is not defined.", e.loc, e.op),
                Operator::And => chs_error!("{} {} is not defined.", e.loc, e.op),
                Operator::Negate | Operator::LNot | Operator::Refer | Operator::Deref => {
                    unreachable!("Not a binary operator")
                }
            },
            Expression::Unop(e) => {
                let expect = e.left.infer(hint, env)?;
                match e.op {
                    Operator::LNot => {
                        let actual = CHSType::Boolean;
                        if !expect.equivalent(&actual, env) {
                            chs_error!(
                                "{} Argument type mismatch. Expect: {}  Actual: {}",
                                e.loc,
                                expect,
                                actual
                            );
                        }
                        e.ttype = Some(actual.clone());
                        Ok(actual)
                    }
                    Operator::Negate => {
                        e.ttype = Some(expect.clone());
                        Ok(expect)
                    }
                    Operator::Deref => {
                        if let CHSType::Pointer(ptr) = expect {
                            e.ttype = Some(*ptr.clone());
                            Ok(*ptr)
                        } else {
                            chs_error!("Cannot deref `{}` type", expect)
                        }
                    }
                    Operator::Refer => {
                        e.ttype = Some(expect.clone());
                        Ok(CHSType::Pointer(Box::new(expect)))
                    }
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
            Integer => {
                let value: i64 = token
                    .value
                    .parse::<i64>()
                    .expect("No integer token. Probably a lexer error.");
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
            Character => {
                let value: char = token
                    .value
                    .parse::<char>()
                    .expect("No char token. Probably a lexer error.");
                Ok(Self::ConstExpression(ConstExpression::CharLiteral(
                    value as u8,
                )))
            }
            _ => chs_error!("{} Unsupported literal", token.loc),
        }
    }

    pub fn is_expression_list(&self) -> bool {
        matches!(self, Self::ExpressionList(..))
    }
}

#[derive(Debug)]
pub struct UseModuleDecl {
    pub loc: Loc,
    pub path: PathBuf,
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
pub struct TypeDecl {
    pub loc: Loc,
    pub name: String,
    pub ttype: CHSType,
}

#[derive(Debug)]
pub struct GlobalDecl {
    pub loc: Loc,
    pub name: String,
    pub ttype: CHSType,
}

#[derive(Debug)]
pub struct Assign {
    pub loc: Loc,
    pub assigned: Expression,
    pub value: Expression,
    pub ttype: Option<CHSType>,
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
pub struct Cast {
    pub loc: Loc,
    pub ttype: CHSType,
    pub casted: Expression,
}

#[derive(Debug)]
pub struct Index {
    pub loc: Loc,
    pub left: Expression,
    pub index: Expression,
    pub ttype: Option<CHSType>,
}

#[derive(Debug)]
pub struct ExpressionList {
    pub loc: Loc,
    pub exprs: Vec<Expression>,
    pub ttype: Option<CHSType>,
}

#[derive(Debug)]
pub struct Call {
    pub loc: Loc,
    pub caller: Expression,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct Syscall {
    pub loc: Loc,
    pub arity: usize,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct Array {
    pub loc: Loc,
    pub ttype: CHSType,
    pub size: u64,
}

#[derive(Debug)]
pub struct Binop {
    pub loc: Loc,
    pub op: Operator,
    pub left: Expression,
    pub right: Expression,
    pub ttype: Option<CHSType>,
}

#[derive(Debug)]
pub struct Unop {
    pub loc: Loc,
    pub op: Operator,
    pub left: Expression,
    pub ttype: Option<CHSType>,
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
            _ => chs_error!("{} Unsupported operator", token.loc),
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
