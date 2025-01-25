use chs_util::{chs_error, CHSResult};
use std::{collections::HashMap, slice::Iter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CHSType {
    Int,
    UInt,
    Void,
    Char,
    Boolean,
    Alias(String),
    Pointer(Box<Self>),
    Function(Vec<Self>, Box<Self>),
}

impl CHSType {
    pub fn equivalent(&self, other: &Self, env: &TypeEnv) -> bool {
        match (self, other) {
            (CHSType::Pointer(a), CHSType::Pointer(b)) => {
                matches!(**a, CHSType::Void) || matches!(**b, CHSType::Void)
            }
            (CHSType::Alias(a), b) => env.get(a).is_some_and(|a| a.equivalent(b, env)),
            (a, CHSType::Alias(b)) => env.get(b).is_some_and(|b| b.equivalent(a, env)),
            (a, b) => a == b,
        }
    }
    pub fn call(&self, env: &TypeEnv, args: Iter<impl InferType>) -> CHSResult<CHSType> {
        use CHSType::*;
        match self {
            Function(fn_args, ret_type) => {
                if fn_args.len() != args.len() {
                    chs_error!("Arity mismatch");
                }
                for (expect, actual) in fn_args.iter().zip(args) {
                    let actual = actual.infer(env)?;
                    if !expect.equivalent(&actual, &env) {
                        chs_error!(
                            "Argument type mismatch. Expect: {:?}  Actual: {:?}",
                            expect,
                            actual
                        );
                    }
                }
                Ok(*ret_type.clone())
            }
            _ => chs_error!("Cannot call `{:?}`", self),
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    globals: HashMap<&'a String, &'a CHSType>,
    locals: Vec<HashMap<&'a String, &'a CHSType>>,
}

impl<'a> TypeEnv<'a> {
    pub fn new(globals: impl Iterator<Item = (&'a String, &'a CHSType)>) -> Self {
        Self {
            globals: HashMap::from_iter(globals),
            locals: vec![],
        }
    }
    pub fn get(&self, k: &String) -> Option<&&CHSType> {
        self.locals
            .last()
            .and_then(|s| s.get(k))
            .or(self.globals.get(k))
    }
    pub fn locals_insert(&mut self, k: &'a String, v: &'a CHSType) -> Option<&CHSType> {
        self.locals
            .last_mut()
            .expect("Expect at least one scope.")
            .insert(k, v)
    }
    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (&'a String, &'a CHSType)>) {
        self.locals
            .last_mut()
            .expect("Expect at least one scope.")
            .extend(iter);
    }
    pub fn locals_new(&mut self) {
        self.locals.push(HashMap::new());
    }
    pub fn locals_pop(&mut self) {
        self.locals.pop();
    }
}

pub trait InferType {
    fn infer(&self, env: &TypeEnv) -> CHSResult<CHSType>;
}
