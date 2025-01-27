use chs_util::CHSResult;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CHSType {
    Int,
    UInt,
    Void,
    Char,
    Boolean,
    String,
    Alias(String),
    Distinct(Box<Self>),
    Pointer(Box<Self>),
    Function(Vec<Self>, Box<Self>),
}

impl CHSType {
    pub fn equivalent(&self, other: &Self, env: &TypeEnv) -> bool {
        if self == other {return true;}
        match (self, other) {
            (CHSType::Pointer(a), CHSType::Pointer(b)) => {
                matches!(**a, CHSType::Void) || matches!(**b, CHSType::Void)
            }
            (CHSType::Alias(a), CHSType::Alias(b)) => a == b,
            (CHSType::Alias(a), b) => env.get(a).is_some_and(|a| a.equivalent(b, env)),
            (a, CHSType::Alias(b)) => env.get(b).is_some_and(|b| b.equivalent(a, env)),
            (a, b) => a == b,
        }
    }
}

#[derive(Debug)]
pub struct TypeMap(HashMap<String, CHSType>);

impl TypeMap {
    pub fn get(&self, k: &String) -> Option<&CHSType> {
        match self.0.get(k) {
            Some(CHSType::Alias(sym)) => self.get(sym),
            other => other
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    type_decls: HashMap<&'a String, &'a CHSType>,
    globals: HashMap<&'a String, &'a CHSType>,
    locals: Vec<HashMap<&'a String, &'a CHSType>>,
}

impl<'a> TypeEnv<'a> {
    pub fn new() -> Self {
        Self {
            type_decls: HashMap::new(),
            globals: HashMap::new(),
            locals: vec![],
        }
    }
    pub fn type_decls_insert(&mut self, k: &'a String, v: &'a CHSType) -> Option<&CHSType> {
        self.type_decls
            .insert(k, v)
    }
    pub fn globals_insert(&mut self, k: &'a String, v: &'a CHSType) -> Option<&CHSType> {
        self.globals
            .insert(k, v)
    }
    pub fn get(&self, k: &String) -> Option<&&CHSType> {
        match self.locals
            .last()
            .and_then(|s| s.get(k)) {
                Some(CHSType::Alias(ref sym)) => self.get(sym),
                None => self.globals.get(k),
                other => other
            }
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

    pub fn into_type_defs(self) -> TypeMap {
        TypeMap(HashMap::from_iter(
            self.globals
                .into_iter()
                .map(|(k, v)| (k.clone(), v.clone())),
        ))
    }
}

pub trait InferType {
    fn infer<'a>(&'a mut self, env: &mut TypeEnv<'a>) -> CHSResult<CHSType>;
}
