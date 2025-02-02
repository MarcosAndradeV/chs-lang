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
        match (self, other) {
            (a, b) if a == b => true,
            (CHSType::Alias(a), CHSType::Alias(b)) => a == b,
            (CHSType::Pointer(a), CHSType::Pointer(b)) => a.is_void() || b.is_void(),
            (CHSType::Pointer(a), CHSType::String) if **a == CHSType::Char => true,
            (CHSType::Alias(a), b) => env.get(a).is_some_and(|a| a.equivalent(b, env)),
            (a, CHSType::Alias(b)) => env.get(b).is_some_and(|b| b.equivalent(a, env)),
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(..) | Self::String)
    }

    pub fn is_void_pointer(&self) -> bool {
        matches!(self, Self::Pointer(a) if **a == Self::Void)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }
}

#[derive(Debug)]
pub struct TypeMap {
    type_decls: HashMap<String, CHSType>,
    globals: HashMap<String, CHSType>,
}

impl TypeMap {
    pub fn get_type(&self, k: &String) -> Option<&CHSType> {
        match self.type_decls.get(k) {
            Some(CHSType::Alias(sym)) => self.get_type(sym),
            other => other,
        }
    }

    pub fn get_global(&self, k: &String) -> Option<&CHSType> {
        match self.globals.get(k) {
            Some(CHSType::Alias(sym)) => self.get_type(sym),
            other => other,
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    type_decls: HashMap<&'a String, &'a CHSType>,
    globals: HashMap<&'a String, &'a CHSType>,
    locals: Vec<HashMap<&'a String, &'a CHSType>>,
}

impl Default for TypeEnv<'_> {
    fn default() -> Self {
        Self::new()
    }
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
        self.type_decls.insert(k, v)
    }
    pub fn type_decls_get(&self, k: &'a String) -> Option<&&CHSType> {
        match self.type_decls.get(k) {
            Some(CHSType::Alias(sym)) => self.type_decls.get(sym),
            tt => tt
        }
    }
    pub fn globals_insert(&mut self, k: &'a String, v: &'a CHSType) -> Option<&CHSType> {
        self.globals.insert(k, v)
    }
    pub fn get(&self, k: &String) -> Option<&&CHSType> {
        for scope in self.locals.iter().rev() {
            let get = scope.get(k);
            match get {
                Some(CHSType::Alias(sym)) => return self.type_decls_get(sym), // return self.get(sym),
                Some(_) => return get,
                _ => (),
            }
        }
        self.globals.get(k)
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
        TypeMap {
            globals: HashMap::from_iter(
            self.globals
                .into_iter()
                .map(|(k, v)| (k.clone(), v.clone()))),
            type_decls: HashMap::from_iter(
            self.type_decls
                .into_iter()
                .map(|(k, v)| (k.clone(), v.clone())))
        }
    }
}

pub trait InferType {
    // TODO: add `hint: Option<CHSType>`
    fn infer<'a>(&'a mut self, env: &mut TypeEnv<'a>) -> CHSResult<CHSType>;
}
