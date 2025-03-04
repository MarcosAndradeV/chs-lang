use chs_util::CHSResult;
use std::{collections::HashMap, fmt};

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

impl fmt::Display for CHSType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CHSType::Int => write!(f, "int"),
            CHSType::UInt => write!(f, "uint"),
            CHSType::Void => write!(f, "void"),
            CHSType::Char => write!(f, "char"),
            CHSType::Boolean => write!(f, "bool"),
            CHSType::String => write!(f, "string"),
            CHSType::Alias(s) => write!(f, "{s}"),
            CHSType::Distinct(t) => write!(f, "distinct {t}"),
            CHSType::Pointer(t) => write!(f, "*{t}"),
            CHSType::Function(args, ret_type) => write!(
                f,
                "fn({}) -> {ret_type}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
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
        matches!(self, Self::Pointer(..))
    }

    pub fn is_void_pointer(&self) -> bool {
        matches!(self, Self::Pointer(a) if **a == Self::Void)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }
}

#[derive(Debug, Default)]
pub struct TypeMap {
    type_decls: HashMap<String, CHSType>,
    globals: HashMap<String, CHSType>,
}

impl TypeMap {
    pub fn get_type(&self, k: &str) -> Option<&CHSType> {
        match self.type_decls.get(k) {
            Some(CHSType::Alias(sym)) => self.get_type(sym),
            other => other,
        }
    }

    pub fn get_global(&self, k: &str) -> Option<&CHSType> {
        match self.globals.get(k) {
            Some(CHSType::Alias(sym)) => self.get_type(sym),
            other => other,
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    type_decls: HashMap<&'a str, &'a CHSType>,
    globals: HashMap<&'a str, &'a CHSType>,
    locals: Vec<HashMap<&'a str, &'a CHSType>>,
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
    pub fn type_decls_get(&self, k: &'a str) -> Option<&&CHSType> {
        match self.type_decls.get(k) {
            Some(CHSType::Alias(sym)) => self.type_decls.get(sym.as_str()),
            tt => tt,
        }
    }
    pub fn globals_insert(&mut self, k: &'a String, v: &'a CHSType) -> Option<&CHSType> {
        self.globals.insert(k, v)
    }
    pub fn get(&self, k: &str) -> Option<&&CHSType> {
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
    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (&'a str, &'a CHSType)>) {
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
                    .map(|(k, v)| (k.to_string(), v.clone())),
            ),
            type_decls: HashMap::from_iter(
                self.type_decls
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v.clone())),
            ),
        }
    }
}

pub trait InferType {
    fn infer<'a>(&'a mut self, hint: Option<&CHSType>, env: &mut TypeEnv<'a>)
        -> CHSResult<CHSType>;
}
