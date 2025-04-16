use chs_util::{chs_error, CHSResult};
use std::{collections::HashMap, fmt, rc::Rc};

/// Represents a type variable used in type inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    id: usize,
    name: Option<String>,
}

impl TypeVar {
    pub fn new(id: usize, name: Option<String>) -> Self {
        Self { id, name }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "{}", name),
            None => write!(f, "t{}", self.id),
        }
    }
}

/// Represents a type constraint in the type system
#[derive(Debug, Clone)]
pub enum TypeConstraint {
    Equal(Rc<CHSType>, Rc<CHSType>),
    Subtype(Rc<CHSType>, Rc<CHSType>),
    Instance(Rc<CHSType>, Rc<CHSType>),
}

/// Represents the core types in the CHS type system
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CHSType {
    /// Basic types
    Int,
    UInt,
    Void,
    Char,
    Boolean,
    String,

    /// Type variables for inference
    Var(TypeVar),

    /// Composite types
    Pointer(Box<CHSType>),
    Function(Vec<CHSType>, Box<CHSType>),
    Tuple(Vec<CHSType>),
    Array(Box<CHSType>),

    /// Generic types
    Generic(String, Vec<CHSType>),

    /// Type aliases
    Alias(String, Box<CHSType>),

    /// Union types
    Union(Vec<CHSType>),

    /// Error type for type checking
    Error,
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
            CHSType::Var(tv) => write!(f, "{}", tv),
            CHSType::Pointer(t) => write!(f, "*{}", t),
            CHSType::Function(args, ret) => write!(
                f,
                "fn({}) -> {}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                ret
            ),
            CHSType::Tuple(types) => write!(
                f,
                "({})",
                types.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CHSType::Array(t) => write!(f, "[{}]", t),
            CHSType::Generic(name, params) => write!(
                f,
                "{}<{}>",
                name,
                params.iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CHSType::Alias(name, _) => write!(f, "{}", name),
            CHSType::Union(types) => write!(
                f,
                "{}",
                types.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            CHSType::Error => write!(f, "error"),
        }
    }
}

/// Type environment for managing type declarations and scoping
#[derive(Debug)]
pub struct TypeEnv<'a> {
    pub type_decls: HashMap<&'a str, Rc<CHSType>>,
    pub globals: HashMap<&'a str, Rc<CHSType>>,
    pub locals: Vec<HashMap<&'a str, Rc<CHSType>>>,
    pub type_vars: HashMap<TypeVar, Rc<CHSType>>,
    pub constraints: Vec<TypeConstraint>,
}

impl<'a> TypeEnv<'a> {
    pub fn new() -> Self {
        Self {
            type_decls: HashMap::new(),
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
            type_vars: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    /// Creates a new type variable
    pub fn fresh_type_var(&mut self, name: Option<String>) -> TypeVar {
        TypeVar::new(self.type_vars.len(), name)
    }

    /// Adds a type constraint
    pub fn add_constraint(&mut self, constraint: TypeConstraint) {
        self.constraints.push(constraint);
    }

    /// Attempts to unify two types
    pub fn unify(&mut self, t1: &CHSType, t2: &CHSType) -> CHSResult<()> {
        match (t1, t2) {
            (CHSType::Var(v1), CHSType::Var(v2)) if v1 == v2 => Ok(()),
            (CHSType::Var(v), t) | (t, CHSType::Var(v)) => {
                if let Some(bound) = self.type_vars.get(v).cloned() {
                    self.unify(&bound, t)
                } else {
                    self.type_vars.insert(v.clone(), Rc::new(t.clone()));
                    Ok(())
                }
            }
            (CHSType::Function(args1, ret1), CHSType::Function(args2, ret2)) => {
                if args1.len() != args2.len() {
                    chs_error!("Function argument count mismatch");
                }
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2)?;
                }
                self.unify(ret1, ret2)
            }
            (a, b) if a == b => Ok(()),
            _ => chs_error!("Types cannot be unified"),
        }
    }

    /// Adds a type declaration
    pub fn global_insert(&mut self, k: &'a str, v: Rc<CHSType>)-> Option<Rc<CHSType>> {
        self.globals.insert(k, v)
    }

    /// Adds a type declaration
    pub fn type_decls_insert(&mut self, k: &'a str, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.type_decls.insert(k, v)
    }

    /// Gets a type declaration
    pub fn get(&self, k: &str) -> Option<&Rc<CHSType>> {
        for scope in self.locals.iter().rev() {
            if let Some(t) = scope.get(k) {
                return Some(t);
            }
        }
        self.globals.get(k)
    }

    /// Inserts a type into the local scope
    pub fn locals_insert(&mut self, k: &'a str, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .insert(k, v)
    }

    /// Extends the local scope with new types
    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (&'a str, Rc<CHSType>)>) {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .extend(iter);
    }

    /// Creates a new local scope
    pub fn locals_new(&mut self) {
        self.locals.push(HashMap::new());
    }

    /// Removes the last local scope
    pub fn locals_pop(&mut self) {
        self.locals.pop();
    }
}

/// Trait for type inference
pub trait InferType {
    /// Infers the type of an expression
    fn infer<'a>(&'a self, env: &mut TypeEnv<'a>) -> CHSResult<&'a CHSType>;

    /// Checks if an expression has a specific type
    fn check<'a>(&'a self, expected: &CHSType, env: &mut TypeEnv<'a>) -> CHSResult<()>;
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct TypeMapLoc(usize);

/// Type map for storing global type information
#[derive(Debug, Default)]
pub struct TypeMap {
    pub type_decls: HashMap<String, Rc<CHSType>>,
    pub globals: HashMap<String, Rc<CHSType>>,
    pub locals: HashMap<TypeMapLoc, Rc<CHSType>>,
}

impl TypeMap {
    pub fn get_type(&self, k: &str) -> Option<&Rc<CHSType>> {
        self.type_decls.get(k)
    }

    pub fn get_global(&self, k: &str) -> Option<&Rc<CHSType>> {
        self.globals.get(k)
    }

    pub fn get_local(&self, k: &TypeMapLoc) -> Option<&Rc<CHSType>> {
        self.locals.get(k)
    }
}
