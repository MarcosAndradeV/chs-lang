use chs_util::{return_chs_error, CHSResult};
use std::{collections::HashMap, fmt, rc::Rc};

/// Represents a type variable used in type inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    id: usize,
}

impl TypeVar {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.id)
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
    Slice(Box<CHSType>),

    /// Generic types
    Generic(String, Vec<CHSType>),

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
            CHSType::Slice(t) => write!(f, "[{}]", t),
            CHSType::Generic(name, params) => write!(
                f,
                "{}<{}>",
                name,
                params.iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CHSType::Error => write!(f, "error"),
        }
    }
}

/// Type environment for managing type declarations and scoping
#[derive(Debug)]
pub struct TypeEnv {
    pub globals: HashMap<String, Rc<CHSType>>,
    pub locals: Vec<HashMap<String, Rc<CHSType>>>,
    pub type_vars: HashMap<TypeVar, Rc<CHSType>>,
    pub constraints: Vec<TypeConstraint>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
            type_vars: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    /// Creates a new type variable
    pub fn fresh_type_var(&mut self) -> TypeVar {
        TypeVar::new(self.type_vars.len())
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
                    return_chs_error!("Function argument count mismatch");
                }
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2)?;
                }
                self.unify(ret1, ret2)
            }
            (a, b) if a == b => Ok(()),
            _ => return_chs_error!("Types cannot be unified"),
        }
    }

    /// Adds a type declaration
    pub fn global_insert(&mut self, k: impl ToString, v: Rc<CHSType>)-> Option<Rc<CHSType>> {
        self.globals.insert(k.to_string(), v)
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
    pub fn locals_insert(&mut self, k: impl ToString, v: Rc<CHSType>) -> Option<Rc<CHSType>> {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .insert(k.to_string(), v)
    }

    /// Extends the local scope with new types
    pub fn locals_extend(&mut self, iter: impl Iterator<Item = (impl ToString, Rc<CHSType>)>) {
        self.locals
            .last_mut()
            .expect("Expected at least one scope")
            .extend(iter.map(|(k, v)| (k.to_string(), v)));
    }

    /// Creates a new local scope
    pub fn locals_new(&mut self) {
        self.locals.push(HashMap::new());
    }
}

pub trait CHSInfer {
    fn infer(&self, env: &TypeEnv) -> CHSType;
}
