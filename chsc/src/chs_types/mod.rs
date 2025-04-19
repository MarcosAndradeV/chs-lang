use std::{fmt, rc::Rc};

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
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CHSType::Error => write!(f, "error"),
        }
    }
}
