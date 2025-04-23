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
    /// Generic Int type
    Int,
    /// Specific Int types
    I32, I64,
    U32, U64,

    Void,
    Char,
    Boolean,
    String,

    /// Type variables for inference
    Var(TypeVar),

    /// Composite types
    Pointer(Box<CHSType>),
    Function(Vec<CHSType>, Box<CHSType>),
    VariadicFunction(Vec<CHSType>, Box<CHSType>),
    Slice(Box<CHSType>),

    /// Generic types
    Generic(String, Vec<CHSType>),
    Any,

    /// Error type for type checking
    Error,
    /// Never type for type checking
    Never,
}

impl fmt::Display for CHSType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CHSType::Int => write!(f, "int"),
            CHSType::I32 => write!(f, "i32"),
            CHSType::I64 => write!(f, "i64"),
            CHSType::U32 => write!(f, "u32"),
            CHSType::U64 => write!(f, "u64"),
            CHSType::Void => write!(f, "void"),
            CHSType::Char => write!(f, "char"),
            CHSType::Boolean => write!(f, "bool"),
            CHSType::String => write!(f, "string"),
            CHSType::Var(tv) => write!(f, "{}", tv),
            CHSType::Any => write!(f, "any"),
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
            CHSType::VariadicFunction(args, ret) => write!(
                f,
                "fn({} ...) -> {}",
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
            CHSType::Never => write!(f, "never"),
        }
    }
}

impl CHSType {
    pub fn is_never(&self) -> bool {
        matches!(self, CHSType::Never)
    }
}
