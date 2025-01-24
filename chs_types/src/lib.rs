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
    pub fn equivalent(&self, other: &Self) -> bool {
        match (self, other) {
            (CHSType::Pointer(a), CHSType::Pointer(b)) => {
                matches!(**a, CHSType::Void) || matches!(**b, CHSType::Void)
            }
            (a, b) => a == b,
        }
    }
}

pub trait InferType {
    fn infer(&self) -> CHSType {
        CHSType::Void
    }
}
