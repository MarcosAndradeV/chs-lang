use crate::{
    chs_ast::{ModuleImpl, RawModule, nodes::Operator},
    chs_lexer::{Span, Token},
    chs_types::CHSType,
};

pub mod lower_hir;
pub mod printer;

#[derive(Debug)]
pub struct MIRModule<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<MIRModuleItem>,
}

impl<'src> ModuleImpl<'src> for MIRModule<'src> {
    fn get_span_str<T>(&self, span: &Span<T>) -> &'src str {
        &self.raw_module[span]
    }

    fn get_token_str(&self, token: &Token) -> &'src str {
        &self.raw_module[token]
    }

    fn get_file_path(&self) -> &'src str {
        &self.raw_module.file_path
    }
}

#[derive(Debug)]
pub enum MIRModuleItem {
    Function(MIRFunction),
    ExternFunction(MIRExternFunction),
}

/// Representation of a function in the MIR
#[derive(Debug)]
pub struct MIRFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
    pub locals: Vec<CHSType>,
    pub body: Vec<MIRBlock>,
}

impl MIRFunction {
    pub fn new(name: Span<String>, fn_type: CHSType) -> Self {
        Self {
            name,
            fn_type,
            locals: vec![],
            body: vec![],
        }
    }
}

/// Representation of a extern function in the MIR
#[derive(Debug)]
pub struct MIRExternFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
}

#[derive(Debug, Default)]
pub struct MIRBlock {
    pub operations: Vec<MIROperation>,
    pub terminator: Option<Terminator>,
}

impl MIRBlock {
    pub const fn is_terminated(&self) -> bool {
        self.terminator.is_some()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(pub usize);

#[derive(Debug)]
pub enum Terminator {
    Return(Option<Operand>),
    Branch {
        cond: Operand,
        true_block: BlockId,
        false_block: BlockId,
    },
    Goto(BlockId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Addr {
    index: usize,
}

// TODO: Maybe add Intrinsics or Buildins
#[derive(Debug)]
pub enum MIROperation {
    Assignment {
        target: LocalAddress,
        value: Operand,
    },
    Binop {
        target: LocalValue,
        op: Operator,
        lhs: Operand,
        rhs: Operand,
    },
    Unop {
        target: LocalValue,
        op: Operator,
        operand: Operand,
    },
    Refer {
        // &x
        target: Addr,
        addr: Addr,
    },
    Load {
        // *x
        target: LocalValue,
        addr: Operand,
    },
    FuncCall {
        target: Option<LocalValue>, // NOTE: f() and a = f()
        name: String,   // NOTE: only named functions calls for now
        args: Vec<Operand>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct LocalValue(pub Addr);

#[derive(Debug, Clone, Copy)]
pub struct LocalAddress(pub Addr);

#[derive(Debug, Clone)]
pub enum Operand {
    Value(LocalValue),
    Address(LocalAddress),
    Literal(MIRLiteral, CHSType),
}


#[derive(Debug, Clone)]
pub enum MIRLiteral {
    Int(Span<u64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
}

impl MIRLiteral {
    pub fn span(&self) -> Span<String> {
        match self {
            MIRLiteral::Int(span) => span.to_span(),
            MIRLiteral::Bool(span) => span.to_span(),
            MIRLiteral::Str(span) => span.to_span(),
            MIRLiteral::Char(span) => span.to_span(),
        }
    }
}
