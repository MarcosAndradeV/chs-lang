use crate::{
    chs_ast::{ModuleImpl, RawModule, nodes::Operator},
    chs_lexer::{Span, Token},
    chs_types::CHSType,
};

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
    pub body: MIRBlock,
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
    Goto(BlockId)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Addr {
    ty: CHSType,
    index: usize,
}

// TODO: Maybe add Intrinsics or Buildins
#[derive(Debug)]
pub enum MIROperation {
    Assignment {
        target: Addr,
        value: Operand,
    },
    Binop {
        target: Addr,
        op: Operator,
        op1: Addr,
        op2: Addr,
    },
    Unop {
        target: Addr,
        op: Operator,
        op1: Addr,
    },
    Refer { // &x
        target: Addr,
        op1: Addr,
    },
    Defer { // *x
        target: Addr,
        op1: Addr,
    },
    FuncCall {
        target: Option<Addr>, // NOTE: f() and a = f()
        name: Span<String>, // NOTE: only named functions calls for now
        args: Vec<Addr>
    },
}

#[derive(Debug)]
pub enum Operand {
    Literal(MIRLiteral, CHSType),
    Local(Addr),
}

#[derive(Debug)]
pub enum MIRLiteral {
    IntSigned(Span<i64>),
    IntUnsigned(Span<u64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
}
