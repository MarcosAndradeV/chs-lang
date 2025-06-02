use chslexer::Token;
use std::collections::HashMap;

use crate::{chs_ast::ast::{BinaryOperator, UnaryOperator}, chs_types::CHSType};

#[derive(Debug)]
pub struct MIRModule {
    pub items: Vec<MIRModuleItem>,
    pub string_literals: Vec<String>,
}

impl MIRModule {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            string_literals: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: MIRFunction) {
        self.items.push(MIRModuleItem::Function(function));
    }

    pub fn add_extern_function(&mut self, extern_fn: MIRExternFunction) {
        self.items.push(MIRModuleItem::ExternFunction(extern_fn));
    }

    pub fn add_string_literal(&mut self, value: String) -> usize {
        let id = self.string_literals.len();
        self.string_literals.push(value);
        id
    }
}

#[derive(Debug)]
pub enum MIRModuleItem {
    Function(MIRFunction),
    ExternFunction(MIRExternFunction),
    GlobalVariable(MIRGlobalVariable),
    Constant(MIRConstant),
}

#[derive(Debug)]
pub struct MIRGlobalVariable {
    pub name: Token,
    pub ty: CHSType,
    pub initializer: Operand,
}

#[derive(Debug)]
pub struct MIRConstant {
    pub name: Token,
    pub ty: CHSType,
    pub value: Operand,
}

/// Representation of a function in the MIR
#[derive(Debug)]
pub struct MIRFunction {
    pub name: Token,
    pub args: Vec<LocalId>,
    pub locals: Vec<MIRLocal>,
    pub body: Vec<MIRBlock>,
    pub return_type: CHSType,
}

impl MIRFunction {
    pub fn new(name: Token, return_type: CHSType) -> Self {
        Self {
            name,
            args: vec![],
            locals: vec![],
            body: vec![],
            return_type,
        }
    }

    pub fn add_local(&mut self, ty: CHSType) -> LocalId {
        let id = LocalId(self.locals.len());
        self.locals.push(MIRLocal { ty, name: None });
        id
    }

    pub fn add_named_local(&mut self, ty: CHSType, name: Token) -> LocalId {
        let id = LocalId(self.locals.len());
        self.locals.push(MIRLocal {
            ty,
            name: Some(name),
        });
        id
    }

    pub fn add_arg(&mut self, ty: CHSType, name: Option<Token>) -> LocalId {
        let id = self.add_local(ty);
        if let Some(name) = name {
            self.locals[id.0].name = Some(name);
        }
        self.args.push(id);
        id
    }

    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.body.len());
        self.body.push(MIRBlock::default());
        id
    }

    pub fn get_block_mut(&mut self, id: BlockId) -> &mut MIRBlock {
        &mut self.body[id.0]
    }

    pub fn get_block(&self, id: BlockId) -> &MIRBlock {
        &self.body[id.0]
    }
}

#[derive(Debug)]
pub struct MIRLocal {
    pub ty: CHSType,
    pub name: Option<Token>,
}

/// Representation of an extern function in the MIR
#[derive(Debug)]
pub struct MIRExternFunction {
    pub name: Token,
    pub args: Vec<CHSType>,
    pub return_type: CHSType,
    pub is_variadic: bool,
}

impl MIRExternFunction {
    pub fn new(name: Token, return_type: CHSType) -> Self {
        Self {
            name,
            args: vec![],
            return_type,
            is_variadic: false,
        }
    }

    pub fn add_arg(&mut self, ty: CHSType) {
        self.args.push(ty);
    }

    pub fn set_variadic(&mut self) {
        self.is_variadic = true;
    }
}

#[derive(Debug, Default)]
pub struct MIRBlock {
    pub operations: Vec<MIROperation>,
    pub terminator: Option<Terminator>,
}

impl MIRBlock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_operation(&mut self, op: MIROperation) {
        self.operations.push(op);
    }

    pub fn set_terminator(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator);
    }

    pub fn is_terminated(&self) -> bool {
        match self.terminator {
            None | Some(Terminator::Unreachable) => false,
            _ => true,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.operations.is_empty() && self.terminator.is_none()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub usize);

#[derive(Debug)]
pub enum Terminator {
    Return(Option<Operand>),
    Branch {
        cond: Operand,
        true_block: BlockId,
        false_block: BlockId,
    },
    Goto(BlockId),
    // Switch {
    //     test: Operand,
    //     cases: Vec<(i64, BlockId)>,
    //     default: Option<BlockId>,
    // },
    // Call {
    //     func: Operand,
    //     args: Vec<Operand>,
    //     destination: Option<(LocalId, BlockId)>,
    // },
    Unreachable,
}

impl Terminator {
    pub fn is_unreachable(&self) -> bool {
        matches!(self, Self::Unreachable)
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Return(_) | Terminator::Unreachable => vec![],
            Terminator::Branch {
                true_block,
                false_block,
                ..
            } => {
                vec![*true_block, *false_block]
            }
            Terminator::Goto(block) => vec![*block],
            // Terminator::Switch { cases, default, .. } => {
            //     let mut successors: Vec<BlockId> = cases.iter().map(|(_, block)| *block).collect();
            //     if let Some(default_block) = default {
            //         successors.push(*default_block);
            //     }
            //     successors
            // }
            // Terminator::Call { destination, .. } => {
            //     if let Some((_, next_block)) = destination {
            //         vec![*next_block]
            //     } else {
            //         vec![]
            //     }
            // }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    /// Reference to a local variable
    Local(LocalId),

    /// Constant values
    Constant(Constant),

    /// Function reference
    Function(Token),

    /// Temporary value from an operation
    Temporary(TempId),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TempId(pub usize);

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    UInt(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(usize),
}

#[derive(Debug, Clone)]
pub enum MIROperation {
    /// Assign a value to a local
    Assign { target: LocalId, value: RValue },

    /// Store a value into memory
    Store { address: Operand, value: Operand },

    /// Load a value from memory
    Load { target: LocalId, address: Operand },

    /// No-op operation
    Nop,

    /// Assert that a condition is true
    Assert {
        cond: Operand,
        message: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub enum RValue {
    /// Use an operand directly
    Use(Operand),

    /// Binary operations
    BinaryOp {
        op: BinaryOperator,
        left: Operand,
        right: Operand,
    },

    /// Unary operations
    UnaryOp { op: UnaryOperator, operand: Operand },

    /// Take address of a place
    AddressOf {
        place: Operand,
    },

    /// Dereference a pointer
    Deref(Operand),

    /// Cast between types
    Cast {
        operand: Operand,
        target_type: CHSType,
    },

    /// Array/struct field access
    FieldAccess { base: Operand, field: usize },

    /// Array index access
    Index { base: Operand, index: Operand },

    /// Function call
    Call { func: Operand, args: Vec<Operand> },

    /// Aggregate construction (struct, array, tuple)
    Aggregate {
        kind: AggregateKind,
        fields: Vec<Operand>,
    },
}


#[derive(Debug, Clone)]
pub enum AggregateKind {
    Array(CHSType),
    Struct(Token),
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

impl std::fmt::Display for TempId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tmp{}", self.0)
    }
}

pub struct MIRBuilder {
    current_function: Option<usize>,
    current_block: Option<BlockId>,
    temp_counter: usize,
}

impl MIRBuilder {
    pub fn new() -> Self {
        Self {
            current_function: None,
            current_block: None,
            temp_counter: 0,
        }
    }

    pub fn next_temp(&mut self) -> TempId {
        let id = TempId(self.temp_counter);
        self.temp_counter += 1;
        id
    }

    pub fn set_current_function(&mut self, func_index: usize) {
        self.current_function = Some(func_index);
    }

    pub fn set_current_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn add_operation(&mut self, module: &mut MIRModule, op: MIROperation) {
        if let (Some(func_idx), Some(block_id)) = (self.current_function, self.current_block) {
            if let MIRModuleItem::Function(func) = &mut module.items[func_idx] {
                func.get_block_mut(block_id).add_operation(op);
            }
        }
    }

    pub fn set_terminator(&mut self, module: &mut MIRModule, terminator: Terminator) {
        if let (Some(func_idx), Some(block_id)) = (self.current_function, self.current_block) {
            if let MIRModuleItem::Function(func) = &mut module.items[func_idx] {
                func.get_block_mut(block_id).set_terminator(terminator);
            }
        }
    }
}

impl Default for MIRBuilder {
    fn default() -> Self {
        Self::new()
    }
}
