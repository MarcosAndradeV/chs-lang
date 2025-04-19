use core::fmt;
use std::collections::HashSet;

use super::mir::{BlockId, MIRFunction, MIRModule, MIRModuleItem, Terminator};

/// Error types that can be encountered during flow checking
#[derive(Debug)]
pub enum FlowError {
    /// A block references another block that doesn't exist
    InvalidBlockReference(BlockId),
    /// A block is unreachable from the entry point
    UnreachableBlock(BlockId),
    /// Found unreachable code after a return statement
    UnreachableCodeAfterReturn(BlockId),
}

impl fmt::Display for FlowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowError::InvalidBlockReference(block_id) => {
                write!(f, "Invalid block reference: {:?}", block_id)
            }
            FlowError::UnreachableBlock(block_id) => write!(f, "Unreachable block: {:?}", block_id),
            FlowError::UnreachableCodeAfterReturn(block_id) => {
                write!(f, "Unreachable code after return in block: {:?}", block_id)
            }
        }
    }
}

/// Result type for flow checking operations
pub type FlowResult<T> = Result<T, Vec<FlowError>>;

/// Main flow checker that validates MIR control flow
pub struct FlowChecker<'src> {
    module: &'src MIRModule<'src>,
}

impl<'src> FlowChecker<'src> {
    /// Create a new flow checker for the given MIR module
    pub fn new(module: &'src MIRModule<'src>) -> Self {
        Self { module }
    }

    /// Check the entire module for flow errors
    pub fn check_module(&self) -> FlowResult<()> {
        let mut errors = Vec::new();

        for item in &self.module.items {
            if let MIRModuleItem::Function(func) = item {
                if let Err(mut func_errors) = self.check_function(func) {
                    errors.append(&mut func_errors);
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check a single function for flow errors
    pub fn check_function(&self, func: &MIRFunction) -> FlowResult<()> {
        let mut errors = Vec::new();

        // Check if all block IDs referenced in terminators exist
        self.check_block_references(func, &mut errors);

        // Check for unreachable blocks
        self.check_reachable_blocks(func, &mut errors);

        // Check for unreachable code after returns
        self.check_unreachable_code(func, &mut errors);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check that all block references in terminators are valid
    fn check_block_references(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        let max_block_id = func.blocks.len();

        for block in &func.blocks {
            match &block.terminator {
                Terminator::Goto(target) => {
                    if target.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference(*target));
                    }
                }
                Terminator::Switch {
                    condition: _,
                    true_block,
                    false_block,
                } => {
                    if true_block.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference(*true_block));
                    }
                    if false_block.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference(*false_block));
                    }
                }
                Terminator::Return(_) | Terminator::Unreachable => {}
            }
        }
    }

    /// Check if all blocks are reachable from the entry block
    fn check_reachable_blocks(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        let mut visited = HashSet::new();
        let mut worklist = vec![BlockId(0)]; // Start with entry block

        while let Some(block_id) = worklist.pop() {
            if !visited.insert(block_id) {
                continue;
            }

            let block = &func.blocks[block_id.0];
            match &block.terminator {
                Terminator::Goto(target) => {
                    worklist.push(*target);
                }
                Terminator::Switch {
                    condition: _,
                    true_block,
                    false_block,
                } => {
                    worklist.push(*true_block);
                    worklist.push(*false_block);
                }
                Terminator::Return(_) | Terminator::Unreachable => {}
            }
        }

        // Check for unreachable blocks
        for (i, _) in func.blocks.iter().enumerate() {
            let block_id = BlockId(i);
            if !visited.contains(&block_id) {
                errors.push(FlowError::UnreachableBlock(block_id));
            }
        }
    }

    /// Check for unreachable code after returns
    fn check_unreachable_code(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        for block in &func.blocks {
            // If a block has statements after a return terminator, mark as error
            if let Terminator::Return(_) = &block.terminator {
                // Check next block if it exists and is reachable
                let next_block_id = BlockId(block.id.0 + 1);
                if next_block_id.0 < func.blocks.len() {
                    // Only report if the next block is reachable through some path
                    // We'll skip this check as it would duplicate unreachable block errors
                    errors.push(FlowError::UnreachableCodeAfterReturn(next_block_id));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        chs_ast::{
            flow_checker::{FlowChecker, FlowError},
            mir::{BasicBlock, BlockId, Local, MIRFunction, MIRModule, MIRModuleItem, Terminator},
            RawModule,
        },
        chs_lexer::{Loc, Token, TokenKind},
        chs_types::CHSType,
    };

    fn create_test_raw_module() -> RawModule {
        RawModule {
            source: String::new(),
            file_path: String::new(),
        }
    }

    fn create_test_function(blocks: Vec<BasicBlock>) -> MIRFunction {
        let loc = Loc::new(1, 1);
        let token = Token::new(TokenKind::Identifier, loc, 0, 4);

        MIRFunction {
            name: token.source,
            fn_type: CHSType::Int,
            params: vec![],
            return_type: CHSType::Int,
            blocks,
            locals: vec![Local {
                name: None,
                ty: CHSType::Int,
            }],
        }
    }

    #[test]
    fn test_invalid_block_reference() {
        let blocks = vec![BasicBlock {
            id: BlockId(0),
            statements: vec![],
            terminator: Terminator::Goto(BlockId(1)), // Reference to non-existent block
        }];

        let func = create_test_function(blocks);
        let module = MIRModule {
            raw_module: &create_test_raw_module(),
            items: vec![MIRModuleItem::Function(func)],
        };

        let checker = FlowChecker::new(&module);
        let result = checker.check_module();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(matches!(
            errors.first().unwrap(),
            FlowError::InvalidBlockReference(BlockId(1))
        ));
    }

    #[test]
    fn test_unreachable_block() {
        let blocks = vec![
            BasicBlock {
                id: BlockId(0),
                statements: vec![],
                terminator: Terminator::Return(None),
            },
            BasicBlock {
                // Unreachable block
                id: BlockId(1),
                statements: vec![],
                terminator: Terminator::Return(None),
            },
        ];

        let func = create_test_function(blocks);
        let module = MIRModule {
            raw_module: &create_test_raw_module(),
            items: vec![MIRModuleItem::Function(func)],
        };

        let checker = FlowChecker::new(&module);
        let result = checker.check_module();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(matches!(
            errors.first().unwrap(),
            FlowError::UnreachableBlock(BlockId(1))
        ));
    }

    #[test]
    fn test_valid_control_flow() {
        let blocks = vec![
            BasicBlock {
                id: BlockId(0),
                statements: vec![],
                terminator: Terminator::Goto(BlockId(1)),
            },
            BasicBlock {
                id: BlockId(1),
                statements: vec![],
                terminator: Terminator::Return(None),
            },
        ];

        let func = create_test_function(blocks);
        let module = MIRModule {
            raw_module: &create_test_raw_module(),
            items: vec![MIRModuleItem::Function(func)],
        };

        let checker = FlowChecker::new(&module);
        let result = checker.check_module();

        assert!(result.is_ok());
    }
}
