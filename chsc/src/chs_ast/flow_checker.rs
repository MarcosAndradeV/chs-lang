use core::fmt;
use std::collections::HashSet;

use super::mir::{BlockId, MIRFunction, MIRModule, MIRModuleItem, Terminator};

/// Error types that can be encountered during flow checking
#[derive(Debug)]
pub enum FlowError {
    /// A block references another block that doesn't exist
    InvalidBlockReference {
        block_id: BlockId,
        function: String,
        location: String,
    },
    /// A block is unreachable from the entry point
    UnreachableBlock {
        block_id: BlockId,
        function: String,
        location: String,
    },
    /// Found unreachable code after a return statement
    UnreachableCodeAfterReturn {
        file_and_line: String,
        function: String,
        unreachable_block: BlockId,
    },
}

impl fmt::Display for FlowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowError::InvalidBlockReference {
                block_id,
                function,
                location,
            } => write!(
                f,
                "Invalid block reference to block {:?} in function `{}` at {}",
                block_id, function, location
            ),
            FlowError::UnreachableBlock {
                block_id,
                function,
                location,
            } => write!(
                f,
                "Unreachable block {:?} detected in function `{}` at {}",
                block_id, function, location
            ),
            FlowError::UnreachableCodeAfterReturn {
                file_and_line,
                function,
                unreachable_block,
            } => write!(
                f,
                "Unreachable code detected after return in function `{}` ({}), at block {:?}",
                function, file_and_line, unreachable_block
            ),
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
    pub fn new(module: &'src MIRModule<'src>) -> Self {
        Self { module }
    }

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

    pub fn check_function(&self, func: &MIRFunction) -> FlowResult<()> {
        let mut errors = Vec::new();

        self.check_block_references(func, &mut errors);
        self.check_reachable_blocks(func, &mut errors);
        // dbg!(&func.blocks);
        // self.check_unreachable_code(func, &mut errors);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_block_references(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        let max_block_id = func.blocks.len();

        for block in &func.blocks {
            let loc_str = format!("{}", self.module.raw_module.file_path);

            match &block.terminator {
                Terminator::Goto(target) => {
                    if target.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference {
                            block_id: *target,
                            function: self.module.raw_module[&func.name].to_string(),
                            location: loc_str.clone(),
                        });
                    }
                }
                Terminator::Switch {
                    true_block,
                    false_block,
                    ..
                } => {
                    if true_block.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference {
                            block_id: *true_block,
                            function: self.module.raw_module[&func.name].to_string(),
                            location: loc_str.clone(),
                        });
                    }
                    if false_block.0 >= max_block_id {
                        errors.push(FlowError::InvalidBlockReference {
                            block_id: *false_block,
                            function: self.module.raw_module[&func.name].to_string(),
                            location: loc_str.clone(),
                        });
                    }
                }
                _ => {}
            }
        }
    }

    fn check_reachable_blocks(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        let mut visited = HashSet::new();
        let mut worklist = vec![BlockId(0)];

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
                    true_block,
                    false_block,
                    ..
                } => {
                    worklist.push(*true_block);
                    worklist.push(*false_block);
                }
                _ => {}
            }
        }

        for (i, _) in func.blocks.iter().enumerate() {
            let block_id = BlockId(i);
            if !visited.contains(&block_id) {
                errors.push(FlowError::UnreachableBlock {
                    block_id,
                    function: self.module.raw_module[&func.name].to_string(),
                    location: self.module.raw_module.file_path.clone(),
                });
            }
        }
    }

    // Disable unreachable code check for now
    #[allow(dead_code)]
    fn check_unreachable_code(&self, func: &MIRFunction, errors: &mut Vec<FlowError>) {
        let mut reachable = HashSet::new();
        let mut worklist = vec![BlockId(0)];

        while let Some(block_id) = worklist.pop() {
            if !reachable.insert(block_id) {
                continue;
            }

            let block = &func.blocks[block_id.0];
            match &block.terminator {
                Terminator::Goto(target) => worklist.push(*target),
                Terminator::Switch { true_block, false_block, .. } => {
                    worklist.push(*true_block);
                    worklist.push(*false_block);
                }
                Terminator::Return | Terminator::Unreachable | Terminator::Nop => {}
            }
        }

        for (i, block) in func.blocks.iter().enumerate() {
            if let Terminator::Return = &block.terminator {
                let next_id = i + 1;
                if next_id < func.blocks.len() {
                    let next_block_id = BlockId(next_id);
                    if reachable.contains(&next_block_id) {
                        dbg!(&block);
                        errors.push(FlowError::UnreachableCodeAfterReturn {
                            file_and_line: format!("{}:{}", self.module.raw_module.file_path, func.name.loc),
                            function: self.module.raw_module[&func.name].to_string(),
                            unreachable_block: next_block_id,
                        });
                    }
                }
            }
        }
    }
}
