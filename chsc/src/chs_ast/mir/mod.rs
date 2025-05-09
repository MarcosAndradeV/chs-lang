use crate::{
    chs_lexer::{Span, Token},
    chs_types::CHSType,
};

use super::{
    ModuleImpl, RawModule,
    hir::{self, HIRExpr},
    nodes::Operator,
    typechecker::{CHSInfer as _, TypeEnv},
};

/// MIR Module
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

impl<'src> MIRModule<'src> {
    pub fn from_hir(hir: hir::HIRModule<'src>, env: &TypeEnv) -> Self {
        let items = hir
            .items
            .into_iter()
            .map(|i| MIRModuleItem::from_hir(hir.raw_module, i, env))
            .collect();
        Self {
            raw_module: hir.raw_module,
            items,
        }
    }
}

/// MIR Module Item
#[derive(Debug)]
pub enum MIRModuleItem {
    Function(MIRFunction),
    ExternFunction(MIRExternFunction),
}

impl MIRModuleItem {
    pub fn from_hir(raw_module: &RawModule, item: hir::HIRModuleItem, env: &TypeEnv) -> Self {
        match item {
            hir::HIRModuleItem::Function(f) => {
                Self::Function(MIRFunction::from_hir(raw_module, f, env))
            }
            hir::HIRModuleItem::ExternFunction(f) => Self::ExternFunction(MIRExternFunction {
                name: f.name,
                fn_type: f.fn_type,
            }),
        }
    }
}

/// MIR Basic Block - contains a sequence of MIR statements that execute linearly
#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockId,
    pub statements: Vec<Statement>,
    /// The terminator determines how control flow continues after this block
    pub terminator: Terminator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

/// MIR Statement represents a single operation
#[derive(Debug)]
pub enum Statement {
    /// Call a function
    Call { func: Operand, args: Vec<Operand> },
    /// Assign a value to a local variable
    Assign { target: LocalId, value: Rvalue },
    /// Store a value to memory
    Store { place: Place, value: Rvalue },
}

/// MIR Terminator determines how control flow continues after a basic block
#[derive(Debug)]
pub enum Terminator {
    /// Jump to another block
    Goto(BlockId),
    /// Conditional branch
    Switch {
        condition: Operand,
        true_block: BlockId,
        false_block: BlockId,
    },
    /// Return from function
    Return(Option<Operand>),
    /// Unreachable code (after return/exit)
    Unreachable,
    Nop,
}

impl Terminator {
    pub fn is_return(&self) -> bool {
        matches!(self, Self::Return(..))
    }

    pub fn is_unreachable(&self) -> bool {
        matches!(self, Self::Unreachable)
    }
}

/// A place represents a location in memory where a value can be stored
#[derive(Debug)]
pub struct Place {
    pub local: LocalId,
    pub projection: Vec<ProjectionElem>,
}

#[derive(Debug)]
pub enum ProjectionElem {
    Deref,
    Field(usize),
    Index(Operand),
}

/// Right-hand side value computation
#[derive(Debug)]
pub enum Rvalue {
    /// Use of a local variable or constant
    Use(Operand),
    /// Binary operation
    BinaryOp(CHSType, Operator, CHSType, Operand, CHSType, Operand),
    /// Unary operation
    UnaryOp(CHSType, Operator, CHSType, Operand),
    /// Function call
    FunCall { func: Operand, args: Vec<Operand> },
    /// Type cast
    Cast { value: Operand, target_ty: CHSType },
    /// System call
    Syscall { number: u32, args: Vec<Operand> },
    /// Index a variable or constant
    Index { base: Operand, index: Operand },
    /// Pointer arithmetic
    PointerArithmetic {
        /// The operation to perform (add or subtract)
        op: Operator,
        /// The pointer operand
        pointer: Operand,
        /// The integer offset
        offset: Operand,
        /// The type the pointer points to
        pointee_ty: CHSType,
    },
}

#[derive(Debug)]
pub enum Operand {
    /// A constant value
    Constant(Constant),
    /// A copy of a place's value
    Copy(Place),
    /// Move a place's value
    Move(Place),
    /// Global value
    Global(Global),
}

#[derive(Debug)]
pub enum Global {
    /// A global extern
    Function(Span<String>, CHSType),
}

#[derive(Debug)]
pub enum Constant {
    I32(Span<i32>),
    U32(Span<u32>),
    I64(Span<i64>),
    U64(Span<u64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
    Void,
}

#[derive(Debug)]
pub struct Local {
    pub name: Option<Span<String>>,
    pub ty: CHSType,
}

#[derive(Debug)]
struct StmtBuilder<'src, 'env> {
    raw_module: &'src RawModule,
    env: &'env TypeEnv,
    blocks: &'src mut Vec<BasicBlock>,
    current_block_id: BlockId,
    locals: &'src mut Vec<Local>,
    next_local_id: &'src mut usize,
}

impl<'src> ModuleImpl<'src> for StmtBuilder<'src, '_> {
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

impl<'src, 'env> StmtBuilder<'src, 'env> {
    fn new(
        raw_module: &'src RawModule,
        env: &'env TypeEnv,
        blocks: &'src mut Vec<BasicBlock>,
        current_block_id: BlockId,
        locals: &'src mut Vec<Local>,
        next_local_id: &'src mut usize,
    ) -> Self {
        Self {
            raw_module,
            env,
            blocks,
            current_block_id,
            locals,
            next_local_id,
        }
    }

    pub fn finalize_blocks(&mut self) {
        let mut reachable = vec![false; self.blocks.len()];
        self.mark_reachable_blocks(BlockId(0), &mut reachable);

        let len = self.blocks.len();
        for (i, block) in self.blocks.iter_mut().enumerate() {
            if !reachable[i] {
                block.terminator = Terminator::Unreachable;
            } else if matches!(block.terminator, Terminator::Nop) {
                // Default fallthrough (if none set), jump to next block if it exists
                if i + 1 < len {
                    block.terminator = Terminator::Goto(BlockId(i + 1));
                } else {
                    block.terminator = Terminator::Unreachable;
                }
            }
        }
    }

    fn mark_reachable_blocks(&self, start: BlockId, reachable: &mut Vec<bool>) {
        let mut worklist = vec![start];

        while let Some(block_id) = worklist.pop() {
            let idx = block_id.0;
            if idx >= self.blocks.len() || reachable[idx] {
                continue;
            }
            reachable[idx] = true;

            match &self.blocks[idx].terminator {
                Terminator::Goto(target) => worklist.push(*target),
                Terminator::Switch {
                    true_block,
                    false_block,
                    ..
                } => {
                    worklist.push(*true_block);
                    worklist.push(*false_block);
                }
                Terminator::Return(_) | Terminator::Unreachable | Terminator::Nop => {}
            }
        }
    }

    fn add_local(&mut self, name: Option<Span<String>>, ty: CHSType) -> LocalId {
        let id = LocalId(*self.next_local_id);
        *self.next_local_id += 1;
        self.locals.push(Local { name, ty });
        id
    }

    fn build_expr(&mut self, expr: hir::HIRExpr) -> Operand {
        let ty = expr.infer();
        match expr {
            hir::HIRExpr::Literal(lit, _) => self.build_literal(lit, ty),
            hir::HIRExpr::Identifier(id, _) => {
                if let Some(local) = self.locals.iter().position(|l| {
                    l.name
                        .as_ref()
                        .is_some_and(|n| self.get_span_str(n) == self.get_span_str(&id))
                }) {
                    Operand::Copy(Place {
                        local: LocalId(local),
                        projection: vec![],
                    })
                } else if let Some(f @ CHSType::Function(..) | f @ CHSType::VariadicFunction(..)) =
                    self.env.global_get(self.get_span_str(&id))
                {
                    Operand::Global(Global::Function(id, f.clone()))
                } else {
                    todo!()
                }
            }
            hir::HIRExpr::Binary { ty, op, lhs, rhs } => {
                let lty = lhs.infer();
                let rty = rhs.infer();
                // TODO: Add a error handling for ExprBuilder
                let ty = ty.unwrap();

                let lhs = self.build_expr(*lhs);
                let rhs = self.build_expr(*rhs);
                let temp = self.add_local(None, ty.clone());

                let lower_binop = op.op;
                let block = self.current_block_mut();

                // Special handling for pointer arithmetic
                match (&lty, &rty, op.op) {
                    (CHSType::Pointer(inner), CHSType::Int, Operator::Plus) => {
                        // Pointer + int
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::PointerArithmetic {
                                op: Operator::Plus,
                                pointer: lhs,
                                offset: rhs,
                                pointee_ty: *inner.clone(),
                            },
                        });
                    }
                    (CHSType::Int, CHSType::Pointer(inner), Operator::Plus) => {
                        // int + Pointer
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::PointerArithmetic {
                                op: Operator::Plus,
                                pointer: rhs,
                                offset: lhs,
                                pointee_ty: *inner.clone(),
                            },
                        });
                    }
                    (CHSType::Pointer(inner), CHSType::Int, Operator::Minus) => {
                        // Pointer - int
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::PointerArithmetic {
                                op: Operator::Minus,
                                pointer: lhs,
                                offset: rhs,
                                pointee_ty: *inner.clone(),
                            },
                        });
                    }
                    (CHSType::Pointer(inner1), CHSType::Pointer(inner2), Operator::Minus) => {
                        // Pointer - Pointer
                        if inner1 == inner2 {
                            // Calculate the offset between two pointers
                            block.statements.push(Statement::Assign {
                                target: temp,
                                value: Rvalue::BinaryOp(ty, Operator::Minus, lty, lhs, rty, rhs),
                            });
                        } else {
                            panic!("Cannot subtract pointers of different types");
                        }
                    }
                    _ => {
                        // Regular binary operation
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::BinaryOp(ty, lower_binop, lty, lhs, rty, rhs),
                        });
                    }
                }

                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::Unary { ty, op, operand } => {
                let ty = ty.unwrap();
                let oparand_ty = operand.infer();
                let operand = self.build_expr(*operand);
                let temp = self.add_local(None, ty.clone());
                let projection = vec![];
                // let lower_unop = match op.op {
                //     op @ Operator::Deref => {
                //         projection.push(ProjectionElem::Deref);
                //         op
                //     }
                //     op => op,
                // };
                let lower_unop = op.op;

                self.current_block_mut().statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::UnaryOp(ty, lower_unop, oparand_ty, operand),
                });
                Operand::Copy(Place {
                    local: temp,
                    projection,
                })
            }
            hir::HIRExpr::Call {
                ty: _,
                span: _,
                callee,
                args,
            } => {
                let callee = self.build_expr(*callee);
                let args = args.into_iter().map(|arg| self.build_expr(arg)).collect();
                let not_void = ty != CHSType::Void;
                let temp = self.add_local(None, ty);
                if not_void {
                    self.current_block_mut().statements.push(Statement::Assign {
                        target: temp,
                        value: Rvalue::FunCall { func: callee, args },
                    });
                    Operand::Copy(Place {
                        local: temp,
                        projection: vec![],
                    })
                } else {
                    Operand::Constant(Constant::Void)
                }
            }
            hir::HIRExpr::Cast {
                span: _,
                expr,
                to_type,
            } => {
                let value = self.build_expr(*expr);
                let temp = self.add_local(None, to_type.clone());

                self.current_block_mut().statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::Cast {
                        value,
                        target_ty: to_type,
                    },
                });
                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::Index {
                span: _,
                base,
                index,
            } => {
                let base = self.build_expr(*base);
                let index = self.build_expr(*index);
                let temp = self.add_local(None, ty);

                self.current_block_mut().statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::Index {
                        base,
                        index: Operand::Copy(Place {
                            local: temp,
                            projection: vec![ProjectionElem::Index(index)],
                        }),
                    },
                });
                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::Syscall {
                span: _,
                arity,
                args,
            } => {
                let args = args.into_iter().map(|arg| self.build_expr(arg)).collect();
                let ty = CHSType::Int;
                let temp = self.add_local(None, ty);

                self.current_block_mut().statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::Syscall {
                        number: arity as u32,
                        args,
                    },
                });
                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
        }
    }

    fn build_literal(&mut self, lit: hir::HIRLiteral, ty: CHSType) -> Operand {
        match lit {
            hir::HIRLiteral::Int(span) => match ty {
                CHSType::I32 => Operand::Constant(Constant::I32(span.to_span())),
                CHSType::U32 => Operand::Constant(Constant::U32(span.to_span())),
                CHSType::I64 => Operand::Constant(Constant::I64(span.to_span())),
                CHSType::U64 => Operand::Constant(Constant::U64(span.to_span())),
                _ => todo!("{} Untyped int found in MIR. This is a bug.", span.loc),
            },
            hir::HIRLiteral::Bool(span) => Operand::Constant(Constant::Bool(span)),
            hir::HIRLiteral::Str(span) => Operand::Constant(Constant::Str(span)),
            hir::HIRLiteral::Char(span) => Operand::Constant(Constant::Char(span)),
            hir::HIRLiteral::Void => Operand::Constant(Constant::Void),
        }
    }

    fn build_stmt(&mut self, stmt: hir::HIRStmt) {
        match stmt {
            hir::HIRStmt::Assign {
                target,
                value,
                ..
            } => {
                let value = self.build_expr(*value);
                let target = self.build_expr(*target);

                match target {
                    Operand::Copy(place) => {
                        self.blocks[self.current_block_id.0]
                            .statements
                            .push(Statement::Store {
                                place,
                                value: Rvalue::Use(value),
                            });
                    }
                    _ => {
                        debug_assert!(false, "Assignment target must be a place but is {target:?}")
                    }
                }
            }
            hir::HIRStmt::VarDecl { name, ty, value } => {
                let ty = ty.unwrap_or_else(|| {
                    let inferred = value.infer();
                    assert!(
                        !inferred.is_never(),
                        "Could not infer type for variable {}",
                        self.get_span_str(&name)
                    );
                    inferred
                });
                let local = self.add_local(Some(name), ty);
                let value = self.build_expr(*value);
                self.blocks[self.current_block_id.0]
                    .statements
                    .push(Statement::Assign {
                        target: local,
                        value: Rvalue::Use(value),
                    });
            }
            hir::HIRStmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_block = self.create_empty_block();
                self.blocks[self.current_block_id.0].terminator = Terminator::Goto(cond_block);
                self.current_block_id = cond_block;
                let condition = self.build_expr(*condition);
                let true_block = self.create_empty_block();
                let false_block = self.create_empty_block();
                let end_block = if else_branch.is_some() {
                    self.create_empty_block()
                } else {
                    false_block
                };

                self.blocks[cond_block.0].terminator = Terminator::Switch {
                    condition,
                    true_block,
                    false_block,
                };

                self.current_block_id = true_block;
                for stmt in then_branch.statements {
                    self.build_stmt(stmt);
                }
                self.blocks[self.current_block_id.0].terminator = Terminator::Goto(end_block);

                self.current_block_id = false_block;
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch.statements {
                        self.build_stmt(stmt);
                    }
                    self.blocks[self.current_block_id.0].terminator = Terminator::Goto(end_block);
                    self.current_block_id = end_block;
                }
            }
            hir::HIRStmt::While {
                condition, body, ..
            } => {
                let cond_block = self.create_empty_block();
                self.blocks[self.current_block_id.0].terminator = Terminator::Goto(cond_block);
                self.current_block_id = cond_block;
                let condition = self.build_expr(*condition);
                let true_block = self.create_empty_block();
                let false_block = self.create_empty_block();
                self.blocks[cond_block.0].terminator = Terminator::Switch {
                    condition,
                    true_block,
                    false_block,
                };
                self.current_block_id = true_block;
                for stmt in body.statements {
                    self.build_stmt(stmt);
                }
                self.blocks[self.current_block_id.0].terminator = Terminator::Goto(cond_block);
                self.current_block_id = false_block;
            }
            hir::HIRStmt::Return { expr, .. } => {
                let value = expr.map(|e| self.build_expr(*e));
                self.current_block_mut().terminator = Terminator::Return(value);
            }
            hir::HIRStmt::ExprStmt {
                value: HIRExpr::Call { callee, args, .. },
            } => {
                let callee = self.build_expr(*callee);
                let args = args.into_iter().map(|arg| self.build_expr(arg)).collect();
                self.current_block_mut()
                    .statements
                    .push(Statement::Call { func: callee, args });
            }
            hir::HIRStmt::ExprStmt { .. } => {}
        }
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.blocks[self.current_block_id.0]
    }

    fn create_empty_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(BasicBlock {
            id,
            statements: Vec::new(),
            terminator: Terminator::Nop,
        });
        id
    }
}

#[derive(Debug)]
pub struct MIRExternFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
}

#[derive(Debug)]
pub struct MIRFunction {
    pub name: Span<String>,
    pub fn_type: CHSType,
    pub params: Vec<LocalId>,
    pub return_type: CHSType,
    pub blocks: Vec<BasicBlock>,
    pub locals: Vec<Local>,
}

impl MIRFunction {
    pub fn from_hir(raw_module: &RawModule, hir_fn: hir::HIRFunction, env: &TypeEnv) -> Self {
        let mut blocks = Vec::new();
        let mut locals = Vec::new();
        let mut next_local_id = 0;
        let current_block_id = BlockId(0);

        // Create initial block
        blocks.push(BasicBlock {
            id: BlockId(0),
            statements: Vec::new(),
            terminator: Terminator::Unreachable,
        });

        // Add parameters as locals
        let params: Vec<_> = hir_fn
            .params
            .into_iter()
            .map(|param| {
                let id = LocalId(next_local_id);
                next_local_id += 1;
                locals.push(Local {
                    name: Some(param.name),
                    ty: param.param_type,
                });
                id
            })
            .collect();

        // Create expr builder to handle expression lowering
        let mut builder = StmtBuilder::new(
            raw_module,
            env,
            &mut blocks,
            current_block_id,
            &mut locals,
            &mut next_local_id,
        );

        // Build the function body
        for stmt in hir_fn.body {
            builder.build_stmt(stmt);
        }

        builder.finalize_blocks();

        MIRFunction {
            name: hir_fn.name,
            fn_type: hir_fn.fn_type,
            params,
            return_type: hir_fn.return_type,
            blocks,
            locals,
        }
    }
}
