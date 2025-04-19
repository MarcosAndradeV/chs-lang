use crate::{chs_lexer::Span, chs_types::CHSType};

use super::{
    RawModule, hir,
    nodes::Operator,
    typechecker::{CHSInfer as _, TypeEnv},
};

/// MIR Module
#[derive(Debug)]
pub struct MIRModule<'src> {
    pub raw_module: &'src RawModule,
    pub items: Vec<MIRModuleItem>,
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
    BinaryOp(BinOp, Operand, Operand),
    /// Unary operation
    UnaryOp(UnOp, Operand),
    /// Function call
    Call { func: Operand, args: Vec<Operand> },
    /// Type cast
    Cast { value: Operand, target_ty: CHSType },
    /// System call
    Syscall { number: u32, args: Vec<Operand> },
    /// Index a variable or constant
    Index { base: Operand, index: Operand },
    /// Pointer arithmetic
    PointerArithmetic {
        /// The operation to perform (add or subtract)
        op: BinOp,
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
    Int(Span<i64>),
    Bool(Span<bool>),
    Str(Span<String>),
    Char(Span<char>),
    Void,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub struct Local {
    pub name: Option<Span<String>>,
    pub ty: CHSType,
}

#[derive(Debug)]
struct ExprBuilder<'src, 'env> {
    raw_module: &'src RawModule,
    env: &'env TypeEnv,
    blocks: &'src mut Vec<BasicBlock>,
    current_block_id: BlockId,
    locals: &'src mut Vec<Local>,
    next_local_id: &'src mut usize,
}

impl<'src, 'env> ExprBuilder<'src, 'env> {
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

    fn add_local(&mut self, name: Option<Span<String>>, ty: CHSType) -> LocalId {
        let id = LocalId(*self.next_local_id);
        *self.next_local_id += 1;
        self.locals.push(Local { name, ty });
        id
    }

    fn get_span_str(&self, span: &Span<String>) -> &'src str {
        &self.raw_module[span]
    }

    fn build_expr(&mut self, expr: hir::HIRExpr) -> Operand {
        let ty = expr.infer(self.raw_module, self.env);
        match expr {
            hir::HIRExpr::Literal(lit) => self.build_literal(lit),
            hir::HIRExpr::Identifier(id) => {
                if let Some(local) = self.locals.iter().position(|l| {
                    l.name
                        .as_ref()
                        .is_some_and(|n| self.get_span_str(n) == self.get_span_str(&id))
                }) {
                    Operand::Copy(Place {
                        local: LocalId(local),
                        projection: vec![],
                    })
                } else if let Some(f @ CHSType::Function(..)) =
                    self.env.global_get(self.get_span_str(&id))
                {
                    Operand::Global(Global::Function(id, f.clone()))
                } else {
                    todo!()
                }
            }
            hir::HIRExpr::Binary { op, lhs, rhs } => {
                let lty = lhs.infer(self.raw_module, self.env);
                let rty = rhs.infer(self.raw_module, self.env);
                // TODO: Add a error handling for ExprBuilder
                let ty = op.get_type_of_op(&lty, &rty).unwrap();

                let lhs = self.build_expr(*lhs);
                let rhs = self.build_expr(*rhs);
                let temp = self.add_local(None, ty);

                let lower_binop = self.lower_binop(op.op);
                let block = &mut self.blocks[self.current_block_id.0];

                // Special handling for pointer arithmetic
                match (&lty, &rty, op.op) {
                    (CHSType::Pointer(inner), CHSType::Int, Operator::Plus) => {
                        // Pointer + int
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::PointerArithmetic {
                                op: BinOp::Add,
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
                                op: BinOp::Add,
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
                                op: BinOp::Sub,
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
                                value: Rvalue::BinaryOp(BinOp::Sub, lhs, rhs),
                            });
                        } else {
                            panic!("Cannot subtract pointers of different types");
                        }
                    }
                    _ => {
                        // Regular binary operation
                        block.statements.push(Statement::Assign {
                            target: temp,
                            value: Rvalue::BinaryOp(lower_binop, lhs, rhs),
                        });
                    }
                }

                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::Unary { op, operand } => {
                let ty = operand.infer(self.raw_module, self.env);
                let operand = self.build_expr(*operand);
                let temp = self.add_local(None, ty);

                let lower_unop = match op.op {
                    Operator::Negate => UnOp::Neg,
                    Operator::LNot => UnOp::Not,
                    _ => panic!("Unsupported unary operator"),
                };

                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::UnaryOp(lower_unop, operand),
                });
                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::Call {
                span: _,
                callee,
                args,
            } => {
                let callee = self.build_expr(*callee);
                let args = args.into_iter().map(|arg| self.build_expr(arg)).collect();
                let not_void = ty != CHSType::Void;
                let temp = self.add_local(None, ty);
                if not_void {
                    let block = &mut self.blocks[self.current_block_id.0];
                    block.statements.push(Statement::Assign {
                        target: temp,
                        value: Rvalue::Call { func: callee, args },
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

                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Assign {
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

                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Assign {
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
            hir::HIRExpr::Assign {
                span: _,
                target,
                value,
            } => {
                let value = self.build_expr(*value);
                let target = self.build_expr(*target);

                match target {
                    Operand::Copy(place) => {
                        let block = &mut self.blocks[self.current_block_id.0];
                        block.statements.push(Statement::Store {
                            place,
                            value: Rvalue::Use(value),
                        });
                    }
                    _ => panic!("Assignment target must be a place"),
                }

                Operand::Constant(Constant::Void)
            }
            hir::HIRExpr::VarDecl { name, ty, value } => {
                let ty = if let Some(ty) = ty {
                    ty
                } else {
                    value.infer(self.raw_module, self.env)
                };
                let local = self.add_local(Some(name), ty);
                let value = self.build_expr(*value);
                let place = Place {
                    local,
                    projection: vec![],
                };
                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Store {
                    place,
                    value: Rvalue::Use(value),
                });
                Operand::Constant(Constant::Void)
            }
            hir::HIRExpr::Block(block) => {
                // Process all expressions except the last one
                let mut block_exprs = block.expressions;
                if let Some(last_expr) = block_exprs.pop() {
                    // Process all but the last expression
                    for expr in block_exprs {
                        self.build_expr(expr);
                    }
                    // The last expression is the result
                    self.build_expr(last_expr)
                } else {
                    // No expressions, return void
                    Operand::Constant(Constant::Void)
                }
            }
            hir::HIRExpr::If {
                span: _,
                condition,
                then_branch,
                else_branch: Some(else_branch),
            } => {
                let condition = self.build_expr(*condition);
                let true_block_id = BlockId(self.blocks.len());
                let false_block_id = BlockId(self.blocks.len() + 1);
                let end_block_id = BlockId(self.blocks.len() + 2);

                // Create blocks
                self.blocks.push(BasicBlock {
                    id: true_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Goto(end_block_id),
                });

                self.blocks.push(BasicBlock {
                    id: false_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Goto(end_block_id),
                });

                self.blocks.push(BasicBlock {
                    id: end_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Unreachable,
                });

                // Set up current block's terminator
                let current_block = &mut self.blocks[self.current_block_id.0];
                current_block.terminator = Terminator::Switch {
                    condition,
                    true_block: true_block_id,
                    false_block: false_block_id,
                };

                // Create a temporary local to store the result
                let result_ty = ty;
                let result_local = self.add_local(None, result_ty.clone());

                // Build then branch
                let _old_block_id = self.current_block_id;
                self.current_block_id = true_block_id;

                // Process all expressions except the last one
                let mut then_exprs = then_branch.expressions;
                let then_result = if let Some(last_expr) = then_exprs.pop() {
                    // Process all but the last expression
                    for expr in then_exprs {
                        self.build_expr(expr);
                    }
                    // The last expression is the result
                    self.build_expr(last_expr)
                } else {
                    // No expressions, use void
                    Operand::Constant(Constant::Void)
                };

                // Store the result in the temporary local
                let then_block = &mut self.blocks[true_block_id.0];
                then_block.statements.push(Statement::Store {
                    place: Place {
                        local: result_local,
                        projection: vec![],
                    },
                    value: Rvalue::Use(then_result),
                });

                // Build else branch if it exists

                self.current_block_id = false_block_id;

                // Process all expressions except the last one
                let mut else_exprs = else_branch.expressions;
                let else_result = if let Some(last_expr) = else_exprs.pop() {
                    // Process all but the last expression
                    for expr in else_exprs {
                        self.build_expr(expr);
                    }
                    // The last expression is the result
                    self.build_expr(last_expr)
                } else {
                    // No expressions, use void
                    Operand::Constant(Constant::Void)
                };

                // Store the result in the temporary local
                let else_block = &mut self.blocks[false_block_id.0];
                else_block.statements.push(Statement::Store {
                    place: Place {
                        local: result_local,
                        projection: vec![],
                    },
                    value: Rvalue::Use(else_result),
                });

                // Restore current block
                self.current_block_id = end_block_id;

                // Return the value stored in the temporary local
                Operand::Copy(Place {
                    local: result_local,
                    projection: vec![],
                })
            }
            hir::HIRExpr::If {
                span: _,
                condition,
                then_branch,
                else_branch: None,
            } => {
                let condition = self.build_expr(*condition);
                let true_block_id = BlockId(self.blocks.len());
                let end_block_id = BlockId(self.blocks.len() + 2);

                // Create blocks
                self.blocks.push(BasicBlock {
                    id: true_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Goto(end_block_id),
                });

                self.blocks.push(BasicBlock {
                    id: end_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Unreachable,
                });

                // Set up current block's terminator
                let current_block = &mut self.blocks[self.current_block_id.0];
                current_block.terminator = Terminator::Switch {
                    condition,
                    true_block: true_block_id,
                    false_block: end_block_id,
                };

                // Build then branch
                let _old_block_id = self.current_block_id;
                self.current_block_id = true_block_id;

                // Process all expressions
                for expr in then_branch.expressions {
                    self.build_expr(expr);
                }

                // Restore current block
                self.current_block_id = end_block_id;

                Operand::Constant(Constant::Void)
            }
            hir::HIRExpr::While {
                span: _,
                condition,
                body,
            } => {
                let start_block_id = self.current_block_id;
                let condition_block_id = BlockId(self.blocks.len());
                let body_block_id = BlockId(self.blocks.len() + 1);
                let end_block_id = BlockId(self.blocks.len() + 2);

                // Create blocks
                self.blocks.push(BasicBlock {
                    id: condition_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Unreachable,
                });

                self.blocks.push(BasicBlock {
                    id: body_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Goto(condition_block_id),
                });

                self.blocks.push(BasicBlock {
                    id: end_block_id,
                    statements: Vec::new(),
                    terminator: Terminator::Unreachable,
                });

                // Set up current block's terminator to go to condition block
                let current_block = &mut self.blocks[start_block_id.0];
                current_block.terminator = Terminator::Goto(condition_block_id);

                // Build condition
                self.current_block_id = condition_block_id;
                let condition = self.build_expr(*condition);
                let condition_block = &mut self.blocks[condition_block_id.0];
                condition_block.terminator = Terminator::Switch {
                    condition,
                    true_block: body_block_id,
                    false_block: end_block_id,
                };

                // Build body
                self.current_block_id = body_block_id;
                for expr in body.expressions {
                    self.build_expr(expr);
                }

                // Restore current block
                self.current_block_id = end_block_id;

                // While loops always return void
                Operand::Constant(Constant::Void)
            }
            hir::HIRExpr::Syscall {
                span: _,
                arity,
                args,
            } => {
                let args = args.into_iter().map(|arg| self.build_expr(arg)).collect();
                let ty = CHSType::Int;
                let temp = self.add_local(None, ty);

                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Assign {
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
            hir::HIRExpr::Return { span: _, expr } => {
                let value = expr.map(|e| self.build_expr(*e));
                let block = &mut self.blocks[self.current_block_id.0];
                block.terminator = Terminator::Return(value);
                Operand::Constant(Constant::Void)
            }
        }
    }

    fn build_literal(&mut self, lit: hir::HIRLiteral) -> Operand {
        match lit {
            hir::HIRLiteral::Int(span) => Operand::Constant(Constant::Int(span)),
            hir::HIRLiteral::Bool(span) => Operand::Constant(Constant::Bool(span)),
            hir::HIRLiteral::Str(span) => Operand::Constant(Constant::Str(span)),
            hir::HIRLiteral::Char(span) => Operand::Constant(Constant::Char(span)),
            hir::HIRLiteral::Void => Operand::Constant(Constant::Void),
        }
    }

    fn lower_binop(&self, op: Operator) -> BinOp {
        match op {
            Operator::Plus => BinOp::Add,
            Operator::Minus => BinOp::Sub,
            Operator::Mult => BinOp::Mul,
            Operator::Div => BinOp::Div,
            Operator::Mod => BinOp::Rem,
            Operator::BitAnd => BinOp::BitAnd,
            Operator::BitOr => BinOp::BitOr,
            Operator::Eq => BinOp::Eq,
            Operator::Lt => BinOp::Lt,
            Operator::NEq => BinOp::Ne,
            Operator::Gt => BinOp::Gt,
            Operator::LOr => BinOp::BitOr,
            Operator::LAnd => BinOp::BitAnd,
            Operator::Le => BinOp::Le,
            Operator::Ge => BinOp::Ge,
            Operator::Shl => BinOp::Shl,
            Operator::Shr => BinOp::Shr,
            Operator::BitXor => BinOp::BitXor,
            _ => panic!("Unsupported binary operator"),
        }
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
        let mut builder = ExprBuilder::new(
            raw_module,
            env,
            &mut blocks,
            current_block_id,
            &mut locals,
            &mut next_local_id,
        );

        // Build the function body
        for expr in hir_fn.body {
            builder.build_expr(expr);
        }

        // Ensure last block has proper terminator
        if matches!(blocks[0].terminator, Terminator::Unreachable) {
            blocks[0].terminator = Terminator::Return(None);
        }

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
