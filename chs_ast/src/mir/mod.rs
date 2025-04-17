use chs_lexer::Span;
use chs_types::CHSInfer as _;
use chs_types::CHSType;
use chs_types::TypeEnv;

use crate::hir;
use crate::nodes::Operator;
use crate::RawModule;

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
}

impl MIRModuleItem {
    pub fn from_hir(raw_module: &RawModule, item: hir::HIRModuleItem, env: &TypeEnv) -> Self {
        match item {
            hir::HIRModuleItem::Function(f) => {
                Self::Function(MIRFunction::from_hir(raw_module, f, env))
            }
            hir::HIRModuleItem::ExternFunction(_) => todo!(),
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
    Syscall { number: Operand, args: Vec<Operand> },
}

#[derive(Debug)]
pub enum Operand {
    /// A constant value
    Constant(Constant),
    /// A copy of a place's value
    Copy(Place),
    /// Move a place's value
    Move(Place),
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
        match expr {
            hir::HIRExpr::Literal(lit) => self.build_literal(lit),
            hir::HIRExpr::Identifier(id) => {
                let local = self
                    .locals
                    .iter()
                    .position(|l| {
                        l.name
                            .as_ref()
                            .is_some_and(|n| self.get_span_str(n) == self.get_span_str(&id))
                    })
                    .expect("Expect bound identifier");
                Operand::Copy(Place {
                    local: LocalId(local),
                    projection: vec![],
                })
            }
            hir::HIRExpr::Binary { op, lhs, rhs } => {
                let lty = lhs.infer(self.env);
                let rty = rhs.infer(self.env);
                // TODO: Add a error handling for ExprBuilder
                let ty = op.get_type_of_op(lty, rty).unwrap();

                let lhs = self.build_expr(*lhs);
                let rhs = self.build_expr(*rhs);
                let temp = self.add_local(None, ty);

                let lower_binop = self.lower_binop(op);
                let block = &mut self.blocks[self.current_block_id.0];
                block.statements.push(Statement::Assign {
                    target: temp,
                    value: Rvalue::BinaryOp(lower_binop, lhs, rhs),
                });
                Operand::Copy(Place {
                    local: temp,
                    projection: vec![],
                })
            }
            hir::HIRExpr::VarDecl { name, ty, value } => {
                let ty = if let Some(ty) = ty {
                    ty
                } else {
                    value.infer(self.env)
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
            hir::HIRExpr::Return(expr) => {
                let value = expr.map(|e| self.build_expr(*e));
                let block = &mut self.blocks[self.current_block_id.0];
                block.terminator = Terminator::Return(value);
                Operand::Constant(Constant::Void)
            }
            _ => todo!("Implement remaining HIR expression types"),
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
            Operator::And => BinOp::BitAnd,
            Operator::Or => BinOp::BitOr,
            Operator::Eq => BinOp::Eq,
            Operator::Lt => BinOp::Lt,
            Operator::NEq => BinOp::Ne,
            Operator::Gt => BinOp::Gt,
            _ => todo!("Implement remaining binary operators"),
        }
    }
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
