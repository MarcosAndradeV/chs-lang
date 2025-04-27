use std::collections::HashMap;

use crate::{
    chs_ast::hir::{
        HIRExpr, HIRExternFunction, HIRFunction, HIRLiteral, HIRModule, HIRModuleItem, HIRStmt,
    },
    chs_lexer::Span,
    chs_types::CHSType,
    chs_util::*,
    return_chs_error,
};

use super::*;

type Scope<'a> = HashMap<&'a str, Addr>;

pub struct SymbolTable<'a> {
    scopes: Vec<Scope<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: &'a str, addr: Addr) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, addr);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Addr> {
        for scope in self.scopes.iter().rev() {
            if let Some(addr) = scope.get(name) {
                return Some(*addr);
            }
        }
        None
    }
}

impl<'src> MIRModule<'src> {
    pub fn from_hir(hir: HIRModule<'src>) -> CHSResult<Self> {
        let mut mir = Self {
            raw_module: hir.raw_module,
            items: vec![],
        };

        for item in hir.items {
            match item {
                HIRModuleItem::Function(f) => mir.add_function(f)?,
                HIRModuleItem::ExternFunction(f) => mir.add_extern_function(f)?,
            }
        }

        Ok(mir)
    }

    fn add_extern_function(&mut self, f: HIRExternFunction) -> CHSResult<()> {
        self.items
            .push(MIRModuleItem::ExternFunction(MIRExternFunction {
                name: f.name,
                fn_type: f.fn_type,
            }));
        Ok(())
    }

    fn add_function(&mut self, f: HIRFunction) -> CHSResult<()> {
        let mut builder = MIRBuilder::new(f.name, f.fn_type);

        let mut sym_table = SymbolTable::new();

        for param in f.params {
            let name = self.get_span_str(&param.name);
            let addr = builder.alloc_local(param.param_type);
            builder.add_arg(addr);
            sym_table.insert(name, addr);
        }

        for stmt in f.body.statements {
            self.lower_stmt(&mut builder, &mut sym_table, stmt)?;
        }
        builder.finalize_blocks();
        self.items.push(MIRModuleItem::Function(builder.function));
        Ok(())
    }

    fn resolve_variable(
        &self,
        target: &HIRExpr,
        _builder: &mut MIRBuilder,
        sym_table: &SymbolTable,
    ) -> CHSResult<Addr> {
        match target {
            HIRExpr::Identifier(span, _) => {
                let name = self.get_span_str(&span);
                if let Some(addr) = sym_table.lookup(name) {
                    Ok(addr)
                } else {
                    return_chs_error!(
                        "{}:{}: Undefined identfier {} found in HIR lowering.",
                        self.get_file_path(),
                        span.loc,
                        name
                    )
                }
            }
            _ => return_chs_error!("TODO: AAAAAAAAAA"),
        }
    }

    #[allow(unused)]
    fn lower_stmt(
        &mut self,
        builder: &mut MIRBuilder,
        sym_table: &mut SymbolTable<'src>,
        stmt: HIRStmt,
    ) -> Result<(), CHSError> {
        match stmt {
            HIRStmt::VarDecl { name, ty, value } => {
                let Some(ty) = ty else {
                    return_chs_error!(
                        "{}:{}: VarDecl type inference missing for {}.",
                        self.get_file_path(),
                        name.loc,
                        self.get_span_str(&name)
                    );
                };
                let addr = builder.alloc_local(ty.clone());

                let rhs = self.lower_expr(builder, *value, sym_table)?;
                builder.emit(MIROperation::Assignment {
                    target: LocalAddress(addr),
                    value: rhs,
                });

                sym_table.insert(self.get_span_str(&name), addr);
            }
            HIRStmt::Assign {
                span: _,
                target,
                value,
            } => {
                let addr = self.resolve_variable(target.as_ref(), builder, &sym_table)?;
                let rhs = self.lower_expr(builder, *value, sym_table)?;
                builder.emit(MIROperation::Assignment {
                    target: LocalAddress(addr),
                    value: rhs,
                });
            }
            HIRStmt::If {
                span: _,
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.lower_expr(builder, *condition, sym_table)?;

                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let end_block = if else_branch.is_some() {
                    builder.create_block()
                } else {
                    else_block
                };

                builder.set_terminator(Terminator::Branch {
                    cond,
                    true_block: then_block,
                    false_block: if else_branch.is_some() {
                        else_block
                    } else {
                        end_block
                    },
                });

                builder.switch_to_block(then_block);
                sym_table.enter_scope();
                for stmt in then_branch.statements {
                    self.lower_stmt(builder, sym_table, stmt)?;
                }
                sym_table.exit_scope();
                if !builder.function.body[then_block.0].is_terminated() {
                    builder.set_terminator(Terminator::Goto(end_block));
                } else {
                    return Ok(());
                }

                if let Some(else_branch) = else_branch {
                    builder.switch_to_block(else_block);
                    sym_table.enter_scope();
                    for stmt in else_branch.statements {
                        self.lower_stmt(builder, sym_table, stmt)?;
                    }
                    sym_table.exit_scope();
                    if !builder.function.body[else_block.0].is_terminated() {
                        builder.set_terminator(Terminator::Goto(end_block));
                    } else {
                        return Ok(());
                    }
                }

                builder.switch_to_block(end_block);
            }
            HIRStmt::While {
                span: _,
                condition,
                body,
            } => {
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                builder.set_terminator(Terminator::Goto(cond_block));

                builder.switch_to_block(cond_block);
                let cond = self.lower_expr(builder, *condition, sym_table)?;
                builder.set_terminator(Terminator::Branch {
                    cond,
                    true_block: body_block,
                    false_block: exit_block,
                });

                builder.switch_to_block(body_block);
                sym_table.enter_scope();
                for stmt in body.statements {
                    self.lower_stmt(builder, sym_table, stmt)?;
                }
                sym_table.exit_scope();
                if !builder.function.body[body_block.0].is_terminated() {
                    builder.set_terminator(Terminator::Goto(cond_block));
                } else {
                    return Ok(());
                }

                builder.switch_to_block(exit_block);
            }
            HIRStmt::Return { span: _, expr } => {
                let ret_val = expr
                    .map(|e| self.lower_expr(builder, *e, sym_table))
                    .transpose()?;
                builder.set_terminator(Terminator::Return(ret_val));
            }
            HIRStmt::Funcall {
                ty,
                span,
                callee,
                args,
            } => {
                let name = {
                    match *callee {
                        HIRExpr::Identifier(span, _) => self.get_span_str(&span).to_string(),
                        _ => todo!(),
                    }
                };
                let mut mir_args = vec![];
                for arg in args {
                    mir_args.push(self.lower_expr(builder, arg, sym_table)?);
                }
                builder.emit(MIROperation::FuncCall {
                    target: None,
                    name,
                    args: mir_args,
                });
            }
            HIRStmt::ExprStmt { .. } => todo!(),
        }
        Ok(())
    }

    fn lower_expr_refer(
        &self,
        builder: &mut MIRBuilder,
        value: Box<HIRExpr>,
        sym_table: &mut SymbolTable<'src>,
    ) -> CHSResult<Operand> {
        let inner_operand = self.lower_expr(builder, *value, sym_table)?;

        match inner_operand {
            Operand::Address(addr) => Ok(Operand::Address(addr)),
            Operand::Value(_) => {
                return_chs_error!(
                    "{}: Cannot take reference to temporary value directly.",
                    self.get_file_path()
                )
            }
            Operand::Literal(lit, _) => {
                return_chs_error!(
                    "{}:{}: Cannot take reference of a literal.",
                    self.get_file_path(),
                    lit.span().loc
                )
            }
        }
    }

    fn lower_expr_deref(
        &self,
        builder: &mut MIRBuilder,
        value: Box<HIRExpr>,
        sym_table: &mut SymbolTable<'src>,
    ) -> CHSResult<Operand> {
        let addr_operand = self.lower_expr(builder, *value, sym_table)?;

        match &addr_operand {
            Operand::Address(addr) => {
                let var_type = &builder.function.locals[addr.0.index];
                let ty = match var_type.get_pointee_type() {
                    Some(inner) => inner.clone(),
                    None => {
                        return_chs_error!(
                            "{}: Cannot dereference non-pointer local type: {:?}",
                            self.get_file_path(),
                            var_type
                        );
                    }
                };
                let target = builder.alloc_local(ty);

                builder.emit(MIROperation::Load {
                    target: LocalValue(target),
                    addr: Operand::Address(*addr),
                });

                Ok(Operand::Value(LocalValue(target)))
            }
            Operand::Value(addr) => {
                let var_type = &builder.function.locals[addr.0.index];
                let ty = match var_type.get_pointee_type() {
                    Some(inner) => inner.clone(),
                    None => {
                        return_chs_error!(
                            "{}: Cannot dereference non-pointer local type: {:?}",
                            self.get_file_path(),
                            var_type
                        );
                    }
                };
                let target = builder.alloc_local(ty);

                builder.emit(MIROperation::Load {
                    target: LocalValue(target),
                    addr: Operand::Value(*addr),
                });

                Ok(Operand::Value(LocalValue(target)))
            }
            Operand::Literal(_, _) => {
                return_chs_error!(
                    "{}: Trying to dereference a non-address operand: {:?}",
                    self.get_file_path(),
                    addr_operand
                );
            }
        }
    }

    fn lower_expr(
        &self,
        builder: &mut MIRBuilder,
        value: HIRExpr,
        sym_table: &mut SymbolTable<'src>,
    ) -> CHSResult<Operand> {
        match value {
            HIRExpr::Literal(HIRLiteral::Int(span), chsty) => {
                let Some(chsty) = chsty else {
                    return_chs_error!(
                        "{}:{}: Untyped int {} found in HIR lowering.",
                        self.get_file_path(),
                        span.loc,
                        self.get_span_str(&span)
                    )
                };
                Ok(Operand::Literal(MIRLiteral::Int(span), chsty))
            }
            HIRExpr::Literal(HIRLiteral::Str(span), chsty) => {
                Ok(Operand::Literal(MIRLiteral::Str(span), chsty.unwrap()))
            }
            HIRExpr::Literal(HIRLiteral::Bool(span), chsty) => {
                Ok(Operand::Literal(MIRLiteral::Bool(span), chsty.unwrap()))
            }
            HIRExpr::Identifier(span, _) => {
                let name = self.get_span_str(&span);
                if let Some(addr) = sym_table.lookup(name) {
                    Ok(Operand::Address(LocalAddress(addr)))
                } else {
                    return_chs_error!(
                        "{}:{}: Undefined identfier {} found in HIR lowering.",
                        self.get_file_path(),
                        span.loc,
                        name
                    )
                }
            }
            HIRExpr::Binary { ty, op, lhs, rhs } => {
                let ty = ty.unwrap(); // SAFETY already infered
                let lhs = self.lower_expr(builder, *lhs, sym_table)?;
                let rhs = self.lower_expr(builder, *rhs, sym_table)?;
                let target = builder.alloc_local(ty);
                builder.emit(MIROperation::Binop {
                    target: LocalValue(target),
                    op,
                    lhs,
                    rhs,
                });
                Ok(Operand::Value(LocalValue(target)))
            }
            HIRExpr::Unary {
                ref op, operand, ..
            } if op.is_refer() => self.lower_expr_refer(builder, operand, sym_table),
            HIRExpr::Unary {
                ref op, operand, ..
            } if op.is_deref() => self.lower_expr_deref(builder, operand, sym_table),
            HIRExpr::Unary { ty, op, operand } => {
                let ty = ty.unwrap(); // SAFETY already infered
                let operand = self.lower_expr(builder, *operand, sym_table)?;
                let target = builder.alloc_local(ty);
                builder.emit(MIROperation::Unop {
                    target: LocalValue(target),
                    op,
                    operand,
                });
                Ok(Operand::Value(LocalValue(target)))
            }
            HIRExpr::Call {
                ty,
                span: _,
                callee,
                args,
            } => {
                let ty = ty.unwrap();
                let name = {
                    match *callee {
                        HIRExpr::Identifier(span, _) => self.get_span_str(&span).to_string(),
                        _ => todo!(),
                    }
                };
                let mut mir_args = vec![];
                for arg in args {
                    mir_args.push(self.lower_expr(builder, arg, sym_table)?);
                }
                let target = builder.alloc_local(ty);
                builder.emit(MIROperation::FuncCall {
                    target: Some(LocalValue(target)),
                    name,
                    args: mir_args,
                });
                Ok(Operand::Value(LocalValue(target)))
            }
            _ => todo!("{value:?}"),
        }
    }
}

#[derive(Debug)]
pub struct MIRBuilder {
    pub function: MIRFunction,
    pub current_block: BlockId,
}

impl MIRBuilder {
    pub fn new(name: Span<String>, fn_type: CHSType) -> Self {
        let mut function = MIRFunction::new(name, fn_type);
        function.body.push(MIRBlock::default());
        MIRBuilder {
            function,
            current_block: BlockId(0),
        }
    }

    /// Allocates a new local variable and returns its address
    pub fn alloc_local(&mut self, ty: CHSType) -> Addr {
        let index = self.function.locals.len();
        self.function.locals.push(ty.clone());
        Addr { index }
    }

    /// Emit a new operation into the current block
    pub fn emit(&mut self, op: MIROperation) {
        self.function.body[self.current_block.0].operations.push(op);
    }

    /// Set the terminator of the current block
    pub fn set_terminator(&mut self, terminator: Terminator) {
        self.function.body[self.current_block.0].terminator = Some(terminator);
    }

    /// Create a new block and switch to it
    pub fn create_block(&mut self) -> BlockId {
        let new_block_id = BlockId(self.function.body.len());
        self.function.body.push(MIRBlock::default());
        eprintln!("[DEBUG] Creating a new block with id: {:?}", new_block_id);
        new_block_id
    }

    /// Switch to an existing block
    pub fn switch_to_block(&mut self, block: BlockId) {
        eprintln!("[DEBUG] Switching to block with id: {:?}", block);
        self.current_block = block;
    }

    fn add_arg(&mut self, addr: Addr) {
        self.function.args.push(addr);
    }

    pub fn finalize_blocks(&mut self) {
        let mut reachable = vec![false; self.function.body.len()];
        self.mark_reachable_blocks(BlockId(0), &mut reachable);

        let len = self.function.body.len();
        for (i, block) in self.function.body.iter_mut().enumerate() {
            if !reachable[i] {
                block.terminator = Some(Terminator::Unreachable);
            } else if block.terminator.is_none() {
                // Default fallthrough (if none set), jump to next block if it exists
                if i + 1 < len {
                    block.terminator = Some(Terminator::Goto(BlockId(i + 1)));
                } else {
                    block.terminator = Some(Terminator::Unreachable);
                }
            }
        }
    }

    fn mark_reachable_blocks(&self, start: BlockId, reachable: &mut Vec<bool>) {
        let mut worklist = vec![start];

        while let Some(block_id) = worklist.pop() {
            let idx = block_id.0;
            if idx >= self.function.body.len() || reachable[idx] {
                continue;
            }
            reachable[idx] = true;

            match &self.function.body[idx].terminator {
                Some(Terminator::Goto(target)) => worklist.push(*target),
                Some(Terminator::Branch {
                    true_block,
                    false_block,
                    ..
                }) => {
                    worklist.push(*true_block);
                    worklist.push(*false_block);
                }
                _ => {}
            }
        }
    }
}
