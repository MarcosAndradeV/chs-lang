use chs_ast::{
    RawModule,
    mir::{
        BasicBlock, BinOp, Constant, LocalId, MIRFunction, MIRModule, Operand, Place,
        ProjectionElem, Rvalue, Statement, Terminator, UnOp,
    },
};
use chs_types::CHSType;

use crate::fasm::{
    self, Addr, DataDef, DataDirective, DataExpr, Function, Instr, Module as FasmModule, Register,
    SizeOperator, Value,
};

pub struct MIRTranslator<'src> {
    raw_module: &'src RawModule,
    module: FasmModule,
    temp_function: Option<Function>,
    // Map MIR locals to stack offsets
    locals_map: Vec<i32>,
    // Keep track of string literals
    string_counter: usize,
}

impl<'src> MIRTranslator<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        let mut module = FasmModule::new();
        module.link_with_c(true);
        Self {
            raw_module,
            module,
            temp_function: None,
            locals_map: Vec::new(),
            string_counter: 0,
        }
    }

    pub fn translate_module(mut self, mir: MIRModule) -> FasmModule {
        for item in mir.items {
            match item {
                chs_ast::mir::MIRModuleItem::Function(f) => {
                    self.translate_function(f);
                }
                chs_ast::mir::MIRModuleItem::ExternFunction(_) => {
                    // Handle extern functions if needed
                }
            }
        }
        self.module
    }

    fn translate_function(&mut self, func: MIRFunction) {
        let name = self.raw_module[&func.name].to_string();
        let mut asm_fn = Function::new(name);

        // Map function parameters to their stack locations
        self.locals_map.clear();

        // Calculate stack space needed for locals
        let mut stack_size = 0;
        for local in &func.locals {
            let size = self.size_of_type(&local.ty);
            stack_size += size;
            self.locals_map.push(-(stack_size as i32));
        }

        // Align stack to 16 bytes
        if stack_size % 16 != 0 {
            stack_size = (stack_size + 15) & !15;
        }

        asm_fn.allocate_stack(stack_size);
        self.temp_function = Some(asm_fn);

        // Store blocks and terminator locally to avoid multiple mutable borrows
        let blocks = func.blocks;
        for block in blocks {
            self.translate_block(block);
        }

        if let Some(fn_) = self.temp_function.take() {
            self.module.push_function(fn_);
        }
    }

    fn translate_block(&mut self, block: BasicBlock) {
        let block_label = format!("block_{}", block.id.0);
        if let Some(ref mut fn_) = self.temp_function {
            fn_.push_block(&block_label);

            // Store statements and terminator locally to avoid multiple mutable borrows
            let statements = block.statements;
            let terminator = block.terminator;

            // Translate statements
            for stmt in statements {
                self.translate_statement(stmt);
            }

            // Translate terminator
            self.translate_terminator(terminator);
        }
    }

    fn translate_statement(&mut self, stmt: Statement) {
        let mut instrs = vec![];
        match stmt {
            Statement::Assign { target, value } => {
                // Get target location
                let target_reg = self.get_local_address(target);

                // Translate Rvalue and store result
                match value {
                    Rvalue::Use(op) => {
                        let src = self.translate_operand(op);
                        instrs.push(Instr::Mov(target_reg, src));
                    }
                    Rvalue::BinaryOp(op, lhs, rhs) => {
                        let lhs_val = self.translate_operand(lhs);
                        let rhs_val = self.translate_operand(rhs);
                        instrs.push(Instr::Mov(target_reg.clone(), lhs_val));

                        match op {
                            BinOp::Add => instrs.push(Instr::Add(target_reg, rhs_val)),
                            BinOp::Sub => instrs.push(Instr::Sub(target_reg, rhs_val)),
                            BinOp::Mul => {
                                instrs.push(Instr::Push(Value::Register(Register::Rax)));
                                instrs.push(Instr::Push(Value::Register(Register::Rdx)));
                                instrs.push(Instr::Mov(
                                    Value::Register(Register::Rax),
                                    target_reg.clone(),
                                ));
                                instrs.push(Instr::Mul(rhs_val));
                                instrs.push(Instr::Mov(target_reg, Value::Register(Register::Rax)));
                                instrs.push(Instr::Pop(Value::Register(Register::Rdx)));
                                instrs.push(Instr::Pop(Value::Register(Register::Rax)));
                            }
                            // Add other binary operations...
                            _ => todo!("Binary op: {:?}", op),
                        }
                    }
                    Rvalue::UnaryOp(op, val) => {
                        let val = self.translate_operand(val);
                        instrs.push(Instr::Mov(target_reg.clone(), val));
                        match op {
                            UnOp::Not => instrs.push(Instr::Not(target_reg)),
                            UnOp::Neg => instrs.push(Instr::Neg(target_reg)),
                        }
                    }
                    Rvalue::Call { func, args } => {
                        // Save caller-saved registers
                        for reg in &[Register::Rax, Register::Rcx, Register::Rdx] {
                            instrs.push(Instr::Push(Value::Register(*reg)));
                        }

                        let is_empty = args.is_empty();
                        let len = args.len();

                        // Push arguments in reverse order
                        for arg in args {
                            let arg_val = self.translate_operand(arg);
                            instrs.push(Instr::Push(arg_val));
                        }

                        // Call function
                        let func_val = self.translate_operand(func);
                        match func_val {
                            Value::Label(label) => instrs.push(Instr::Call(label)),
                            _ => panic!("Expected function label"),
                        }

                        // Cleanup stack
                        if !is_empty {
                            instrs.push(Instr::Add(
                                Value::Register(Register::Rsp),
                                Value::Const(SizeOperator::Qword, (len * 8) as i64),
                            ));
                        }

                        // Move result to target
                        instrs.push(Instr::Mov(target_reg, Value::Register(Register::Rax)));

                        // Restore caller-saved registers
                        for reg in &[Register::Rdx, Register::Rcx, Register::Rax] {
                            instrs.push(Instr::Pop(Value::Register(*reg)));
                        }
                    }
                    // Add other Rvalue cases...
                    _ => todo!("Rvalue: {:?}", value),
                }
            }
            Statement::Store { place, value } => {
                let addr = self.translate_place(place);
                let value = match value {
                    Rvalue::Use(op) => self.translate_operand(op),
                    _ => todo!("Complex store value"),
                };
                instrs.push(Instr::Mov(addr, value));
            }
        }
        if let Some(ref mut fn_) = self.temp_function {
            fn_.extend_instr(instrs);
        }
    }

    fn translate_terminator(&mut self, terminator: Terminator) {
        let mut instrs = Vec::new();
        match terminator {
            Terminator::Goto(target) => {
                instrs.push(Instr::Jmp(format!("block_{}", target.0)));
            }
            Terminator::Return(value) => {
                if let Some(val) = value {
                    let val = self.translate_operand(val);
                    instrs.push(Instr::Mov(Value::Register(Register::Rax), val));
                    instrs.push(Instr::Raw("pop rbp".to_string()));
                }
                instrs.push(Instr::Ret);
            }
            Terminator::Switch {
                condition,
                true_block,
                false_block,
            } => {
                let cond = self.translate_operand(condition);
                instrs.push(Instr::Cmp(cond, Value::Const(SizeOperator::Byte, 0)));
                instrs.push(Instr::J(fasm::Cond::NZ, format!("block_{}", true_block.0)));
                instrs.push(Instr::Jmp(format!("block_{}", false_block.0)));
            }
            Terminator::Unreachable => {
                // TODO: Add a trap instruction
            }
        }
        if let Some(ref mut fn_) = self.temp_function {
            fn_.extend_instr(instrs);
        }
    }

    fn translate_operand(&mut self, op: Operand) -> Value {
        match op {
            Operand::Constant(c) => self.translate_constant(c),
            Operand::Copy(place) | Operand::Move(place) => self.translate_place(place),
        }
    }

    fn translate_constant(&mut self, constant: Constant) -> Value {
        match constant {
            Constant::Int(span) => Value::Const(
                SizeOperator::Qword,
                self.raw_module[&span]
                    .parse()
                    .expect("Probably bug in lexer"),
            ),
            Constant::Bool(span) => Value::Const(
                SizeOperator::Byte,
                self.raw_module[&span]
                    .parse::<bool>()
                    .expect("Probably bug in lexer") as i64,
            ),
            Constant::Str(span) => {
                let label = format!("str_{}", self.string_counter);
                self.string_counter += 1;
                self.module.push_data(DataDef::new(
                    &label,
                    DataDirective::Db,
                    vec![DataExpr::Str(self.raw_module[&span].to_string())],
                ));
                Value::Label(label)
            }
            Constant::Char(span) => Value::Const(
                SizeOperator::Byte,
                self.raw_module[&span]
                    .parse::<char>()
                    .expect("Probably bug in lexer") as u8 as i64,
            ),
            Constant::Void => todo!(),
        }
    }

    fn translate_place(&mut self, place: Place) -> Value {
        let base_addr = self.get_local_address(place.local);

        // If no projections, return the base address
        if place.projection.is_empty() {
            return base_addr;
        }
        let mut instrs = vec![];
        // Handle projections (pointer dereference, field access, indexing)
        let temp_reg = Register::Rax; // Should use proper register allocation

        instrs.push(Instr::Mov(Value::Register(temp_reg), base_addr));

        for proj in place.projection {
            match proj {
                ProjectionElem::Deref => {
                    instrs.push(Instr::Mov(
                        Value::Register(temp_reg),
                        Value::Memory(SizeOperator::Qword, Addr::Base(temp_reg)),
                    ));
                }
                ProjectionElem::Field(idx) => {
                    // Assume field offset calculation based on idx
                    instrs.push(Instr::Add(
                        Value::Register(temp_reg),
                        Value::Const(SizeOperator::Qword, (idx * 8) as i64),
                    ));
                }
                ProjectionElem::Index(idx) => {
                    let idx_val = self.translate_operand(idx);
                    instrs.push(Instr::Push(Value::Register(Register::Rcx)));
                    instrs.push(Instr::Mov(Value::Register(Register::Rcx), idx_val));
                    instrs.push(Instr::Add(
                        Value::Register(temp_reg),
                        Value::Register(Register::Rcx),
                    ));
                    instrs.push(Instr::Pop(Value::Register(Register::Rcx)));
                }
            }
        }

        if let Some(ref mut fn_) = self.temp_function {
            fn_.extend_instr(instrs);
        }
        Value::Memory(SizeOperator::Qword, Addr::Base(temp_reg))
    }

    fn get_local_address(&self, local: LocalId) -> Value {
        let offset = self.locals_map[local.0];
        Value::Memory(
            SizeOperator::Qword,
            Addr::BaseDisplacement(Register::Rbp, offset),
        )
    }

    fn size_of_type(&self, ty: &CHSType) -> usize {
        match ty {
            CHSType::Int => 8,
            CHSType::Boolean => 1,
            CHSType::Char => 1,
            CHSType::Void => 0,
            CHSType::String => 8, // pointer size
            CHSType::Pointer(_) => 8,
            CHSType::Function(_, _) => 8, // function pointer
            _ => todo!("Size of type: {:?}", ty),
        }
    }
}
