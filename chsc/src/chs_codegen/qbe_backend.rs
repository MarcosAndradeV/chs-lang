use qbe::*;

use crate::{
    chs_ast::{
        ModuleImpl, RawModule,
        mir::{self, *},
        nodes::Operator,
    },
    chs_lexer::{Span, Token},
    chs_types::CHSType,
};

pub struct QBEBackend<'src> {
    raw_module: &'src RawModule,
    module: Module<'src>,
    str_count: u32,
}

impl<'src> ModuleImpl<'src> for QBEBackend<'src> {
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

impl<'src> QBEBackend<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        Self {
            raw_module,
            module: Module::new(),
            str_count: 0,
        }
    }

    pub fn generate_module(&mut self, mir_module: MIRModule) {
        for item in mir_module.items {
            match item {
                MIRModuleItem::Function(f) => self.generate_function(f),
                MIRModuleItem::ExternFunction(f) => self.declare_extern_function(f),
            }
        }
    }

    fn generate_function(&mut self, f: MIRFunction) {
        let MIRFunction {
            name,
            fn_type: _,
            params,
            return_type,
            blocks,
            locals,
        } = f;
        let name = &self.raw_module[&name];
        let arguments = params
            .into_iter()
            .map(|local_id| {
                let Local { name: _, ty } = &locals[local_id.0];
                let ty = self.convert_type(ty.clone());
                let val = Value::Temporary(format!("__l{}", local_id.0));
                (ty, val)
            })
            .collect();
        let return_ty = {
            if return_type == CHSType::Void {
                None
            } else {
                Some(self.convert_type(return_type))
            }
        };
        let mut func = Function::new(Linkage::public(), name, arguments, return_ty);

        for block in blocks {
            let mut b = Block {
                label: format!("b{}", block.id.0),
                items: Vec::new(),
            };
            for stmt in block.statements {
                match stmt {
                    mir::Statement::Assign { target, value } => {
                        let Local { name: _, ty } = &locals[target.0];
                        let ty = self.convert_type(ty.clone());

                        let temp = Value::Temporary(format!("__l{}", target.0));
                        let instr = self.instr_from_rvalue(value, &locals);
                        b.assign_instr(temp, ty, instr);
                    }
                    mir::Statement::Store { place, value } => {
                        let (ty, temp) = self.convert_place(place, &locals);
                        let instr = self.instr_from_rvalue(value, &locals);
                        b.assign_instr(temp, ty, instr);
                    }
                    mir::Statement::Call {
                        func: Operand::Global(Global::Function(func, _)),
                        args,
                    } => {
                        let func = self.raw_module[&func].to_string();
                        let args = args
                            .into_iter()
                            .map(|operand| self.convert_operand(operand, &locals))
                            .collect();
                        b.add_instr(Instr::Call(func, args, None))
                    }
                    mir::Statement::Call { .. } => todo!(),
                }
            }
            match block.terminator {
                Terminator::Goto(bid) => {
                    b.add_instr(Instr::Jmp(format!("b{}", bid.0)));
                }
                Terminator::Switch {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let (_, value) = self.convert_operand(condition, &locals);
                    b.add_instr(Instr::Jnz(
                        value,
                        format!("b{}", true_block.0),
                        format!("b{}", false_block.0),
                    ));
                }
                Terminator::Return(Some(operand)) => {
                    let (_, value) = self.convert_operand(operand, &locals);
                    b.add_instr(Instr::Ret(Some(value)));
                }
                Terminator::Return(None) => {
                    b.add_instr(Instr::Ret(None));
                }
                _ => todo!(),
            }
            func.blocks.push(b);
        }
        self.module.add_function(func);
    }

    fn convert_place(&mut self, place: Place, locals: &[Local]) -> (Type<'src>, Value) {
        let Local { name: _, ty } = &locals[place.local.0];
        let temp = Value::Temporary(format!("__l{}", place.local.0));
        let ty = self.convert_type(ty.clone());
        (ty, temp)
    }

    fn declare_extern_function(&self, _f: MIRExternFunction) {
        //
    }

    fn convert_type(&self, ty: CHSType) -> Type<'src> {
        match ty {
            CHSType::I32 | CHSType::U32 => Type::Word,
            CHSType::I64 | CHSType::U64 => Type::Long,
            CHSType::String => Type::Word,
            CHSType::Boolean => Type::Byte,
            _ => todo!("Implement type: {:?}", ty),
        }
        .into_abi()
    }

    fn convert_operand(&mut self, operand: Operand, locals: &[Local]) -> (Type<'src>, Value) {
        match operand {
            Operand::Constant(constant) => self.convert_constant(constant),
            Operand::Copy(place) => {
                if place.projection.is_empty() {
                    self.convert_place(place, locals)
                } else {
                    todo!()
                }
            }
            Operand::Move(..) => todo!(),
            Operand::Global(Global::Function(name, _)) => (
                Type::Long,
                Value::Global(self.raw_module[&name].to_string()),
            ),
        }
    }

    fn convert_constant(&mut self, constant: Constant) -> (Type<'src>, Value) {
        match constant {
            Constant::I32(value) => (
                Type::Word.into_abi(),
                Value::Const(self.raw_module[&value].parse::<i32>().unwrap() as u64),
            ),
            Constant::U32(value) => (
                Type::Word.into_abi(),
                Value::Const(self.raw_module[&value].parse::<u32>().unwrap() as u64),
            ),
            Constant::I64(value) => (
                Type::Long.into_abi(),
                Value::Const(self.raw_module[&value].parse::<i64>().unwrap() as u64),
            ),
            Constant::U64(value) => (
                Type::Long.into_abi(),
                Value::Const(self.raw_module[&value].parse::<u64>().unwrap() as u64),
            ),
            Constant::Str(s) => {
                let s = self.raw_module[&s].to_string();
                let str_count = self.str_count;
                self.str_count += 1;
                let v = format!("str{}", str_count);
                let items = vec![
                    (Type::Byte, DataItem::Str(s)),
                    (Type::Byte, DataItem::Const(0)),
                ];
                self.module
                    .add_data(DataDef::new(Linkage::private(), &v, None, items));
                (Type::Long, Value::Global(v))
            }
            Constant::Bool(value) => (
                Type::Byte.into_abi(),
                Value::Const(if self.raw_module[&value].parse::<bool>().unwrap() {
                    1
                } else {
                    0
                }),
            ),
            Constant::Char(_) => todo!(),
            Constant::Void => (Type::Zero.into_abi(), Value::Const(0)),
        }
    }

    pub fn finish(self) -> Module<'src> {
        self.module
    }

    fn instr_from_rvalue(&mut self, value: Rvalue, locals: &[Local]) -> Instr<'src> {
        match value {
            Rvalue::Use(operand) => {
                let (_, value) = self.convert_operand(operand, locals);
                Instr::Copy(value)
            }
            Rvalue::BinaryOp(binop, op1, op2) => self.convert_binop(binop, op1, op2, locals),
            Rvalue::UnaryOp(unop, op1) => self.convert_unop(unop, op1, locals),
            Rvalue::FunCall {
                func: Operand::Global(Global::Function(func, _)),
                args,
            } => {
                let func = self.raw_module[&func].to_string();
                let args = args
                    .into_iter()
                    .map(|operand| self.convert_operand(operand, locals))
                    .collect();
                Instr::Call(func, args, None)
            }
            Rvalue::FunCall { .. } => todo!(),
            Rvalue::Cast { value, target_ty } => {
                let (ty, value) = self.convert_operand(value, locals);
                match (&ty, &target_ty) {
                    (Type::Word, CHSType::I32) => Instr::Copy(value),
                    (Type::Word, CHSType::U32) => Instr::Copy(value),
                    (Type::Word, CHSType::U64) => Instr::Extuw(value),
                    (Type::Word, CHSType::I64) => Instr::Extsw(value),
                    _ => todo!("Implement cast between types {} and {}", ty, target_ty),
                }
            }
            Rvalue::Syscall { .. } => todo!(),
            Rvalue::Index { .. } => todo!(),
            Rvalue::PointerArithmetic { .. } => todo!(),
        }
    }

    fn convert_binop(
        &mut self,
        binop: Operator,
        op1: Operand,
        op2: Operand,
        locals: &[Local],
    ) -> Instr<'src> {
        let (ty1, op1) = self.convert_operand(op1, locals);
        let (ty2, op2) = self.convert_operand(op2, locals);

        match binop {
            Operator::Plus => Instr::Add(op1, op2),
            Operator::Minus => Instr::Sub(op1, op2),
            Operator::Mult => Instr::Mul(op1, op2),
            Operator::Div => Instr::Div(op1, op2),
            Operator::Mod => Instr::Rem(op1, op2),
            Operator::BitOr | Operator::LOr => Instr::Or(op1, op2),
            Operator::Eq => {
                debug_assert_eq!(
                    ty1, ty2,
                    "TODO: Implement comparison instructions for different types"
                );
                Instr::Cmp(ty1, Cmp::Eq, op1, op2)
            }
            Operator::Lt => {
                debug_assert_eq!(
                    ty1, ty2,
                    "TODO: Implement comparison instructions for different types"
                );
                Instr::Cmp(ty1, Cmp::Slt, op1, op2)
            }
            Operator::Le => todo!(),
            Operator::NEq => Instr::Cmp(ty1, Cmp::Ne, op1, op2),
            Operator::Ge => todo!(),
            Operator::Gt => {
                debug_assert_eq!(
                    ty1, ty2,
                    "TODO: Implement comparison instructions for different types"
                );
                Instr::Cmp(ty1, Cmp::Sgt, op1, op2)
            }
            _ => todo!("TODO: Implemente binop {binop}"),
        }
    }

    fn convert_unop(&mut self, unop: Operator, op1: Operand, locals: &[Local]) -> Instr<'src> {
        let (_, op1) = self.convert_operand(op1, locals);

        match unop {
            Operator::Negate | Operator::LNot => {
                Instr::Cmp(Type::Word, Cmp::Eq, op1, Value::Const(0))
            }
            _ => todo!("TODO: Implemente unop {unop}"),
        }
    }
}
