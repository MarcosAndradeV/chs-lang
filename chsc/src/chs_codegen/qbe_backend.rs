use qbe::*;

use crate::{
    chs_ast::{
        RawModule,
        mir::{self, *},
    },
    chs_types::CHSType,
};

pub struct QBEBackend<'src> {
    raw_module: &'src RawModule,
    module: Module<'src>,
    str_count: u32,
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
                let Local { name, ty } = &locals[local_id.0];
                let ty = self.convert_type(ty.clone());
                let val = Value::Temporary(self.raw_module[name.as_ref().unwrap()].to_string());
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
                        let Local { name, ty } = &locals[target.0];
                            let ty = self.convert_type(ty.clone());
                        if let Some(name) = name {
                            let temp = Value::Temporary(self.raw_module[name].to_string());
                            b.assign_instr(temp, ty, self.instr_from_rvalue(value, &locals));
                        } else {
                            let temp = Value::Temporary(format!("__l{}", target.0));
                            b.assign_instr(temp, ty, self.instr_from_rvalue(value, &locals));
                        }
                    }
                    mir::Statement::Store { place, value } => {
                        let (ty, temp) = self.convert_place(place, &locals);
                        b.assign_instr(temp, ty, self.instr_from_rvalue(value, &locals));
                    }
                    mir::Statement::Return(Some(operand)) => {
                        let (_, value) = self.convert_operand(operand, &locals);
                        b.add_instr(Instr::Ret(Some(value)));
                    }
                    mir::Statement::Return(None) => {
                        b.add_instr(Instr::Ret(None));
                    }
                }
            }
            match block.terminator {
                Terminator::Goto(..) => todo!(),
                Terminator::Switch { .. } => todo!(),
                Terminator::Return => {
                    b.add_comment("return");
                }
                Terminator::Unreachable => todo!(),
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
            CHSType::Int => Type::Word,
            CHSType::String => Type::Word,
            _ => todo!(),
        }
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
                Type::Zero,
                Value::Global(self.raw_module[&name].to_string()),
            ),
        }
    }

    fn convert_constant(&mut self, constant: Constant) -> (Type<'src>, Value) {
        match constant {
            Constant::Int(value) => (
                Type::Word,
                Value::Const(self.raw_module[&value].parse::<u64>().unwrap()),
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
                (Type::Word, Value::Global(v))
            }
            Constant::Bool(_) => todo!(),
            Constant::Char(_) => todo!(),
            Constant::Void => (Type::Zero, Value::Const(0)),
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
            Rvalue::UnaryOp(..) => todo!(),
            Rvalue::Call {
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
            Rvalue::Call { .. } => todo!(),
            Rvalue::Cast { .. } => todo!(),
            Rvalue::Syscall { .. } => todo!(),
            Rvalue::Index { .. } => todo!(),
            Rvalue::PointerArithmetic { .. } => todo!(),
        }
    }

    fn convert_binop(
        &mut self,
        binop: BinOp,
        op1: Operand,
        op2: Operand,
        locals: &[Local],
    ) -> Instr<'src> {
        match binop {
            BinOp::Add => {
                let (_, op1) = self.convert_operand(op1, locals);
                let (_, op2) = self.convert_operand(op2, locals);
                Instr::Add(op1, op2)
            }
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Rem => todo!(),
            BinOp::BitXor => todo!(),
            BinOp::BitAnd => todo!(),
            BinOp::BitOr => todo!(),
            BinOp::Shl => todo!(),
            BinOp::Shr => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Ge => todo!(),
            BinOp::Gt => todo!(),
        }
    }
}
