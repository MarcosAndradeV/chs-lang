#![allow(unused)]
#![allow(dead_code)]

use qbe;

use crate::{
    chs_ast::{
        RawModule,
        mir::{
            Constant, Global, Local, LocalId, MIRExternFunction, MIRFunction, MIRModule,
            MIRModuleItem, Operand, Place, ProjectionElem, Rvalue, Statement, Terminator,
        },
        nodes::Operator,
    },
    chs_lexer::{Span, Token},
    chs_types::CHSType,
};

pub struct QBEBackend<'src> {
    raw_module: &'src RawModule,
    module: qbe::Module<'src>,
    str_count: u32,
}

macro_rules! temp {
    ($message:literal, $($field: expr),*) => {
       qbe::Value::Temporary(format!($message, $($field),*))
    };
}

impl<'src> QBEBackend<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        Self {
            raw_module,
            module: qbe::Module::new(),
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
            fn_type,
            params,
            return_type,
            blocks,
            locals,
        } = f;

        let mut b = qbe::Block {
            label: format!("start"),
            items: vec![],
        };

        for (i, local) in locals.iter().enumerate() {
            let ty = convert_type(&local.ty);
            let size = ty.size();
            b.assign_instr(temp!("s{}", i), qbe::Type::Long, qbe::Instr::Alloc8(size));
            if let Some(p) = params.get(i) {
                b.add_instr(qbe::Instr::Store(ty, temp!("s{}", p.0), temp!("t{}", p.0)));
            }
        }

        b.add_instr(qbe::Instr::Jmp(format!("b0")));

        let name = self.get_span_str(&name);
        let linkage = qbe::Linkage::public();
        let arguments = self.generate_arguments(params, &locals);
        let return_ty = if return_type == CHSType::Void {
            None
        } else {
            Some(convert_type(&return_type))
        };
        let mut qf = qbe::Function::new(linkage, name, arguments, return_ty);
        qf.blocks.push(b);
        for block in blocks {
            let mut b = qbe::Block {
                label: format!("b{}", block.id.0),
                items: vec![],
            };
            for stmt in block.statements {
                match stmt {
                    Statement::Call { func, args } => {
                        if let Operand::Global(Global::Function(
                            func,
                            CHSType::Function(..) | CHSType::VariadicFunction(..),
                        )) = func
                        {
                            let args_ty: Vec<_> = args
                                .iter()
                                .map(|operand| self.get_opreand_ty(&locals, operand))
                                .collect();
                            let args = args_ty
                                .into_iter()
                                .zip(args)
                                .map(|(ty, arg)| (ty, self.generate_opreand(&mut b, arg, &locals)))
                                .collect();
                            let func = self.get_span_str(&func);
                            // TODO Fix varidic calls
                            b.add_instr(qbe::Instr::Call(func.to_string(), args, None));
                        } else {
                            todo!()
                        }
                    }
                    Statement::Assign { target, value } => {
                        let Local { name, ty } = &locals[target.0];
                        let ty = convert_type(&ty);
                        let value = self.generate_rvalue(&mut b, value, &locals);
                        let instr = qbe::Instr::Store(ty, temp!("s{}", target.0), value);
                        b.add_instr(instr);
                    }
                    Statement::Store { place, value } if place.projection.is_empty() => {
                        let Local { name, ty } = &locals[place.local.0];
                        let ty = convert_type(&ty);
                        let value = self.generate_rvalue(&mut b, value, &locals);
                        let instr = qbe::Instr::Store(ty, temp!("s{}", place.local.0), value);
                        b.add_instr(instr);
                    }
                    Statement::Store { place, value } => todo!(),
                }
            }
            match block.terminator {
                Terminator::Goto(block_id) => {
                    b.add_instr(qbe::Instr::Jmp(format!("b{}", block_id.0)))
                }
                Terminator::Switch {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let value = self.generate_opreand(&mut b, condition, &locals);
                    b.add_instr(qbe::Instr::Jnz(
                        value,
                        format!("b{}", true_block.0),
                        format!("b{}", false_block.0),
                    ))
                }
                Terminator::Return(Some(operand)) => {
                    let value = self.generate_opreand(&mut b, operand, &locals);
                    b.add_instr(qbe::Instr::Ret(Some(value)))
                }
                Terminator::Return(None) => b.add_instr(qbe::Instr::Ret(None)),
                Terminator::Unreachable => todo!(),
                Terminator::Nop => todo!(),
            }
            qf.blocks.push(b);
        }

        self.module.add_function(qf);
    }

    fn generate_rvalue(
        &mut self,
        b: &mut qbe::Block<'_>,
        rvalue: Rvalue,
        locals: &[Local],
    ) -> qbe::Value {
        match rvalue {
            Rvalue::Use(operand) => self.generate_opreand(b, operand, locals),
            Rvalue::BinaryOp(ty, operator, ty1, operand1, ty2, operand2) => {
                self.generate_binop(b, ty, operator, ty1, operand1, ty2, operand2, locals)
            }
            Rvalue::UnaryOp(ty, operator, oparand_ty, operand) => {
                self.generate_unop(b, ty, operator, oparand_ty, operand, locals)
            }
            Rvalue::FunCall { func, args } => {
                if let Operand::Global(Global::Function(
                    func,
                    CHSType::Function(_, ret_ty) | CHSType::VariadicFunction(_, ret_ty),
                )) = func
                {
                    let ret_ty = convert_type(&ret_ty);
                    let args_ty: Vec<_> = args
                        .iter()
                        .map(|operand| self.get_opreand_ty(&locals, operand))
                        .collect();
                    let args = args_ty
                        .into_iter()
                        .zip(args)
                        .map(|(ty, arg)| (ty, self.generate_opreand(b, arg, locals)))
                        .collect();
                    let func = self.get_span_str(&func);
                    // TODO Fix varidic calls
                    b.assign_instr(
                        temp!("a1",),
                        ret_ty,
                        qbe::Instr::Call(func.to_string(), args, None),
                    );
                    temp!("a1",)
                } else {
                    todo!()
                }
            }
            Rvalue::Cast { value, target_ty } => {
                let ty = convert_type(&target_ty);
                let instr = match (self.get_opreand_ty(locals, &value), &ty) {
                    (qbe::Type::Long, qbe::Type::Long) => {
                        return self.generate_opreand(b, value, locals);
                    }
                    _ => todo!("Other casts"),
                };
                b.assign_instr(temp!("a1",), ty, instr);
                temp!("a1",)
            }
            Rvalue::Syscall { number, args } => todo!(),
            Rvalue::Index { base, index } => todo!(),
            Rvalue::PointerArithmetic {
                op,
                pointer,
                offset,
                pointee_ty,
            } => todo!(),
        }
    }

    fn get_opreand_ty<'a, 'b>(&'a self, locals: &'a [Local], operand: &Operand) -> qbe::Type<'b> {
        match operand {
            Operand::Constant(constant) => match constant {
                Constant::I32(..) | Constant::U32(..) | Constant::Bool(..) => qbe::Type::Word,
                Constant::Str(..) | Constant::I64(..) | Constant::U64(..) => qbe::Type::Long,
                Constant::Char(..) => todo!(),
                Constant::Void => todo!(),
            },
            Operand::Copy(Place { local, projection }) if projection.is_empty() => {
                convert_type(&locals[local.0].ty)
            }
            // Operand::Copy(Place { local, projection }) if projection.len() == 1 => {
            //     let proj = &projection[0];
            //     match proj {
            //         ProjectionElem::Deref => {
            //             convert_type(&locals[local.0].ty)
            //         }
            //         ProjectionElem::Field(_) => todo!(),
            //         ProjectionElem::Index(operand) => todo!(),
            //     }
            // },
            Operand::Copy(..) => todo!(),
            Operand::Move(..) => todo!(),
            Operand::Global(..) => qbe::Type::Long,
        }
    }

    fn generate_opreand(
        &mut self,
        b: &mut qbe::Block<'_>,
        operand: Operand,
        locals: &[Local],
    ) -> qbe::Value {
        match operand {
            Operand::Constant(constant) => match constant {
                Constant::I32(span) => {
                    let imm = self.get_span_str(&span).parse::<i32>().unwrap() as u64;
                    qbe::Value::Const(imm)
                }
                Constant::U32(span) => todo!(),
                Constant::I64(span) => {
                    let imm = self.get_span_str(&span).parse::<i64>().unwrap() as u64;
                    qbe::Value::Const(imm)
                }
                Constant::U64(span) => {
                    let imm = self.get_span_str(&span).parse::<u64>().unwrap();
                    qbe::Value::Const(imm)
                }
                Constant::Bool(span) => {
                    let imm = if self.get_span_str(&span).parse::<bool>().unwrap() {
                        1
                    } else {
                        0
                    };
                    qbe::Value::Const(imm)
                }
                Constant::Str(span) => {
                    let name = format!("str{}", self.str_count);
                    self.str_count += 1;
                    let data = qbe::DataDef::new(
                        qbe::Linkage::private(),
                        name.clone(),
                        Some(8),
                        vec![
                            (
                                qbe::Type::Byte,
                                qbe::DataItem::Str(self.get_span_str(&span).to_string()),
                            ),
                            (qbe::Type::Byte, qbe::DataItem::Const(0)),
                        ],
                    );
                    self.module.add_data(data);
                    qbe::Value::Global(name)
                }
                Constant::Char(span) => todo!(),
                Constant::Void => todo!(),
            },
            Operand::Copy(Place { local, projection }) if projection.is_empty() => {
                let ty = convert_type(&locals[local.0].ty);
                let instr = qbe::Instr::Load(ty.clone(), temp!("s{}", local.0));
                b.assign_instr(temp!("t{}", local.0), ty, instr);
                temp!("t{}", local.0)
            }
            Operand::Copy(place) => todo!(),
            Operand::Move(place) => todo!(),
            Operand::Global(global) => todo!(),
        }
    }

    fn declare_extern_function(&mut self, f: MIRExternFunction) {}

    pub fn into_qbe_module(self) -> qbe::Module<'src> {
        self.module
    }

    fn generate_arguments(
        &self,
        params: Vec<LocalId>,
        locals: &[Local],
    ) -> Vec<(qbe::Type<'src>, qbe::Value)> {
        let mut arguments: Vec<(qbe::Type<'src>, qbe::Value)> = Vec::new();
        for param in params {
            let Local { name, ty } = &locals[param.0];
            let ty = convert_type(&ty);
            arguments.push((ty, temp!("t{}", param.0)));
        }
        arguments
    }

    fn generate_unop(
        &mut self,
        b: &mut qbe::Block<'_>,
        ty: CHSType,
        operator: Operator,
        oparand_ty: CHSType,
        operand: Operand,
        locals: &[Local],
    ) -> qbe::Value {
        let ty = convert_type(&oparand_ty);
        let size = ty.size();
        match operator {
            Operator::Negate => todo!(),
            Operator::LNot => todo!(),
            Operator::Refer => match &operand {
                Operand::Copy(place) => temp!("s{}", place.local.0),
                Operand::Constant(constant) => {
                    b.assign_instr(temp!("p1",), qbe::Type::Long, qbe::Instr::Alloc8(size));
                    let operand = self.generate_opreand(b, operand, locals);
                    b.add_instr(qbe::Instr::Store(ty, temp!("p1",), operand));
                    temp!("p1",)
                }
                Operand::Move(place) => todo!(),
                Operand::Global(global) => todo!(),
            },
            Operator::Deref => {
                let operand = self.generate_opreand(b, operand, locals);
                b.assign_instr(temp!("p1",), ty.clone(), qbe::Instr::Load(ty, operand));
                temp!("p1",)
            }
            _ => todo!(),
        }
    }

    fn generate_binop(
        &mut self,
        b: &mut qbe::Block<'_>,
        ty: CHSType,
        operator: Operator,
        ty1: CHSType,
        operand1: Operand,
        ty2: CHSType,
        operand2: Operand,
        locals: &[Local],
    ) -> qbe::Value {
        let s = get_type_sign(&ty1);
        let operand1 = self.generate_opreand(b, operand1, locals);
        let operand2 = self.generate_opreand(b, operand2, locals);
        match operator {
            Operator::Plus => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    qbe::Instr::Add(operand1, operand2),
                );
                temp!("a1",)
            }
            Operator::Minus => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    qbe::Instr::Sub(operand1, operand2),
                );
                temp!("a1",)
            }
            Operator::Div => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    match s {
                        TypeSign::Signed => qbe::Instr::Div(operand1, operand2),
                        TypeSign::UnSigned => qbe::Instr::Udiv(operand1, operand2),
                    },
                );
                temp!("a1",)
            }
            Operator::Mult => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    qbe::Instr::Mul(operand1, operand2),
                );
                temp!("a1",)
            }
            Operator::Mod => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    match s {
                        TypeSign::Signed => qbe::Instr::Rem(operand1, operand2),
                        TypeSign::UnSigned => qbe::Instr::Urem(operand1, operand2),
                    },
                );
                temp!("a1",)
            }
            Operator::LOr | Operator::BitOr => {
                b.assign_instr(
                    temp!("a1",),
                    convert_type(&ty),
                    qbe::Instr::Or(operand1, operand2),
                );
                temp!("a1",)
            }
            Operator::LAnd => todo!(),
            Operator::BitAnd => todo!(),
            Operator::Eq => {
                b.assign_instr(
                    temp!("c1",),
                    convert_type(&ty),
                    qbe::Instr::Cmp(convert_type(&ty1), qbe::Cmp::Eq, operand1, operand2),
                );
                temp!("c1",)
            }
            Operator::NEq => todo!(),
            Operator::Gt => {
                b.assign_instr(
                    temp!("c1",),
                    convert_type(&ty),
                    qbe::Instr::Cmp(
                        convert_type(&ty1),
                        match s {
                            TypeSign::Signed => qbe::Cmp::Sgt,
                            TypeSign::UnSigned => qbe::Cmp::Ugt,
                        },
                        operand1,
                        operand2,
                    ),
                );
                temp!("c1",)
            }
            Operator::Lt => {
                b.assign_instr(
                    temp!("c1",),
                    convert_type(&ty),
                    qbe::Instr::Cmp(
                        convert_type(&ty1),
                        match s {
                            TypeSign::Signed => qbe::Cmp::Slt,
                            TypeSign::UnSigned => qbe::Cmp::Ult,
                        },
                        operand1,
                        operand2,
                    ),
                );
                temp!("c1",)
            }
            Operator::Assign => todo!(),
            Operator::Le => todo!(),
            Operator::Ge => todo!(),
            Operator::Shl => todo!(),
            Operator::Shr => todo!(),
            Operator::BitXor => todo!(),
            _ => todo!(),
        }
    }
}

fn convert_type<'a, 'b>(chsty: &'a CHSType) -> qbe::Type<'b> {
    match chsty {
        CHSType::I32 | CHSType::U32 => qbe::Type::Word,
        CHSType::I64 | CHSType::U64 => qbe::Type::Word,
        CHSType::Boolean => qbe::Type::Word,
        CHSType::Pointer(..) | CHSType::String => qbe::Type::Long,
        _ => todo!("Not supported type"),
    }
}

fn get_type_sign(chsty: &CHSType) -> TypeSign {
    match chsty {
        CHSType::I32 | CHSType::I64 => TypeSign::Signed,
        CHSType::U32 | CHSType::U64 => TypeSign::UnSigned,
        _ => TypeSign::UnSigned,
    }
}

#[derive(Clone, Copy)]
enum TypeSign {
    Signed,
    UnSigned,
}
