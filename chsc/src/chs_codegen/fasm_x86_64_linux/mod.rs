use std::{fs, path::PathBuf};

use crate::{
    chs_ast::ast::BinaryOperator,
    chs_codegen::fasm_x86_64_linux::fasm::{
        Addr, Function, Instr, Module, Register, SizeOperator, Value,
    },
    chs_mir::{Constant, MIRModule, MIRModuleItem, MIROperation, Operand, RValue, Terminator},
};
mod fasm;

pub fn generate(output_path: &PathBuf, mir_module: MIRModule) {
    let mut module = Module::new();
    module.set_link_with_c(true);

    for func in mir_module.func {
        let mut f = Function::new(true, func.name.source);
        f.set_epiloge(false);
        f.allocate_stack(func.locals.len() * 8);
        for (i, block) in func.body.into_iter().enumerate() {
            f.push_block(format!("b{i}"));
            for op in block.operations {
                match op {
                    MIROperation::Assign { target, value } => {
                        let loc = Addr::BaseDisplacement(Register::Rbp, -(target.0 as i32) * 8);
                        let val: Value = rvalue_to_value(&mut f, value);
                        f.push_raw_instr(format!("mov [{loc}], {val}"));
                    }
                    MIROperation::Store { address, value } => todo!(),
                    MIROperation::Load { target, address } => todo!(),
                    MIROperation::Nop => todo!(),
                    MIROperation::Assert { cond, message } => todo!(),
                }
            }
            match block.terminator {
                Some(Terminator::Return(o)) => {
                    if let Some(o) = o {
                        mov_operand_to_reg(&mut f, Register::Rax, o);
                    }
                    f.push_raw_instr("mov rsp, rbp");
                    f.push_raw_instr("pop rbp");
                    f.push_raw_instr("ret");
                }
                Some(Terminator::Branch { .. }) => todo!(),
                Some(Terminator::Unreachable) => todo!(),
                Some(Terminator::Goto(_)) => todo!(),
                None => todo!(),
            }
        }
        module.push_function(f);
    }

    fs::write(output_path, module.to_string()).expect("");
}

fn rvalue_to_value(f: &mut Function, value: RValue) -> Value {
    match value {
        RValue::Use(operand) => todo!(),
        RValue::BinaryOp { op, left, right } => match op {
            BinaryOperator::Add => {
                mov_operand_to_reg(f, Register::Rax, left);
                mov_operand_to_reg(f, Register::Rbx, right);
                f.push_raw_instr(format!("add rax, rbx"));
                Value::Register(Register::Rax)
            }
            BinaryOperator::Subtract => todo!(),
            _ => todo!(),
        },
        RValue::UnaryOp { op, operand } => todo!(),
        RValue::AddressOf { place } => todo!(),
        RValue::Deref(operand) => todo!(),
        RValue::Cast {
            operand,
            target_type,
        } => todo!(),
        RValue::FieldAccess { base, field } => todo!(),
        RValue::Index { base, index } => todo!(),
        RValue::Call { func, args } => todo!(),
        RValue::Aggregate { kind, fields } => todo!(),
    }
}

fn mov_operand_to_reg(f: &mut Function, reg: Register, o: Operand) {
    match o {
        Operand::Local(local_id) => {
            f.push_raw_instr(format!(
                "mov {reg}, [{}]",
                Addr::BaseDisplacement(Register::Rbp, -(local_id.0 as i32) * 8)
            ));
        }
        Operand::Constant(constant) => match constant {
            Constant::Int(i) => {
                f.push_raw_instr(format!("mov {reg}, {i}"));
            }
            Constant::UInt(_) => todo!(),
            Constant::Float(_) => todo!(),
            Constant::Bool(_) => todo!(),
            Constant::Char(_) => todo!(),
            Constant::String(_) => todo!(),
        },
        Operand::Function(token) => todo!(),
    }
}
