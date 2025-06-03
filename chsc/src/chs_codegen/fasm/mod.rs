mod fasm;
use std::{fs, path::PathBuf};

use fasm::Module;

use crate::{
    chs_codegen::fasm::fasm::{Function, Instr, Register, SizeOperator, Value},
    chs_mir::{MIRModule, MIRModuleItem},
    chs_util::{CHSResult, binary_exists, run_cc, run_fasm},
};

pub fn generate(m: MIRModule, input_path: PathBuf, mut output_path: PathBuf) -> CHSResult<Module> {
    if !binary_exists("fasm") {
        eprintln!("[ERROR] qbe binary not found. Please install it.");
        std::process::exit(1);
    }
    let asm_path = input_path.with_extension("asm");
    let mut module = Module::new();

    for item in m.items {
        match item {
            MIRModuleItem::ExternFunction(..) => todo!(),
            MIRModuleItem::GlobalVariable(..) => todo!(),
            MIRModuleItem::Constant(..) => todo!(),
            MIRModuleItem::Function(f) => {
                let f = module.push_function(Function::new(true, f.name.source));
                f.push_block("start");
                f.push_instr(Instr::Mov(
                    Value::Register(Register::Rax),
                    Value::Const(SizeOperator::Qword, 0),
                ));
            }
        }
    }

    Ok(module)
}
