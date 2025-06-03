use std::path::PathBuf;

use crate::{chs_mir::{MIRModule, MIRModuleItem}, chs_util::CHSResult};


pub mod fasm;
pub mod qbe;

#[derive(Debug, Clone)]
pub enum CodeGenerator {
    QBE,
    Fasm,
    Mir,
}

impl CodeGenerator {
    pub fn generate(self, m: MIRModule, input_path: PathBuf, output_path: PathBuf) -> CHSResult<()>{
        match self {
            CodeGenerator::QBE => qbe::generate  (m, input_path, output_path),
            CodeGenerator::Fasm => {
                fasm::generate(m, input_path, output_path)?;
                // fs::write(&asm_path, module.to_string());
                // if module.get_link_with_c() {
                //     output_path = output_path.with_extension("o");
                // }
                // run_fasm(&asm_path, &output_path)?;
                // if module.get_link_with_c() {
                //     run_cc(vec![], output_path, asm_path)?;
                // }
                Ok(())
            },
            CodeGenerator::Mir => {
                println!("{}", m.print());
                Ok(())
            }
        }
    }
}
