use crate::chs_mir::MIRModule;


pub mod fasm;
pub mod qbe;

pub enum CodeGenerator {
    QBE,
    Fasm,
}

impl CodeGenerator {
    pub fn generate(self, m: MIRModule) {
        match self {
            CodeGenerator::QBE => qbe::generate(m),
            CodeGenerator::Fasm => fasm::generate(m),
        }
    }
}
