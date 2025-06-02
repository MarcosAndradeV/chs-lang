
pub mod fasm;
pub mod qbe;

pub enum CodeGenerator {
    QBE,
    Fasm,
}

impl CodeGenerator {
    pub fn generate(&self) {
        match self {
            CodeGenerator::QBE => qbe::generate(),
            CodeGenerator::Fasm => fasm::generate(),
        }
    }
}
