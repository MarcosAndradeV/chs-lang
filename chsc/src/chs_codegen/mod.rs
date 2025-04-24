use core::fmt;

use qbe_backend::QBEBackend;

use crate::{chs_ast::mir::MIRModule, chs_util::CHSResult};

pub mod fasm_backend;
pub mod qbe_backend;

pub enum Backend {
    Fasm,
    Qbe,
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Backend::Fasm => write!(f, "Fasm"),
            Backend::Qbe => write!(f, "Qbe"),
        }
    }
}

impl Backend {
    pub fn generate_module(&self, m: MIRModule) -> CHSResult<GenCode> {
        match self {
            Backend::Qbe => {
                let mut q = QBEBackend::new(m.raw_module);
                q.generate_module(m);
                Ok(GenCode { data: q.into_qbe_module().to_string() })
            },
            Backend::Fasm => todo!(),
        }
    }
}

/// GenCode holds the data to be write into the file
pub struct GenCode {
    data: String
}

impl fmt::Display for GenCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}
