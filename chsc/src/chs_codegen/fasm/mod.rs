mod fasm;
use std::fs;

use fasm::Module;

use crate::chs_mir::{MIRModule, MIRModuleItem};

pub fn generate(m: MIRModule) {
    let mut module = Module::new();
    module.link_with_c(true);

    for item in m.items {
        match item {
            MIRModuleItem::ExternFunction(..) => todo!(),
            MIRModuleItem::GlobalVariable(..) => todo!(),
            MIRModuleItem::Constant(..) => todo!(),
            MIRModuleItem::Function(mirfunction) => todo!(),
        }
    }
    todo!()
}
