
use super::*;
use std::fmt:: Write;

/// Pretty printer for MIR structures
pub struct MIRPrinter {
    indent_level: usize,
    output: String,
}

impl MIRPrinter {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            output: String::new(),
        }
    }

    pub fn print_module(&mut self, module: &MIRModule) -> String {
        self.output.clear();
        self.indent_level = 0;

        writeln!(self.output, "// MIR Module").unwrap();
        writeln!(self.output).unwrap();

        // Print string literals
        if !module.string_literals.is_empty() {
            writeln!(self.output, "// String Literals").unwrap();
            for (i, literal) in module.string_literals.iter().enumerate() {
                writeln!(self.output, "str_{}: {:?}", i, literal).unwrap();
            }
            writeln!(self.output).unwrap();
        }

        // Print module items
        for item in &module.items {
            self.print_module_item(item);
            writeln!(self.output).unwrap();
        }

        self.output.clone()
    }

    fn print_module_item(&mut self, item: &MIRModuleItem) {
        match item {
            MIRModuleItem::Function(func) => self.print_function(func),
            MIRModuleItem::ExternFunction(extern_fn) => self.print_extern_function(extern_fn),
            MIRModuleItem::GlobalVariable(global) => self.print_global_variable(global),
            MIRModuleItem::Constant(constant) => self.print_constant(constant),
        }
    }

    fn print_function(&mut self, func: &MIRFunction) {
        // Function signature
        write!(self.output, "fn {}", func.name.source).unwrap();
        write!(self.output, "(").unwrap();

        for (i, &arg_id) in func.args.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ").unwrap();
            }
            let local = &func.locals[arg_id.0];
            if let Some(name) = &local.name {
                write!(self.output, "{}: ", name.source).unwrap();
            } else {
                write!(self.output, "{}: ", arg_id).unwrap();
            }
            write!(self.output, "{}", local.ty).unwrap();
        }
        write!(self.output, ")").unwrap();

        if let Some(ret_type) = &func.return_type {
            write!(self.output, " -> {}", ret_type).unwrap();
        }

        writeln!(self.output, " {{").unwrap();
        self.indent_level += 1;

        // Print locals section
        if !func.locals.is_empty() {
            self.write_indent();
            writeln!(self.output, "// Locals").unwrap();
            for (i, local) in func.locals.iter().enumerate() {
                if func.args.contains(&LocalId(i)) {
                    continue;
                }
                self.write_indent();
                write!(self.output, "let _{}: {}", i, local.ty).unwrap();
                if let Some(name) = &local.name {
                    write!(self.output, " // {}", name.source).unwrap();
                }
                writeln!(self.output, ";").unwrap();
            }
            writeln!(self.output).unwrap();
        }

        // Print basic blocks
        for (i, block) in func.body.iter().enumerate() {
            self.print_block(BlockId(i), block);
        }

        self.indent_level -= 1;
        writeln!(self.output, "}}").unwrap();
    }

    fn print_extern_function(&mut self, extern_fn: &MIRExternFunction) {
        write!(self.output, "extern fn {}", extern_fn.name.source).unwrap();
        write!(self.output, "(").unwrap();

        for (i, arg_type) in extern_fn.args.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ").unwrap();
            }
            write!(self.output, "{}", arg_type).unwrap();
        }

        if extern_fn.is_variadic {
            if !extern_fn.args.is_empty() {
                write!(self.output, ", ").unwrap();
            }
            write!(self.output, "...").unwrap();
        }

        write!(self.output, ")").unwrap();
        writeln!(self.output, " -> {};", extern_fn.return_type).unwrap();
    }

    fn print_global_variable(&mut self, global: &MIRGlobalVariable) {
        write!(self.output, "static {}: {} = ",
               global.name.source, global.ty).unwrap();
        self.print_operand(&global.initializer);
        writeln!(self.output, ";").unwrap();
    }

    fn print_constant(&mut self, constant: &MIRConstant) {
        write!(self.output, "const {}: {} = ",
               constant.name.source, constant.ty).unwrap();
        self.print_operand(&constant.value);
        writeln!(self.output, ";").unwrap();
    }

    fn print_block(&mut self, block_id: BlockId, block: &MIRBlock) {
        self.write_indent();
        writeln!(self.output, "{}:", block_id).unwrap();
        self.indent_level += 1;

        // Print operations
        for op in &block.operations {
            self.write_indent();
            self.print_operation(op);
            writeln!(self.output).unwrap();
        }

        // Print terminator
        if let Some(terminator) = &block.terminator {
            self.write_indent();
            self.print_terminator(terminator);
            writeln!(self.output).unwrap();
        } else {
            self.write_indent();
            writeln!(self.output, "// No terminator").unwrap();
        }

        self.indent_level -= 1;
        writeln!(self.output).unwrap();
    }

    fn print_operation(&mut self, op: &MIROperation) {
        match op {
            MIROperation::Assign { target, value } => {
                write!(self.output, "{} = ", target).unwrap();
                self.print_rvalue(value);
                write!(self.output, ";").unwrap();
            }
            MIROperation::Store { address, value } => {
                write!(self.output, "*").unwrap();
                self.print_operand(address);
                write!(self.output, " = ").unwrap();
                self.print_operand(value);
                write!(self.output, ";").unwrap();
            }
            MIROperation::Load { target, address } => {
                write!(self.output, "{} = *", target).unwrap();
                self.print_operand(address);
                write!(self.output, ";").unwrap();
            }
            MIROperation::Nop => {
                write!(self.output, "nop;").unwrap();
            }
            MIROperation::Assert { cond, message } => {
                write!(self.output, "assert(").unwrap();
                self.print_operand(cond);
                if let Some(msg) = message {
                    write!(self.output, ", {:?}", msg).unwrap();
                }
                write!(self.output, ");").unwrap();
            }
        }
    }

    fn print_terminator(&mut self, terminator: &Terminator) {
        match terminator {
            Terminator::Return(None) => {
                write!(self.output, "return;").unwrap();
            }
            Terminator::Return(Some(value)) => {
                write!(self.output, "return ").unwrap();
                self.print_operand(value);
                write!(self.output, ";").unwrap();
            }
            Terminator::Branch { cond, true_block, false_block } => {
                write!(self.output, "if ").unwrap();
                self.print_operand(cond);
                write!(self.output, " {{ goto {}; }} else {{ goto {}; }}",
                       true_block, false_block).unwrap();
            }
            Terminator::Goto(block) => {
                write!(self.output, "goto {};", block).unwrap();
            }
            Terminator::Unreachable => {
                write!(self.output, "unreachable;").unwrap();
            }
        }
    }

    fn print_rvalue(&mut self, rvalue: &RValue) {
        match rvalue {
            RValue::Use(operand) => {
                self.print_operand(operand);
            }
            RValue::BinaryOp { op, left, right } => {
                write!(self.output, "{:?}(", op).unwrap();
                self.print_operand(left);
                write!(self.output, ", ").unwrap();
                self.print_operand(right);
                write!(self.output, ")").unwrap();
            }
            RValue::UnaryOp { op, operand } => {
                write!(self.output, "{:?}(", op).unwrap();
                self.print_operand(operand);
                write!(self.output, ")").unwrap();
            }
            RValue::AddressOf { place } => {
                write!(self.output, "&").unwrap();
                self.print_operand(place);
            }
            RValue::Deref(operand) => {
                write!(self.output, "*").unwrap();
                self.print_operand(operand);
            }
            RValue::Cast { operand, target_type } => {
                write!(self.output, "(").unwrap();
                self.print_operand(operand);
                write!(self.output, " as {})", target_type).unwrap();
            }
            RValue::FieldAccess { base, field } => {
                self.print_operand(base);
                write!(self.output, ".{}", field).unwrap();
            }
            RValue::Index { base, index } => {
                self.print_operand(base);
                write!(self.output, "[").unwrap();
                self.print_operand(index);
                write!(self.output, "]").unwrap();
            }
            RValue::Call { func, args } => {
                self.print_operand(func);
                write!(self.output, "(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    self.print_operand(arg);
                }
                write!(self.output, ")").unwrap();
            }
            RValue::Aggregate { kind, fields } => {
                match kind {
                    AggregateKind::Array(ty) => {
                        write!(self.output, "[{}; ", ty).unwrap();
                    }
                    AggregateKind::Struct(name) => {
                        write!(self.output, "{} {{ ", name.source).unwrap();
                    }
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    self.print_operand(field);
                }
                match kind {
                    AggregateKind::Array(_) => write!(self.output, "]").unwrap(),
                    AggregateKind::Struct(_) => write!(self.output, " }}").unwrap(),
                }
            }
        }
    }

    fn print_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Local(id) => {
                write!(self.output, "{}", id).unwrap();
            }
            Operand::Constant(constant) => {
                self.print_constant_value(constant);
            }
            Operand::Function(name) => {
                write!(self.output, "{}", name.source).unwrap();
            }
        }
    }

    fn print_constant_value(&mut self, constant: &Constant) {
        match constant {
            Constant::Int(value) => write!(self.output, "{}", value).unwrap(),
            Constant::UInt(value) => write!(self.output, "{}u", value).unwrap(),
            Constant::Float(value) => write!(self.output, "{}", value).unwrap(),
            Constant::Bool(value) => write!(self.output, "{}", value).unwrap(),
            Constant::Char(value) => write!(self.output, "'{}'", value).unwrap(),
            Constant::String(id) => write!(self.output, "str_{}", id).unwrap(),
        }
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            write!(self.output, "    ").unwrap();
        }
    }
}

impl Default for MIRPrinter {
    fn default() -> Self {
        Self::new()
    }
}

// Convenience functions for quick printing
impl MIRModule {
    pub fn print(&self) -> String {
        let mut printer = MIRPrinter::new();
        printer.print_module(self)
    }
}

impl MIRFunction {
    pub fn print(&self) -> String {
        let mut printer = MIRPrinter::new();
        printer.print_function(self);
        printer.output
    }
}

impl MIRBlock {
    pub fn print(&self, block_id: BlockId) -> String {
        let mut printer = MIRPrinter::new();
        printer.print_block(block_id, self);
        printer.output
    }
}
