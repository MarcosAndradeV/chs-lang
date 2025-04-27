use super::*;
use std::fmt::Write as _;

pub struct MIRPrinter<'src> {
    raw_module: &'src RawModule,
}

impl<'src> ModuleImpl<'src> for MIRPrinter<'src> {
    fn get_span_str<T>(&self, span: &Span<T>) -> &'src str {
        &self.raw_module[span]
    }

    fn get_token_str(&self, token: &Token) -> &'src str {
        &self.raw_module[token]
    }

    fn get_file_path(&self) -> &'src str {
        &self.raw_module.file_path
    }
}

impl<'src> MIRPrinter<'src> {
    pub fn new(raw_module: &'src RawModule) -> Self {
        Self { raw_module }
    }

    pub fn print_module(self, m: &MIRModule) -> String {
        let mut output = String::new();
        for item in m.items.iter() {
            match item {
                MIRModuleItem::Function(f) => {
                    self.print_function(&mut output, f);
                }
                MIRModuleItem::ExternFunction(f) => {
                    writeln!(
                        output,
                        "extern fn {} : {}\n",
                        self.get_span_str(&f.name),
                        f.fn_type
                    )
                    .unwrap();
                }
            }
        }
        output
    }

    fn print_function(&self, output: &mut String, func: &MIRFunction) {
        let args_str = func.args
            .iter()
            .map(|addr| format!("{} : {}", self.format_addr(addr), func.locals[addr.index]))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            output,
            "fn {}({}) : {} {{",
            self.get_span_str(&func.name),
            args_str,
            func.fn_type
        )
        .unwrap();

        for (block_id, block) in func.body.iter().enumerate() {
            writeln!(output, ":b{block_id}").unwrap();
            for op in block.operations.iter() {
                writeln!(output, "    {}", self.format_operation(op, &func.locals)).unwrap();
            }

            if let Some(terminator) = &block.terminator {
                writeln!(output, "    {}", self.format_terminator(terminator)).unwrap();
            } else {
                writeln!(output, "    Incomplete").unwrap();
            }
        }

        writeln!(output, "}}").unwrap();
    }

    pub fn format_operation(&self, op: &MIROperation, locals: &[CHSType]) -> String {
        match op {
            MIROperation::Assignment { target, value } => {
                format!(
                    "{} : {} = {}",
                    self.format_addr(&target.0),
                    locals[target.0.index],
                    self.format_operand(value)
                )
            }
            MIROperation::Binop {
                target,
                op,
                lhs,
                rhs,
            } => {
                format!(
                    "{} : {} = {} {} {}",
                    self.format_addr(&target.0),
                    locals[target.0.index],
                    self.format_operand(lhs),
                    op,
                    self.format_operand(rhs)
                )
            }
            MIROperation::Unop { target, op, operand: op1 } => {
                format!(
                    "{} : {} = {}{}",
                    self.format_addr(&target.0),
                    locals[target.0.index],
                    op,
                    self.format_operand(op1)
                )
            }
            MIROperation::Refer { target, addr } => {
                format!("{} : {} = addr_of {}", self.format_addr(target), locals[target.index], self.format_addr(addr))
            }
            MIROperation::Load { target, addr } => {
                format!("{} : {} = load {}", self.format_addr(&target.0), locals[target.0.index], self.format_operand(addr))
            }
            MIROperation::FuncCall { target, name, args } => {
                let args_str = args
                    .iter()
                    .map(|addr| self.format_operand(addr))
                    .collect::<Vec<_>>()
                    .join(", ");
                match target {
                    Some(tgt) => format!(
                        "{} = {}({})",
                        self.format_addr(&tgt.0),
                        name,
                        args_str
                    ),
                    None => format!("{}({})", name, args_str),
                }
            }
        }
    }

    pub fn format_terminator(&self, term: &Terminator) -> String {
        match term {
            Terminator::Return(Some(op)) => format!("return {}", self.format_operand(op)),
            Terminator::Return(None) => "return".to_string(),
            Terminator::Branch {
                cond,
                true_block,
                false_block,
            } => {
                format!(
                    "if ({}) goto b{} else goto b{}",
                    self.format_operand(cond),
                    true_block.0,
                    false_block.0
                )
            }
            Terminator::Goto(block) => format!("goto b{}", block.0),
        }
    }

    pub fn format_operand(&self, op: &Operand) -> String {
        match op {
            Operand::Literal(lit, _) => self.format_literal(lit),
            Operand::Value(addr) => self.format_addr(&addr.0),
            Operand::Address(addr) => self.format_addr(&addr.0),
        }
    }

    pub fn format_literal(&self, lit: &MIRLiteral) -> String {
        match lit {
            MIRLiteral::Int(span) => self.get_span_str(span).to_string(),
            MIRLiteral::Bool(span) => self.get_span_str(span).to_string(),
            MIRLiteral::Str(span) => format!("\"{}\"", self.get_span_str(span)),
            MIRLiteral::Char(span) => format!("'{}'", self.get_span_str(span)),
        }
    }

    pub fn format_addr(&self, addr: &Addr) -> String {
        format!("t{}", addr.index)
    }
}
