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
    pub fn print_function(&self, func: &MIRFunction) -> String {
        let mut output = String::new();

        writeln!(
            output,
            "fn {} : {} {{",
            self.get_span_str(&func.name),
            func.fn_type
        )
        .unwrap();

        for (block_id, block) in func.body.iter().enumerate() {
            write!(output, "Block({block_id})").unwrap();
            for (i, op) in block.operations.iter().enumerate() {
                writeln!(output, "{i}:    {}", self.format_operation(op)).unwrap();
            }

            if let Some(terminator) = &block.terminator {
                writeln!(output, "    {}", self.format_terminator(terminator)).unwrap();
            } else {
                writeln!(output, "    Incomplete").unwrap();
            }
        }

        writeln!(output, "}}").unwrap();

        output
    }

    pub fn format_operation(&self, op: &MIROperation) -> String {
        match op {
            MIROperation::Assignment { target, value } => {
                format!(
                    "{} = {}",
                    self.format_addr(target),
                    self.format_operand(value)
                )
            }
            MIROperation::Binop {
                target,
                op,
                op1,
                op2,
            } => {
                format!(
                    "{} = {} {} {}",
                    self.format_addr(target),
                    self.format_addr(op1),
                    op,
                    self.format_addr(op2)
                )
            }
            MIROperation::Unop { target, op, op1 } => {
                format!(
                    "{} = {}{}",
                    self.format_addr(target),
                    op,
                    self.format_addr(op1)
                )
            }
            MIROperation::Refer { target, op1 } => {
                format!("{} = &{}", self.format_addr(target), self.format_addr(op1))
            }
            MIROperation::Defer { target, op1 } => {
                format!("{} = *{}", self.format_addr(target), self.format_addr(op1))
            }
            MIROperation::FuncCall { target, name, args } => {
                let args_str = args
                    .iter()
                    .map(|addr| self.format_addr(addr))
                    .collect::<Vec<_>>()
                    .join(", ");
                match target {
                    Some(tgt) => format!(
                        "{} = {}({})",
                        self.format_addr(tgt),
                        self.get_span_str(name),
                        args_str
                    ),
                    None => format!("{:?}({})", name, args_str),
                }
            }
        }
    }

    pub fn format_terminator(&self, term: &Terminator) -> String {
        match term {
            Terminator::Return(Some(op)) => format!("return {};", self.format_operand(op)),
            Terminator::Return(None) => "return;".to_string(),
            Terminator::Branch {
                cond,
                true_block,
                false_block,
            } => {
                format!(
                    "if ({}) goto block{} else goto block{};",
                    self.format_operand(cond),
                    true_block.0,
                    false_block.0
                )
            }
            Terminator::Goto(block) => format!("goto block{};", block.0),
        }
    }

    pub fn format_operand(&self, op: &Operand) -> String {
        match op {
            Operand::Literal(lit, _) => self.format_literal(lit),
            Operand::Local(addr) => self.format_addr(addr),
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
        format!("_{}", addr.index)
    }
}
