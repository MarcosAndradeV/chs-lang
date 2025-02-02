use std::{fmt, path::PathBuf};

use chs_util::{chs_error, CHSResult};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizeOperator {
    Byte,
    Word,
    Dword,
    Fword,
    Pword,
    Qword,
    Tbyte,
    Tword,
    Dqword,
    Xword,
    Qqword,
    Yword,
    Dqqword,
    Zword,
}

#[allow(dead_code)]
impl SizeOperator {
    pub fn byte_size(&self) -> usize {
        match self {
            SizeOperator::Byte => 1,
            SizeOperator::Word => 2,
            SizeOperator::Dword => 4,
            SizeOperator::Pword | SizeOperator::Fword => 6,
            SizeOperator::Qword => 8,
            SizeOperator::Tbyte | SizeOperator::Tword => 10,
            SizeOperator::Dqword | SizeOperator::Xword => 16,
            SizeOperator::Qqword | SizeOperator::Yword => 32,
            SizeOperator::Dqqword | SizeOperator::Zword => 64,
        }
    }

    pub fn bit_size(&self) -> usize {
        self.byte_size() * 8
    }

    pub fn from_chstype(
        ttype: &chs_types::CHSType,
        type_map: &chs_types::TypeMap,
    ) -> CHSResult<Self> {
        match ttype {
            chs_types::CHSType::Pointer(_)
            | chs_types::CHSType::Function(_, _)
            | chs_types::CHSType::String
            | chs_types::CHSType::Int
            | chs_types::CHSType::UInt => Ok(Self::Qword),
            chs_types::CHSType::Alias(k) => {
                if let Some(ttype) = type_map.get_type(k) {
                    Self::from_chstype(ttype, type_map)
                } else {
                    chs_error!("Type not found")
                }
            }
            chs_types::CHSType::Distinct(chstype) => Self::from_chstype(chstype, type_map),
            chs_types::CHSType::Char | chs_types::CHSType::Boolean => Ok(Self::Byte),
            chs_types::CHSType::Void => chs_error!("TODO"),
        }
    }

    pub fn register_for_size(&self, reg: Register) -> Register {
        match (self, reg) {
            (SizeOperator::Qword, reg) if reg.is_64() => reg,
            (SizeOperator::Byte, reg) if reg.is_8() => reg,
            (SizeOperator::Qword, Register::Al) => Register::Rax,
            (SizeOperator::Byte, Register::Rax) => Register::Al,
            (SizeOperator::Byte, Register::Rbx) => Register::Bl,
            (SizeOperator::Byte, Register::Rdi) => Register::Dil,
            (SizeOperator::Byte, Register::R12) => Register::R12L,
            _ => todo!("{self}, {reg}"),
        }
    }
}

impl fmt::Display for SizeOperator {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SizeOperator::Byte    => write!(f, "BYTE"),
            SizeOperator::Word    => write!(f, "WORD"),
            SizeOperator::Dword   => write!(f, "DWORD"),
            SizeOperator::Fword   => write!(f, "FWORD"),
            SizeOperator::Pword   => write!(f, "PWORD"),
            SizeOperator::Qword   => write!(f, "QWORD"),
            SizeOperator::Tbyte   => write!(f, "TBYTE"),
            SizeOperator::Tword   => write!(f, "TWORD"),
            SizeOperator::Dqword  => write!(f, "DQWORD"),
            SizeOperator::Xword   => write!(f, "XWORD"),
            SizeOperator::Qqword  => write!(f, "QQWORD"),
            SizeOperator::Yword   => write!(f, "YWORD"),
            SizeOperator::Dqqword => write!(f, "DQQWORD"),
            SizeOperator::Zword   => write!(f, "ZWORD"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Al,
    Bl,
    Cl,
    Dl,
    Sil,
    Dil,
    R12L,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Register::*;
        match self {
            Rax => write!(f, "rax"),
            Rcx => write!(f, "rcx"),
            Rdx => write!(f, "rdx"),
            Rbx => write!(f, "rbx"),
            Rsp => write!(f, "rsp"),
            Rbp => write!(f, "rbp"),
            Rsi => write!(f, "rsi"),
            Rdi => write!(f, "rdi"),
            R8 => write!(f, "r8"),
            R9 => write!(f, "r9"),
            R10 => write!(f, "r10"),
            R11 => write!(f, "r11"),
            R12 => write!(f, "r12"),
            R13 => write!(f, "r13"),
            R14 => write!(f, "r14"),
            R15 => write!(f, "r15"),
            Al => write!(f, "al"),
            Bl => write!(f, "bl"),
            Cl => write!(f, "cl"),
            Dl => write!(f, "dl"),
            Sil => write!(f, "sil"),
            Dil => write!(f, "dil"),
            R12L => write!(f, "r12l"),
        }
    }
}

impl Register {
    pub const fn get_syscall_call_convention() -> [Self; 6] {
        use Register::*;
        [Rdi, Rsi, Rdx, R10, R8, R9]
    }
    /// Excluding `rsp` and `rbp`
    pub const fn get_callee_saved() -> [Self; 5] {
        use Register::*;
        [Rbx, R12, R13, R14, R15]
    }

    fn is_64(&self) -> bool {
        use Register::*;
        matches!(
            self,
            Rax | Rcx
                | Rdx
                | Rbx
                | Rsp
                | Rbp
                | Rsi
                | Rdi
                | R8
                | R9
                | R10
                | R11
                | R12
                | R13
                | R14
                | R15
        )
    }
    pub fn is_8(&self) -> bool {
        use Register::*;
        matches!(self,
            Al |
            Bl |
            Cl |
            Dl |
            Sil |
            Dil |
            R12L
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// Scale: A 2-bit constant factor that is either 1, 2, 4, or 8.
    ///
    /// Index: Any general purpose register.
    ///
    /// Base: Any general purpose register.
    ///
    /// Displacement: An integral offset. (normally limited to 32 bits even in 64-bit mode but can be 64-bits with a few select encodings)
    Memory(SizeOperator, String),
    Register(Register),
    Const(SizeOperator, i64),
    Label(String),
}

impl Value {
    pub fn is_register(&self) -> bool {
        matches!(self, Self::Register(..))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Memory(size, addrs) => write!(f, "{} [{}]", size, addrs),
            Self::Label(label) => write!(f, "{}", label),
            Self::Register(name) => write!(f, "{}", name),
            Self::Const(_, value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataDef {
    pub name: String,
    pub directive: DataDirective,
    pub items: Vec<DataExpr>,
}

impl DataDef {
    pub fn new(name: impl Into<String>, directive: DataDirective, items: Vec<DataExpr>) -> Self {
        Self {
            name: name.into(),
            directive,
            items,
        }
    }
}

impl fmt::Display for DataDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.name)?;
        write!(f, " {} ", self.directive)?;
        let mut line = self
            .items
            .iter()
            .map(|expr| format!("{}", expr))
            .collect::<Vec<String>>()
            .join(", ");
        let mut limit = 0;
        for i in 0..line.len() {
            if limit < 100 {
                limit += 1;
            } else {
                if line.as_bytes()[i] != b' ' {
                    limit += 1;
                    continue;
                }
                line.insert_str(i, "\\\n");
                limit = 0;
            }
        }
        write!(f, "{}", line)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum DataDirective {
    Db,
    Rb,
    Dw,
    Rw,
    Dd,
    Rd,
    Dq,
    Rq,
    // TODO: do be others
}

impl fmt::Display for DataDirective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Db => write!(f, "db"),
            Self::Rb => write!(f, "rb"),
            Self::Dw => write!(f, "dw"),
            Self::Rw => write!(f, "rw"),
            Self::Dd => write!(f, "dd"),
            Self::Rd => write!(f, "rd"),
            Self::Dq => write!(f, "dq"),
            Self::Rq => write!(f, "rq"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataExpr {
    Str(String),
    Const(u64),
}

impl fmt::Display for DataExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Str(string) => {
                write!(
                    f,
                    "{}",
                    string
                        .bytes()
                        .map(|expr| format!("{}", expr))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::Const(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Cond {
    E,  // EQUAL
    Z,  // ZERO
    NE, // NOT EQUAL
    NZ, // NOT ZERO
    L,  // less
    GE, // greater or equal
    LE, // less or equal
    G,  // greater
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::E => write!(f, "e"),
            Self::Z => write!(f, "z"),
            Self::NE => write!(f, "ne"),
            Self::NZ => write!(f, "nz"),
            Self::L => write!(f, "l"),
            Self::GE => write!(f, "ge"),
            Self::LE => write!(f, "le"),
            Self::G => write!(f, "g"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Nop,
    Syscall,
    Raw(String),

    Inc(Value),
    Dec(Value),
    Neg(Value),
    Not(Value),

    Mov(Value, Value),
    Lea(Value, Value),
    Push(Value),
    Pop(Value),

    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value),
    Div(Value),

    And(Value, Value),
    Or(Value, Value),
    Xor(Value, Value),

    Cmp(Value, Value),
    Cmove(Cond, Value, Value),

    Test(Value, Value),
    Set(Cond, Value),
    J(Cond, Value),
    Jmp(Value),
    Call(Value),
    Ret,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nop => write!(f, "nop"),
            Self::Syscall => write!(f, "syscall"),
            Self::Raw(raw) => write!(f, "{raw}"),
            Self::Inc(val) => write!(f, "inc {val}"),
            Self::Dec(val) => write!(f, "dec {val}"),
            Self::Neg(val) => write!(f, "neg {val}"),
            Self::Not(val) => write!(f, "not {val}"),
            Self::Mov(dst, src) => match (dst, src) {
                (dst, Value::Const(SizeOperator::Byte, src)) => write!(f, "mov {dst}, BYTE {src}"),
                _ => write!(f, "mov {dst}, {src}"),
            },
            Self::Lea(dst, src) => write!(f, "lea {dst}, {src}"),
            Self::Push(src) => write!(f, "push {src}"),
            Self::Pop(dst) => write!(f, "pop {dst}"),
            Self::Add(dst, src) => write!(f, "add {dst}, {src}"),
            Self::Sub(dst, src) => write!(f, "sub {dst}, {src}"),
            Self::Mul(src) => write!(f, "mul {src}"),
            Self::Div(src) => write!(f, "div {src}"),
            Self::And(dst, src) => write!(f, "and {dst}, {src}"),
            Self::Or(dst, src) => write!(f, "or {dst}, {src}"),
            Self::Xor(dst, src) => write!(f, "xor {dst}, {src}"),
            Self::Cmp(dst, src) => write!(f, "cmp {dst}, {src}"),
            Self::Cmove(cond, dst, src) => write!(f, "cmov{cond} {dst}, {src}"),
            Self::Test(dst, src) => write!(f, "test {dst}, {src}"),
            Self::Set(cond, dst) => write!(f, "set{cond} {dst}"),
            Instr::J(cond, label) => write!(f, "j{cond} .{label}"), // local labels
            Instr::Jmp(label) => write!(f, "jmp .{label}"),         // local labels
            Instr::Call(label) => {
                if let Value::Label(..) = label {
                    write!(f, "call _{label}")
                } else {
                    write!(f, "call {label}")
                }
            }
            Instr::Ret => write!(f, "ret"),
        }
    }
}

#[derive(Debug, Default, Clone)]
/// Blocks are labels + instructions
pub struct Block {
    pub label: String,
    pub instrs: Vec<Instr>,
}

impl Block {
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            instrs: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.instrs.push(instr);
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.label.is_empty() {
            writeln!(f, ".{}:", self.label)?;
        }

        for instr in self.instrs.iter() {
            writeln!(f, "\t{}", instr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
pub struct Function {
    name: String,
    stack_allocated: usize,
    blocks: Vec<Block>,
    epiloge: bool,
    prologe: bool,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Function {
            name: name.into(),
            stack_allocated: 0,
            blocks: Vec::new(),
            prologe: true,
            epiloge: true,
        }
    }
    pub fn allocate_stack(&mut self, size: usize) -> usize {
        // self.push_raw_instr("sub rsp, 8");
        self.stack_allocated += size;
        self.stack_allocated
    }

    pub fn push_block(&mut self, label: impl Into<String>) -> &mut Block {
        self.blocks.push(Block::new(label));
        self.blocks.last_mut().unwrap()
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.blocks
            .last_mut()
            .expect("Last block must be present")
            .push_instr(instr);
    }

    pub fn last_instr(&mut self) -> &Instr {
        self.blocks
            .last()
            .expect("Last block must be present")
            .instrs
            .last()
            .expect("Expect at least one expression")
    }

    pub fn push_raw_instr(&mut self, instr: impl Into<String>) {
        self.blocks
            .last_mut()
            .expect("Last block must be present")
            .push_instr(Instr::Raw(instr.into()));
    }

    pub fn set_prelude(&mut self, arg: bool) {
        self.prologe = arg;
    }

    pub fn set_epiloge(&mut self, arg: bool) {
        self.epiloge = arg;
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.name.is_empty() {
            return Ok(());
        }
        writeln!(f, ";; function",)?;
        writeln!(f, "_{}:", self.name)?;

        if self.prologe {
            writeln!(f, "\tpush rbp")?;
            writeln!(f, "\tmov rbp, rsp")?;
            if self.stack_allocated > 0 {
                writeln!(f, "\tsub rsp, {}", self.stack_allocated)?;
            }
        }

        for blk in self.blocks.iter() {
            write!(f, "{}", blk)?;
        }

        if self.epiloge {
            if self.stack_allocated > 0 {
                writeln!(f, "\tadd rsp, {}", self.stack_allocated)?;
            }

            writeln!(f, "\tpop rbp")?;
            writeln!(f, "\tret")?;
        }

        writeln!(f, ";; end")
    }
}

/// Represents a single fasm file
#[derive(Debug, Default, Clone)]
pub struct Module {
    out_path: PathBuf,
    start: Block,
    functions: Vec<Function>,
    data: Vec<DataDef>,
}

impl Module {
    pub fn new(out_path: PathBuf) -> Module {
        let start = Block::new("");
        Module {
            out_path,
            start,
            ..Default::default()
        }
    }

    pub fn push_function(&mut self, func: Function) -> &mut Function {
        self.functions.push(func);
        self.functions.last_mut().unwrap()
    }

    pub fn push_data(&mut self, data: DataDef) -> &mut DataDef {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }

    pub fn push_raw_instr_to_start(&mut self, instr: impl Into<String>) {
        self.start.push_instr(Instr::Raw(instr.into()));
    }

    pub fn out_path(&self) -> &PathBuf {
        &self.out_path
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "format ELF64 executable")?;
        writeln!(f, "entry _start")?;

        writeln!(f, "segment executable")?;
        writeln!(f, "_start:")?;
        for instr in self.start.instrs.iter() {
            writeln!(f, "\t{}", instr)?;
        }
        writeln!(f, "\tcall _main")?;
        writeln!(f, "\tmov rax, 60")?;
        writeln!(f, "\tmov rdi, 0")?;
        writeln!(f, "\tsyscall")?;

        for func in self.functions.iter() {
            writeln!(f, "{}", func)?;
        }

        if !self.data.is_empty() {
            writeln!(f, "segment readable writable")?;
        }
        for data in self.data.iter() {
            writeln!(f, "{}", data)?;
        }
        Ok(())
    }
}
