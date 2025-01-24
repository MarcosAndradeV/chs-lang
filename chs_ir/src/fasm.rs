use std::fmt;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Size {
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
impl Size {
    pub fn byte_size(&self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Dword => 4,
            Size::Pword | Size::Fword => 6,
            Size::Qword => 8,
            Size::Tbyte | Size::Tword => 10,
            Size::Dqword | Size::Xword => 16,
            Size::Qqword | Size::Yword => 32,
            Size::Dqqword | Size::Zword => 64,
        }
    }

    pub fn bit_size(&self) -> usize {
        self.byte_size() * 8
    }
}

impl fmt::Display for Size {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Size::Byte    => write!(f, "BYTE"),
            Size::Word    => write!(f, "WORD"),
            Size::Dword   => write!(f, "DWORD"),
            Size::Fword   => write!(f, "FWORD"),
            Size::Pword   => write!(f, "PWORD"),
            Size::Qword   => write!(f, "QWORD"),
            Size::Tbyte   => write!(f, "TBYTE"),
            Size::Tword   => write!(f, "TWORD"),
            Size::Dqword  => write!(f, "DQWORD"),
            Size::Xword   => write!(f, "XWORD"),
            Size::Qqword  => write!(f, "QQWORD"),
            Size::Yword   => write!(f, "YWORD"),
            Size::Dqqword => write!(f, "DQQWORD"),
            Size::Zword   => write!(f, "ZWORD"),
        }
    }
}

#[derive(Debug, Clone)]
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
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Rax => write!(f, "rax"),
            Self::Rcx => write!(f, "rcx"),
            Self::Rdx => write!(f, "rdx"),
            Self::Rbx => write!(f, "rbx"),
            Self::Rsp => write!(f, "rsp"),
            Self::Rbp => write!(f, "rbp"),
            Self::Rsi => write!(f, "rsi"),
            Self::Rdi => write!(f, "rdi"),
            Self::R8 => write!(f, "r8"),
            Self::R9 => write!(f, "r9"),
            Self::R10 => write!(f, "r10"),
            Self::R11 => write!(f, "r11"),
            Self::R12 => write!(f, "r12"),
            Self::R13 => write!(f, "r13"),
            Self::R14 => write!(f, "r14"),
            Self::R15 => write!(f, "r15"),
        }
    }
}

impl Register {
    pub const fn get_syscall_call_convention() -> [Self; 6] {
        use Register::*;
        [Rdi, Rsi, Rdx, R10, R8, R9]
    }
    /// Exculding `rsp` and `rbp`
    pub const fn get_callee_saved() -> [Self; 5] {
        use Register::*;
        [R12, R13, R14, R15, Rbx]
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    /// Scale: A 2-bit constant factor that is either 1, 2, 4, or 8.
    ///
    /// Index: Any general purpose register.
    ///
    /// Base: Any general purpose register.
    ///
    /// Displacement: An integral offset. (normally limited to 32 bits even in 64-bit mode but can be 64-bits with a few select encodings)
    Memory(String),
    Register(Register),
    Const(i64),
    Label(String)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Memory(addrs) => write!(f, "[{}]", addrs),
            Self::Label(label) => write!(f, "{}", label),
            Self::Register(name) => write!(f, "{}", name),
            Self::Const(value) => write!(f, "{}", value),
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
        write!(
            f,
            "{}: {} {}",
            self.name,
            self.directive,
            self.items
                .iter()
                .map(|expr| format!("{}", expr))
                .collect::<Vec<String>>()
                .join(", ")
        )
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
    // TODO: do te others
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
            Self::Str(string) => write!(f, "\"{}\"", string),
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

    Mov(Value, Value),

    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),

    And(Value, Value),
    Or(Value, Value),
    Xor(Value, Value),

    Cmp(Value, Value),
    Test(Value, Value),
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
            Self::Mov(dst, src) => write!(f, "mov {dst}, {src}"),
            Self::Add(dst, src) => write!(f, "add {dst}, {src}"),
            Self::Sub(dst, src) => write!(f, "sub {dst}, {src}"),
            Self::Mul(dst, src) => write!(f, "mul {dst}, {src}"),
            Self::Div(dst, src) => write!(f, "div {dst}, {src}"),
            Self::And(dst, src) => write!(f, "and {dst}, {src}"),
            Self::Or(dst, src) => write!(f, "or {dst}, {src}"),
            Self::Xor(dst, src) => write!(f, "xor {dst}, {src}"),
            Self::Cmp(dst, src) => write!(f, "cmp {dst}, {src}"),
            Self::Test(dst, src) => write!(f, "test {dst}, {src}"),
            Instr::J(cond, label) => write!(f, "j{cond} {label}"),
            Instr::Jmp(label) => write!(f, "jmp {label}"),
            Instr::Call(label) => write!(f, "call {label}"),
            Instr::Ret => write!(f, "ret"),
        }
    }
}

#[derive(Debug, Clone)]
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
        writeln!(f, ".{}:", self.label)?;

        write!(
            f,
            "{}",
            self.instrs
                .iter()
                .map(|instr| format!("\t{}", instr))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Function {
            name: name.into(),
            blocks: Vec::new(),
        }
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

    pub fn push_raw_instr(&mut self, instr: impl Into<String>) {
        self.blocks
            .last_mut()
            .expect("Last block must be present")
            .push_instr(Instr::Raw(instr.into()));
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, ";; function",)?;
        writeln!(f, "{}:", self.name)?;

        for blk in self.blocks.iter() {
            writeln!(f, "{}", blk)?;
        }

        write!(f, ";; end")
    }
}

/// Represents a single fasm file
///
/// `TODO: Add entry and file format`
#[derive(Debug, Default, Clone)]
pub struct Module {
    functions: Vec<Function>,
    data: Vec<DataDef>,
}

impl Module {
    pub fn new() -> Module {
        Module::default()
    }

    pub fn push_function(&mut self, func: Function) -> &mut Function {
        self.functions.push(func);
        self.functions.last_mut().unwrap()
    }

    pub fn push_data(&mut self, data: DataDef) -> &mut DataDef {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "format ELF64 executable")?;
        writeln!(f, "entry main")?;

        if self.functions.len() > 0 {
            writeln!(f, "segment executable")?;
        }
        for func in self.functions.iter() {
            writeln!(f, "{}", func)?;
        }

        if self.data.len() > 0 {
            writeln!(f, "segment readble writable")?;
        }
        for data in self.data.iter() {
            writeln!(f, "{}", data)?;
        }
        Ok(())
    }
}
