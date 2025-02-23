#![allow(unused)]
use crate::fasm::{Function, Module};
pub mod fasm;
use crate::fasm::Instr::{Cmov, Push};
use chs_ast::nodes::ConstExpression::{
    BooleanLiteral, CharLiteral, IntegerLiteral, StringLiteral, Symbol,
};
use chs_ast::nodes::{
    self, Assign, Binop, Call, ConstExpression, Expression, ExpressionList, FunctionDecl, IfElseExpression, IfExpression, Index, Operator, Syscall, TypedModule, Unop, WhileExpression
};
use chs_types::{CHSType, TypeMap};
use chs_util::{chs_error, CHSError, CHSResult};
use fasm::{Cond, DataDef, DataDirective, DataExpr, Instr, Register, SizeOperator, Value};
use nodes::VarDecl;
use std::collections::HashMap;

#[derive(Default)]
pub struct FasmGenerator {
    label_count: usize,
    str_count: usize,
    scopes: Vec<HashMap<String, Value>>,
    data_defs: Vec<DataDef>,
    type_map: TypeMap,
    temp_regs: usize,
}

impl FasmGenerator {
    pub fn new(type_map: TypeMap) -> Self {
        Self {
            type_map,
            ..Self::default()
        }
    }
    pub fn generate(m: TypedModule) -> CHSResult<Module> {
        let mut gen = FasmGenerator::new(m.type_map);
        let mut fasm_module = Module::new(m.file_path.with_extension("asm"));

        let func = fasm_module.push_function(fasm::Function::new("print_int"));
        func.set_prelude(false);
        func.set_epiloge(false);
        func.push_block("");
        func.push_raw_instr("mov r9, -3689348814741910323");
        func.push_raw_instr("sub rsp, 40");
        func.push_raw_instr("mov BYTE [rsp+31], 10");
        func.push_raw_instr("lea rcx, [rsp+30]");
        func.push_block("L2");
        func.push_raw_instr("mov rax, rdi");
        func.push_raw_instr("lea r8, [rsp+32]");
        func.push_raw_instr("mul r9");
        func.push_raw_instr("mov rax, rdi");
        func.push_raw_instr("sub r8, rcx");
        func.push_raw_instr("shr rdx, 3");
        func.push_raw_instr("lea rsi, [rdx+rdx*4]");
        func.push_raw_instr("add rsi, rsi");
        func.push_raw_instr("sub rax, rsi");
        func.push_raw_instr("add eax, 48");
        func.push_raw_instr("mov BYTE [rcx], al");
        func.push_raw_instr("mov rax, rdi");
        func.push_raw_instr("mov rdi, rdx");
        func.push_raw_instr("mov rdx, rcx");
        func.push_raw_instr("sub rcx, 1");
        func.push_raw_instr("cmp rax, 9");
        func.push_raw_instr("ja  .L2");
        func.push_raw_instr("lea rax, [rsp+32]");
        func.push_raw_instr("mov edi, 1");
        func.push_raw_instr("sub rdx, rax");
        func.push_raw_instr("xor eax, eax");
        func.push_raw_instr("lea rsi, [rsp+32+rdx]");
        func.push_raw_instr("mov rdx, r8");
        func.push_raw_instr("mov rax, 1");
        func.push_raw_instr("syscall");
        func.push_raw_instr("add rsp, 40");
        func.push_raw_instr("ret");

        for decl in m.function_decls {
            let func = gen.generate_function(decl)?;
            fasm_module.push_function(func);
        }

        for data in gen.data_defs {
            fasm_module.push_data(data);
        }

        Ok(fasm_module)
    }

    fn generate_function(&mut self, func_decl: FunctionDecl) -> CHSResult<Function> {
        let mut func = Function::new(func_decl.name);
        func.push_block("");
        // func.set_prelude(false);
        // func.push_raw_instr("push rbp");
        // func.push_raw_instr("mov rbp, rsp");
        self.label_count = 0;
        let mut scope: HashMap<String, Value> = HashMap::new();
        let cc = Register::get_syscall_call_convention();
        for (i, (name, v)) in func_decl.args.into_iter().enumerate() {
            assert!(i < cc.len());
            let size = SizeOperator::Qword; // from_chstype(&v, &self.type_map)?;
            let stack_pos = func.allocate_stack(size.byte_size());
            let src = Value::Register(cc[i]);
            let dst = Value::Memory(size, format!("rbp-{}", stack_pos));
            func.push_instr(Instr::Mov(dst.clone(), src));
            scope.insert(name, dst);
        }
        self.scopes.push(scope);
        for expr in func_decl.body.iter() {
            self.generate_expression(&mut func, expr)?;
        }

        if !func_decl.ret_type.is_void() {
            func.push_raw_instr("pop rax");
        }
        self.scopes.pop();
        Ok(func)
    }

    fn generate_expression(&mut self, func: &mut Function, expr: &Expression) -> CHSResult<()> {
        match expr {
            Expression::ConstExpression(IntegerLiteral(v)) => {
                func.push_raw_instr(format!("mov rax, {v}"));
                func.push_raw_instr(format!("push rax"));
            }
            Expression::ConstExpression(CharLiteral(v)) => {
                func.push_instr(Push(Value::Const(SizeOperator::Qword, *v as i64)));
            }
            Expression::ConstExpression(BooleanLiteral(v)) => {
                func.push_instr(Push(Value::Const(SizeOperator::Qword, *v as i64)));
            }
            Expression::ConstExpression(Symbol(v)) => {
                func.push_instr(Push(self.get_var(v)?.clone()));
            }
            Expression::ConstExpression(StringLiteral(v)) => {
                self.str_count += 1;
                let name = format!("str{}", self.str_count);
                self.data_defs.push(DataDef {
                    name: name.clone(),
                    directive: DataDirective::Db,
                    items: vec![DataExpr::Str(v.clone()), DataExpr::Const(0)],
                });
                func.push_instr(Push(Value::Label(name)));
            }
            Expression::VarDecl(e) => self.generate_var_decl(func, e)?,
            Expression::Assign(e) => self.generate_assign(func, e)?,
            Expression::Call(e) => self.generate_call(func, e)?,
            Expression::Syscall(e) => self.generate_syscall(func, e)?,
            Expression::Binop(e) => self.generate_binop(func, e)?,
            Expression::Unop(e) => self.generate_unop(func, e)?,
            Expression::Index(e) => self.generate_index(func, e)?,
            Expression::IfExpression(e) => self.generate_if(func, e)?,
            Expression::IfElseExpression(e) => self.generate_if_else(func, e)?,
            Expression::WhileExpression(e) => self.generate_while(func, e)?,
            Expression::Cast(e) => {
                return self.generate_expression(func, &e.casted);
            },
            Expression::Group(e) => {
                return self.generate_expression(func, &e);
            },
            e => todo!("generate_expression {:?}", e),
        }
        Ok(())
    }

    fn generate_assign(&mut self, func: &mut Function, e: &Assign) -> Result<(), CHSError> {
        self.generate_expression(func, &e.value)?;
        match &e.assigned {
            Expression::ConstExpression(Symbol(v)) => {
                let lhs = self.get_var(v)?;
                func.push_raw_instr("pop rax");
                func.push_raw_instr(format!("lea rbx, {lhs}"));
                func.push_raw_instr("mov [rbx], rax");
            }
            Expression::Unop(unop) if unop.op == Operator::Deref => {
                self.generate_expression(func, &unop.left)?;
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("lea rbx, [rbx]");
                func.push_raw_instr("mov [rbx], rax");
            }
            Expression::Index(i) => {
                let size = SizeOperator::from_chstype(e.ttype.as_ref().unwrap(), &self.type_map)?.byte_size();
                func.push_raw_instr("pop rcx");
                self.generate_expression(func, &i.index)?;
                func.push_raw_instr("pop rbx");
                self.generate_expression(func, &i.left)?;
                func.push_raw_instr("pop rax");
                func.push_raw_instr("lea rax, [rax]");
                func.push_raw_instr(format!("mov [rax+rbx*{size}], rcx"));
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn generate_binop(&mut self, func: &mut Function, binop: &Binop) -> CHSResult<()> {
        let Binop {
            loc,
            op,
            left,
            right,
            ttype: _,
        } = binop;
        self.generate_expression(func, left)?;
        self.generate_expression(func, right)?;
        match op {
            Operator::Plus => {
                func.push_raw_instr("pop rax");
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("add rax, rbx");
                func.push_raw_instr("push rax");
            }
            Operator::Minus => {
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("sub rax, rbx");
                func.push_raw_instr("push rax");
            }
            Operator::Div => {
                func.push_raw_instr("xor rdx, rdx");
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("div rbx");
                func.push_raw_instr("push rax");
            }
            Operator::Mult => {
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("mul rbx");
                func.push_raw_instr("push rax");
            }
            Operator::Mod => {
                func.push_raw_instr("xor rdx, rdx");
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("div rbx");
                func.push_raw_instr("push rdx");
            }
            Operator::LOr | Operator::Or => {
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("or rax, rbx");
                func.push_raw_instr("push rax");
            }
            Operator::LAnd | Operator::And => {
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("and rax, rbx");
                func.push_raw_instr("push rax");
            }
            Operator::Eq | Operator::NEq | Operator::Gt | Operator::Lt => {
                let cond = match op {
                    Operator::Eq => Cond::E,
                    Operator::NEq => Cond::NE,
                    Operator::Gt => Cond::G,
                    Operator::Lt => Cond::L,
                    _ => unreachable!(),
                };
                func.push_raw_instr("mov rcx, 0");
                func.push_raw_instr("mov rdx, 1");
                func.push_raw_instr("pop rbx");
                func.push_raw_instr("pop rax");
                func.push_raw_instr("cmp rax, rbx");
                func.push_instr(Cmov(
                    cond,
                    Value::Register(Register::Rcx),
                    Value::Register(Register::Rdx),
                ));
                func.push_raw_instr("push rcx");
            }
            _ => unreachable!("Not a binary operator"),
        }
        Ok(())
    }

    fn generate_call(&mut self, func: &mut Function, c: &Call) -> Result<(), CHSError> {
        let cc = Register::get_syscall_call_convention();
        let f = match &c.caller {
            Expression::ConstExpression(ConstExpression::Symbol(sym)) => sym.clone(),
            _ => todo!("Do other types of call"),
        };
        for (i, arg) in c.args.iter().enumerate().rev() {
            self.generate_expression(func, arg)?;
            if i <= cc.len() {
                let reg = cc[i];
                func.push_instr(Instr::Pop(Value::Register(reg)));
            }
        }
        func.push_instr(Instr::Call(f));
        // TODO: Make the expression contain the return type. For now it will always return on rax
        if let Some(CHSType::Function(_, ret)) = c.ttype.as_ref() {
            if !ret.is_void() {
                func.push_raw_instr("push rax");
            }
        }
        Ok(())
    }

    fn generate_var_decl(&mut self, func: &mut Function, e: &VarDecl) -> CHSResult<()> {
        if e.name.as_str() == "_" {
            self.generate_expression(func, &e.value)?;
            func.push_raw_instr("pop rax");
            return Ok(());
        }
        // NOTE: Everything is a qword size for now. Its easy to work with.
        let size = SizeOperator::Qword;// from_chstype(e.ttype.as_ref().unwrap(), &self.type_map)?;
        let stack_pos = func.allocate_stack(size.byte_size());
        self.generate_expression(func, &e.value)?;
        func.push_raw_instr("pop rax");
        func.push_raw_instr(format!("mov [rbp-{stack_pos}], rax"));
        self.scopes.last_mut().unwrap().insert(
            e.name.clone(),
            Value::Memory(size, format!("rbp-{stack_pos}")),
        );
        Ok(())
    }

    fn get_var(&self, name: &str) -> CHSResult<&Value> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s: &HashMap<_, _>| s.get(name))
            .next()
            .ok_or_else(|| CHSError(format!("Undefined variable '{}'", name)))
    }

    fn generate_syscall(&mut self, func: &mut Function, c: &Syscall) -> Result<(), CHSError> {
        let cc = Register::get_syscall_call_convention_with_rax();
        for (i, arg) in c.args.iter().enumerate().rev() {
            self.generate_expression(func, arg)?;
            if i <= cc.len() {
                let reg = cc[i];
                func.push_instr(Instr::Pop(Value::Register(reg)));
            }
        }
        func.push_instr(Instr::Syscall);
        func.push_raw_instr("push rax");
        Ok(())
    }

    fn generate_if(&mut self, func: &mut Function, e: &IfExpression) -> Result<(), CHSError> {
        let IfExpression { loc: _, cond, body } = e;
        self.label_count += 1;
        let label_after = format!("L{}", self.label_count);
        self.generate_expression(func, cond)?;
        func.push_raw_instr("pop rax");
        func.push_raw_instr("test rax, rax");
        func.push_raw_instr(format!("jz .{label_after}"));

        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_block(label_after.as_str());
        Ok(())
    }

    fn generate_if_else(&mut self, func: &mut Function, e: &IfElseExpression) -> Result<(), CHSError> {
        let IfElseExpression { loc: _, cond, body, else_body } = e;
        self.label_count += 1;
        let label_else = format!("L{}", self.label_count);
        self.label_count += 1;
        let label_after = format!("L{}", self.label_count);
        self.generate_expression(func, cond)?;
        func.push_raw_instr("pop rax");
        func.push_raw_instr("test rax, rax");
        func.push_raw_instr(format!("jz .{label_else}"));

        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_raw_instr(format!("jmp .{label_after}"));

        func.push_block(label_else.as_str());
        self.scopes.push(HashMap::new());
        for expr in else_body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_block(label_after.as_str());
        Ok(())
    }

    fn generate_while(&mut self, func: &mut Function, e: &WhileExpression) -> Result<(), CHSError> {
        let WhileExpression { loc, cond, body } = e;

        self.label_count += 1;
        let label_cond = format!("L{}", self.label_count);
        self.label_count += 1;
        let label_body = format!("L{}", self.label_count);

        func.push_instr(Instr::Jmp(label_cond.clone()));

        func.push_block(label_body.as_str());
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_block(label_cond.as_str());
        self.generate_expression(func, cond)?;
        func.push_raw_instr("pop rax");
        func.push_raw_instr("test rax, rax");
        func.push_instr(Instr::J(Cond::NZ, label_body));

        Ok(())
    }

    fn generate_unop(&mut self, func: &mut Function, e: &Unop) -> Result<(), CHSError> {
        match e.op {
            Operator::Negate => match e.left {
                Expression::ConstExpression(ConstExpression::IntegerLiteral(ref int)) => {
                    func.push_raw_instr(format!("push -{int}"));
                }
                _ => {
                    func.push_raw_instr("pop rax");
                    func.push_raw_instr("not rax");
                    func.push_raw_instr("push rax");
                }
            }
            Operator::LNot => todo!(),
            Operator::Refer => match e.left {
                Expression::ConstExpression(ConstExpression::Symbol(ref sym)) => {
                    let lhs = self.get_var(sym)?;
                    func.push_raw_instr(format!("lea rbx, {lhs}"));
                    func.push_raw_instr("push rbx");
                }
                _ => todo!()
            }
            Operator::Deref => {
                let size = SizeOperator::from_chstype(e.ttype.as_ref().unwrap(), &self.type_map)?;
                self.generate_expression(func, &e.left)?;
                let reg = size.register_for_size(Register::Rbx);
                func.push_raw_instr("pop rax");
                func.push_raw_instr("xor rbx, rbx");
                func.push_raw_instr(format!("mov {reg}, [rax]"));
                func.push_raw_instr("push rbx");
            },
            _ => unreachable!("Not a unary operator.")
        }
        Ok(())
    }

    fn generate_index(&mut self, func: &mut Function, e: &Index) -> Result<(), CHSError> {

        let size = SizeOperator::from_chstype(e.ttype.as_ref().unwrap(), &self.type_map)?;
        self.generate_expression(func, &e.index)?;
        func.push_raw_instr("pop rbx");
        self.generate_expression(func, &e.left)?;
        func.push_raw_instr("pop rax");
        func.push_raw_instr("lea rax, [rax]");
        func.push_raw_instr(format!("push {size} [rax+rbx*{}]", size.byte_size()));


        Ok(())
    }
}
