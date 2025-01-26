#![allow(dead_code)]
pub mod fasm;
use std::collections::HashMap;

use chs_ast::nodes::{self, Binop, Expression, TypedModule};
use chs_types::TypeMap;
use chs_util::{CHSError, CHSResult};
use fasm::{Instr, Register, SizeOperator, Value};

pub struct FasmGenerator {
    tmp_reg: usize,
    scopes: Vec<HashMap<String, fasm::Value>>,
    labels: Vec<String>,
    datadefs: Vec<fasm::DataDef>,
    type_map: TypeMap,
}

impl FasmGenerator {
    pub fn generate(tm: TypedModule) -> CHSResult<fasm::Module> {
        let TypedModule {
            function_decls,
            type_defs,
        } = tm;
        let mut gen = Self {
            tmp_reg: 0,
            scopes: vec![],
            labels: vec![],
            datadefs: vec![],
            type_map: type_defs,
        };
        let mut out = fasm::Module::new();

        for func_decl in function_decls {
            let func = gen.generate_function(func_decl)?;
            out.push_function(func);
        }

        Ok(out)
    }

    fn generate_function(&mut self, func_decl: nodes::FunctionDecl) -> CHSResult<fasm::Function> {
        let mut func = fasm::Function::new(func_decl.name);
        let mut scope: HashMap<String, fasm::Value> = HashMap::new();
        let cc = Register::get_syscall_call_convention();
        for (i, (name, _)) in func_decl.args.into_iter().enumerate() {
            assert!(i < cc.len());
            scope.insert(name, fasm::Value::Register(cc[i].clone()));
        }
        self.scopes.push(scope);
        func.push_block("start");

        for expr in &func_decl.body {
            self.generate_expression(&mut func, expr)?;
        }

        match func_decl.ret_type {
            chs_types::CHSType::Void => {}
            chs_types::CHSType::Int | chs_types::CHSType::UInt => {
                let cs = Register::get_callee_saved();
                let reg = cs[self.tmp_reg - 1];
                func.push_raw_instr(format!("mov rax, {reg}"));
            }
            _ => todo!("return other types"),
        }

        self.scopes.pop();
        Ok(func)
    }

    fn generate_expression(
        &mut self,
        func: &mut fasm::Function,
        expr: &Expression,
    ) -> CHSResult<Option<Value>> {
        use nodes::ConstExpression::*;
        let cs = Register::get_callee_saved();
        let cc = Register::get_syscall_call_convention();
        match expr {
            Expression::ConstExpression(IntegerLiteral(v)) => {
                Ok(Some(Value::Const(*v)))
            }
            Expression::ConstExpression(BooleanLiteral(v)) => {
                assert!(self.tmp_reg < cs.len());
                let register = Value::Register(cs[self.tmp_reg]);
                func.push_instr(Instr::Mov(register.clone(), Value::Const(*v as i64)));
                self.tmp_reg += 1;
                Ok(Some(register))
            }
            Expression::ConstExpression(Symbol(v)) => {
                assert!(self.tmp_reg < cs.len());
                let register = Value::Register(cs[self.tmp_reg]);
                let value = match self.type_map.get(v) {
                    Some(chs_types::CHSType::Function(..)) => Value::Label(v.clone()),
                    _ => self.get_var(v)?.clone(),
                };
                func.push_instr(Instr::Mov(register.clone(), value));
                self.tmp_reg += 1;
                Ok(Some(register))
            }
            Expression::VarDecl(v) => {
                let stack_pos = func.allocate_stack(8);
                let src = self.generate_expression(func, &v.value)?.unwrap();
                let dst = Value::Memory((SizeOperator::Qword, format!("rbp-{}", stack_pos)));
                if src.is_register() { self.tmp_reg -= 1; }
                func.push_instr(Instr::Mov(dst.clone(), src));
                let s = self.scopes.last_mut().expect("Expected scope");
                s.insert(v.name.clone(), dst);
                Ok(None)
            }
            Expression::Call(c) => {
                // TODO: Make the expression contain the return type. For now it will always return on rax
                let f = self.generate_expression(func, &c.caller)?.unwrap();
                for (i, arg) in c.args.iter().enumerate() {
                    let src = self.generate_expression(func, arg)?.unwrap();
                    if i >= cc.len() {
                        let dst = Value::Register(cc[i]);
                        func.push_instr(Instr::Mov(dst, src));
                    } else {
                        func.push_instr(Instr::Push(src));
                    }
                    self.tmp_reg -= 1;
                }

                func.push_instr(Instr::Call(f));

                Ok(Some(Value::Register(Register::Rax)))
            }
            Expression::Binop(binop) => self.generate_binop(func, binop),
            _ => todo!("generation for expression {:?}", expr),
        }
    }

    fn get_var(&self, name: &str) -> CHSResult<&Value> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s: &HashMap<_, _>| s.get(name))
            .next()
            .ok_or_else(|| CHSError(format!("Undefined variable '{}'", name)))
    }

    fn generate_binop(&mut self, func: &mut fasm::Function, binop: &Binop) -> CHSResult<Option<Value>> {
        _ = func;
        _ = binop;
        todo!()
    }
}
