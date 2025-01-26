#![allow(dead_code)]
pub mod fasm;
use std::collections::HashMap;

use chs_ast::nodes::{self, Binop, Expression, TypedModule};
use chs_types::TypeMap;
use chs_util::{chs_error, CHSError, CHSResult};
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

        // TODO: Handle the return

        self.scopes.pop();
        Ok(func)
    }

    fn generate_expression<'a>(
        &mut self,
        func: &'a mut fasm::Function,
        expr: &Expression,
    ) -> CHSResult<Option<Value>> {
        use nodes::ConstExpression::*;
        let cs = Register::get_callee_saved();
        match expr {
            Expression::ConstExpression(IntegerLiteral(v)) => {
                assert!(self.tmp_reg < cs.len());
                let register = Value::Register(cs[self.tmp_reg]);
                func.push_instr(Instr::Mov(register.clone(), Value::Const(*v)));
                self.tmp_reg += 1;
                Ok(Some(register))
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
                func.push_instr(Instr::Mov(register.clone(), self.get_var(v)?.clone()));
                self.tmp_reg += 1;
                Ok(Some(register))
            }
            Expression::VarDecl(v) => {
                let stack_pos = func.allocate_stack(8);
                let src = self.generate_expression(func, &v.value)?.unwrap();
                let dst = Value::Memory((SizeOperator::Qword, format!("rbp-{}", stack_pos)));
                func.push_instr(Instr::Mov(dst.clone(), src));
                self.tmp_reg -= 1;
                if let Some(s) = self.scopes.last_mut() {
                    s.insert(v.name.clone(), dst);
                } else {
                    chs_error!("");
                }

                Ok(None)
            }
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

    fn generate_binop(&mut self, func: &mut fasm::Function, binop: &Binop) -> CHSResult<()> {
        _ = func;
        _ = binop;
        todo!()
    }
}
