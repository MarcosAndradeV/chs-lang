#![allow(dead_code)]
pub mod fasm;
use std::collections::HashMap;

use chs_ast::nodes::{self, Binop, ConstExpression, Expression, IfElseExpression, IfExpression, Operator, TypedModule, Unop, WhileExpression};
use chs_types::TypeMap;
use chs_util::{CHSError, CHSResult};
use fasm::{Cond, Instr, Register, SizeOperator, Value};

pub struct FasmGenerator {
    tmp_reg: usize,
    label_count: usize,
    scopes: Vec<HashMap<String, fasm::Value>>,
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
            label_count: 0,
            scopes: vec![],
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
        self.label_count = 0;
        let mut scope: HashMap<String, fasm::Value> = HashMap::new();
        let cc = Register::get_syscall_call_convention();
        for (i, (name, _)) in func_decl.args.into_iter().enumerate() {
            assert!(i < cc.len());
            scope.insert(name, fasm::Value::Register(cc[i].clone()));
        }
        self.scopes.push(scope);
        func.push_block("start");
        let len = func_decl.body.len().saturating_sub(1);
        let mut last: Option<Value> = None;
        for (i, expr) in func_decl.body.iter().enumerate() {
            let last_value = self.generate_expression(&mut func, expr)?;
            if i == len {
                last = last_value;
            }
        }

        match func_decl.ret_type {
            chs_types::CHSType::Void => {}
            chs_types::CHSType::Int | chs_types::CHSType::UInt => {
                func.push_instr(Instr::Mov(
                    Value::Register(Register::Rax),
                    last.expect("Expect last value"),
                ));
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
        let cc = Register::get_syscall_call_convention();
        match expr {
            Expression::ConstExpression(IntegerLiteral(v)) => Ok(Some(Value::Const(*v))),
            Expression::ConstExpression(BooleanLiteral(_)) => {
                todo!()
            }
            Expression::ConstExpression(Symbol(v)) => Ok(Some(self.get_var(v)?.clone())),
            Expression::VarDecl(v) => {
                let stack_pos = func.allocate_stack(8);

                let src = self.generate_expression(func, &v.value)?.unwrap();
                let dst = Value::Memory(SizeOperator::Qword, format!("rbp-{}", stack_pos));

                let s = self.scopes.last_mut().expect("Expected scope");
                s.insert(v.name.clone(), dst.clone());

                func.push_instr(Instr::Mov(dst, src));
                Ok(None)
            }
            Expression::Assign(v) => {
                let dst = self.generate_expression(func, &v.assined)?.unwrap();
                let src = self.generate_expression(func, &v.value)?.unwrap();
                func.push_instr(Instr::Mov(dst, src));
                Ok(None)
            }
            Expression::Call(c) => {
                let f = match &c.caller {
                    Expression::ConstExpression(ConstExpression::Symbol(sym)) => {
                        Value::Label(sym.clone())
                    }
                    _ => todo!(),
                };
                for (i, arg) in c.args.iter().enumerate() {
                    let src = self.generate_expression(func, arg)?.unwrap();
                    if src.is_register() {
                        self.tmp_reg -= 1;
                    }
                    if i <= cc.len() {
                        let dst = Value::Register(cc[i]);
                        func.push_instr(Instr::Mov(dst, src));
                    } else {
                        func.push_instr(Instr::Push(src));
                    }
                }

                func.push_instr(Instr::Call(f));

                // TODO: Make the expression contain the return type. For now it will always return on rax
                Ok(Some(Value::Register(Register::Rax)))
            }
            Expression::Binop(binop) => self.generate_binop(func, binop),
            Expression::Unop(unop) => self.generate_unop(func, unop),
            Expression::IfExpression(e) => self.generate_if(func, e),
            Expression::IfElseExpression(e) => self.generate_if_else(func, e),
            Expression::WhileExpression(e) => self.generate_while(func, e),
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

    fn generate_binop(
        &mut self,
        func: &mut fasm::Function,
        binop: &Binop,
    ) -> CHSResult<Option<Value>> {
        let Binop {
            loc: _,
            op,
            left,
            right,
        } = binop;
        let lhs = self.generate_expression(func, left)?.unwrap();
        if !matches!(lhs, Value::Register(Register::Rax)) {
            func.push_instr(Instr::Mov(Value::Register(Register::Rax), lhs));
        }
        let rhs = self.generate_expression(func, right)?.unwrap();
        match op {
            Operator::Plus => {
                func.push_instr(Instr::Add(Value::Register(Register::Rax), rhs));
            }
            Operator::Minus => {
                func.push_instr(Instr::Sub(Value::Register(Register::Rax), rhs));
            }
            Operator::Div => {
                func.push_instr(Instr::Div(Value::Register(Register::Rax), rhs));
            }
            Operator::Mod => {
                func.push_instr(Instr::Div(Value::Register(Register::Rax), rhs));
                return Ok(Some(Value::Register(Register::Rdx)));
            }
            Operator::Mult => {
                func.push_instr(Instr::Mul(Value::Register(Register::Rax), rhs));
            }
            Operator::Eq => {
                func.push_instr(Instr::Mov(Value::Register(Register::Rbx), Value::Const(0)));
                func.push_instr(Instr::Cmp(Value::Register(Register::Rax), rhs));
                func.push_instr(Instr::Cmove(
                    Cond::E,
                    Value::Register(Register::Rax),
                    Value::Register(Register::Rbx),
                ));
            }
            Operator::NEq => {
                func.push_instr(Instr::Mov(Value::Register(Register::Rbx), Value::Const(0)));
                func.push_instr(Instr::Cmp(Value::Register(Register::Rax), rhs));
                func.push_instr(Instr::Cmove(
                    Cond::NE,
                    Value::Register(Register::Rax),
                    Value::Register(Register::Rbx),
                ));
            }
            Operator::Gt => {
                func.push_instr(Instr::Mov(Value::Register(Register::Rbx), Value::Const(0)));
                func.push_instr(Instr::Cmp(Value::Register(Register::Rax), rhs));
                func.push_instr(Instr::Cmove(
                    Cond::G,
                    Value::Register(Register::Rax),
                    Value::Register(Register::Rbx),
                ));
            }
            Operator::Lt => {
                func.push_instr(Instr::Mov(Value::Register(Register::Rbx), Value::Const(0)));
                func.push_instr(Instr::Cmp(Value::Register(Register::Rax), rhs));
                func.push_instr(Instr::Cmove(
                    Cond::L,
                    Value::Register(Register::Rax),
                    Value::Register(Register::Rbx),
                ));
            }
            _ => unreachable!(),
        }
        Ok(Some(Value::Register(Register::Rax)))
    }

    fn generate_unop(
        &mut self,
        func: &mut fasm::Function,
        unop: &Unop,
    ) -> Result<Option<Value>, CHSError> {
        let Unop { loc: _, op, left } = unop;
        let lhs = self.generate_expression(func, left)?.unwrap();
        match op {
            Operator::Negate => {
                func.push_instr(Instr::Neg(lhs));
            }
            Operator::LNot => {
                func.push_instr(Instr::Not(lhs));
            }
            Operator::Refer => {
                func.push_instr(Instr::Lea(Value::Register(Register::Rax), lhs));
            }
            Operator::Deref => {
                let lhs = match lhs {
                    Value::Register(_) => lhs,
                    _ => {
                        func.push_instr(Instr::Mov(Value::Register(Register::Rbx), lhs));
                        Value::Register(Register::Rbx)
                    }
                };
                func.push_instr(Instr::Mov(
                    Value::Register(Register::Rax),
                    Value::Memory(SizeOperator::Qword, lhs.to_string()),
                ));
            }
            _ => unreachable!(),
        }
        Ok(Some(Value::Register(Register::Rax)))
    }

    fn generate_if(&mut self, func: &mut fasm::Function, e: &IfExpression) -> Result<Option<Value>, CHSError> {
        let IfExpression { loc: _, cond, body } = e;
        self.label_count += 1;
        let l = format!("L{}", self.label_count);
        let cond = self.generate_expression(func, cond)?.unwrap();
        if !matches!(cond, Value::Register(Register::Rax)) {
            func.push_instr(Instr::Mov(Value::Register(Register::Rax), cond));
        }
        func.push_instr(Instr::Test(
            Value::Register(Register::Rax), Value::Register(Register::Rax)
        ));
        func.push_instr(Instr::J(Cond::Z,
            Value::Label(l.clone())
        ));
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(l);
        Ok(None)
    }

    fn generate_if_else(&mut self, func: &mut fasm::Function, e: &IfElseExpression) -> Result<Option<Value>, CHSError> {
        let IfElseExpression { loc: _, cond, body, else_body } = e;
        self.label_count += 1;
        let l1 = format!("L{}", self.label_count);
        self.label_count += 1;
        let l2 = format!("L{}", self.label_count);
        let cond = self.generate_expression(func, cond)?.unwrap();
        if !matches!(cond, Value::Register(Register::Rax)) {
            func.push_instr(Instr::Mov(Value::Register(Register::Rax), cond));
        }
        func.push_instr(Instr::Test(
            Value::Register(Register::Rax), Value::Register(Register::Rax)
        ));
        func.push_instr(Instr::J(Cond::Z,
            Value::Label(l1.clone())
        ));
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_instr(Instr::Jmp(
            Value::Label(l2.clone())
        ));
        func.push_block(l1);
        self.scopes.push(HashMap::new());
        for expr in else_body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(l2);
        Ok(None) // TODO: Make the if-else return
    }

    fn generate_while(&mut self, func: &mut fasm::Function, e: &WhileExpression) -> Result<Option<Value>, CHSError> {
        let WhileExpression { loc: _, cond, body } = e;
        self.label_count += 1;
        let lcond = format!("L{}", self.label_count);
        self.label_count += 1;
        let after = format!("L{}", self.label_count);
        func.push_block(lcond.clone());
        let cond = self.generate_expression(func, cond)?.unwrap();
        if !matches!(cond, Value::Register(Register::Rax)) {
            func.push_instr(Instr::Mov(Value::Register(Register::Rax), cond));
        }
        func.push_instr(Instr::Test(
            Value::Register(Register::Rax), Value::Register(Register::Rax)
        ));
        func.push_instr(Instr::J(Cond::NZ,
            Value::Label(after.clone())
        ));
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_instr(Instr::Jmp(
            Value::Label(lcond)
        ));
        func.push_block(after);
        Ok(None)
    }
}
