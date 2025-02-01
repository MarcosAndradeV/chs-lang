#![allow(dead_code)]
pub mod fasm;
use std::collections::HashMap;

use chs_ast::nodes::{
    self, Binop, ConstExpression, Expression, IfElseExpression, IfExpression, Operator, Syscall,
    TypedModule, Unop, WhileExpression,
};
use chs_types::TypeMap;
use chs_util::{chs_error, CHSError, CHSResult};
use fasm::{Cond, DataDef, DataDirective, DataExpr, Instr, Register, SizeOperator, Value};

pub struct FasmGenerator {
    label_count: usize,
    str_count: usize,
    bf: bool,
    scopes: Vec<HashMap<String, fasm::Value>>,
    datadefs: Vec<fasm::DataDef>,
    type_map: TypeMap,
}

impl FasmGenerator {
    pub fn generate(tm: TypedModule) -> CHSResult<fasm::Module> {
        let TypedModule {
            file_path,
            function_decls,
            type_defs,
        } = tm;
        let mut gen = Self {
            type_map: type_defs,
            label_count: 0,
            str_count: 0,
            bf: false,
            scopes: vec![],
            datadefs: vec![],
        };
        let mut out = fasm::Module::new(file_path.with_extension("asm"));

        out.push_raw_instr_to_start("pop rdi");
        out.push_raw_instr_to_start("pop rsi");

        let func = out.push_function(fasm::Function::new("print_int"));
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

        for func_decl in function_decls {
            let func = gen.generate_function(func_decl)?;
            out.push_function(func);
        }

        for data in gen.datadefs {
            out.push_data(data);
        }

        Ok(out)
    }

    fn generate_function(&mut self, func_decl: nodes::FunctionDecl) -> CHSResult<fasm::Function> {
        let mut func = fasm::Function::new(func_decl.name);
        self.label_count = 0;
        let mut scope: HashMap<String, fasm::Value> = HashMap::new();
        func.push_block("");
        let cc = Register::get_syscall_call_convention();
        for (i, (name, v)) in func_decl.args.into_iter().enumerate() {
            assert!(i < cc.len());
            let size = SizeOperator::from_chstype(&v, &self.type_map)?;
            let stack_pos = func.allocate_stack(size.byte_size());

            let src = fasm::Value::Register(cc[i]);
            let dst = Value::Memory(size, format!("rbp-{}", stack_pos));
            func.push_instr(Instr::Mov(dst.clone(), src));
            scope.insert(name, dst);
        }
        self.scopes.push(scope);
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
            chs_types::CHSType::Int | chs_types::CHSType::UInt | chs_types::CHSType::Pointer(_) => {
                match last.expect("Expect last value") {
                    Value::Register(Register::Rax) => {}
                    value => func.push_instr(Instr::Mov(Value::Register(Register::Rax), value)),
                }
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
            Expression::ConstExpression(IntegerLiteral(v)) => {
                Ok(Some(Value::Const(SizeOperator::Qword, *v)))
            }
            Expression::ConstExpression(BooleanLiteral(v)) => {
                Ok(Some(Value::Const(SizeOperator::Byte, *v as i64)))
            }
            Expression::ConstExpression(Symbol(v)) => Ok(Some(self.get_var(v)?.clone())),
            Expression::ConstExpression(StringLiteral(v)) => {
                self.str_count += 1;
                let name = format!("str{}", self.str_count);
                self.datadefs.push(DataDef {
                    name: format!("{}_len", name.clone()),
                    directive: DataDirective::Dq,
                    items: vec![DataExpr::Const(v.len() as u64)],
                });
                self.datadefs.push(DataDef {
                    name: name.clone(),
                    directive: DataDirective::Db,
                    items: vec![DataExpr::Str(v.clone())],
                });
                Ok(Some(Value::Label(name)))
            }
            Expression::Cast(v) => {
                let size = SizeOperator::from_chstype(&v.ttype, &self.type_map)?;
                Ok(Some(
                    match self.generate_expression(func, &v.casted)?.unwrap() {
                        Value::Memory(_, addr) => Value::Memory(size, addr),
                        Value::Const(_, v) => Value::Const(size, v),
                        Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                        val => val,
                    },
                ))
            }
            Expression::VarDecl(v) => self.generater_var_decl(func, v),
            Expression::Assign(v) => {
                let size = SizeOperator::from_chstype(
                    v.ttype.as_ref().expect("Expected type"),
                    &self.type_map,
                )?;
                let dst = match &v.assigned {
                    Expression::Unop(u) if u.op == Operator::Deref => {
                        self.generate_assign_deref(func, &u.left, &u.ttype)?
                    }
                    _ => match self.generate_expression(func, &v.assigned)?.unwrap() {
                        Value::Memory(_, a) => Value::Memory(size, a),
                        Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                        dst => dst,
                    },
                };
                let src = match self.generate_expression(func, &v.value)?.unwrap() {
                    Value::Memory(_, a) => Value::Memory(size, a),
                    Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                    src => src,
                };
                if dst != src {
                    func.push_instr(Instr::Mov(dst, src));
                }
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
            Expression::Len(e) => {
                let src = self.generate_expression(func, e)?.unwrap();
                let src = match src {
                    Value::Register(_) => src,
                    Value::Label(_) => src,
                    _ => {
                        func.push_instr(Instr::Mov(Value::Register(Register::Rbx), src));
                        Value::Register(Register::Rbx)
                    }
                };
                func.push_instr(Instr::Mov(
                    Value::Register(Register::Rax),
                    Value::Memory(SizeOperator::Qword, format!("{}-8", src)),
                ));
                Ok(Some(Value::Register(Register::Rax)))
            }
            Expression::Group(e) => self.generate_expression(func, e),
            Expression::Syscall(e) => self.generate_syscall(func, e),
            _ => todo!("generation for expression {:?}", expr),
        }
    }

    fn generater_var_decl(
        &mut self,
        func: &mut fasm::Function,
        v: &nodes::VarDecl,
    ) -> Result<Option<Value>, CHSError> {
        if v.name == "_" {
            self.generate_expression(func, &v.value)?.unwrap();
            return Ok(None);
        }
        let size =
            SizeOperator::from_chstype(v.ttype.as_ref().expect("Expected type"), &self.type_map)?;
        let stack_pos = func.allocate_stack(size.byte_size());

        let src = match self.generate_expression(func, &v.value)?.unwrap() {
            Value::Register(reg) => Value::Register(size.register_for_size(reg)),
            Value::Const(size, c) => Value::Const(size, c),
            src => {
                let reg = size.register_for_size(Register::Rbx);
                func.push_instr(Instr::Mov(Value::Register(reg), src));
                Value::Register(reg)
            }
        };
        let dst = Value::Memory(size, format!("rbp-{}", stack_pos));

        let s = self.scopes.last_mut().expect("Expected scope");
        s.insert(v.name.clone(), dst.clone());

        func.push_instr(Instr::Mov(dst, src));
        Ok(None)
    }

    fn generate_assign_deref(
        &mut self,
        func: &mut fasm::Function,
        left: &Expression,
        ttype: &Option<chs_types::CHSType>,
    ) -> Result<Value, CHSError> {
        let size = SizeOperator::from_chstype(ttype.as_ref().unwrap(), &self.type_map)?;
        let lhs = self.generate_expression(func, left)?.unwrap();
        let lhs = match lhs {
            Value::Register(_) => lhs,
            _ => {
                let reg = size.register_for_size(Register::Rbx);
                func.push_instr(Instr::Mov(Value::Register(reg), lhs));
                Value::Register(reg)
            }
        };
        Ok(Value::Memory(size, lhs.to_string()))
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
            loc,
            op,
            left,
            right,
            ttype,
        } = binop;
        let size = SizeOperator::from_chstype(ttype.as_ref().unwrap(), &self.type_map)?;
        let lhs = {
            let lhs = self.generate_expression(func, left)?.unwrap();
            if lhs.is_register() {
                let reg = Value::Register(size.register_for_size(Register::R12));
                func.push_instr(Instr::Mov(reg.clone(), lhs));
                reg
            } else {
                lhs
            }
        };
        let rhs = self.generate_expression(func, right)?.unwrap();
        match op {
            Operator::LAnd => match (&lhs, &rhs) {
                (Value::Memory(_, _) | Value::Register(_), Value::Const(_, _) | Value::Register(_)) => {
                    func.push_instr(Instr::And(lhs.clone(), rhs));
                    Ok(Some(lhs))
                }
                ( Value::Register(_)|Value::Const(_, _) , Value::Memory(_, _) | Value::Register(_))=> {
                    func.push_instr(Instr::And(rhs.clone(), lhs));
                    Ok(Some(rhs))
                }
                (Value::Memory(_, _), Value::Memory(size, _)) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), rhs));
                    func.push_instr(Instr::And(lhs.clone(), reg));
                    Ok(Some(lhs))
                }
                (Value::Const(_, c1), Value::Const(_, c2)) => {
                    Ok(Some(Value::Const(SizeOperator::Byte, (*c1 != 0 && *c2 != 0) as i64)))
                }
                (lhs, rhs) => todo!("implement generation {lhs} && {rhs}"),
            },
            Operator::LOr => match (&lhs, &rhs) {
                (Value::Memory(_, _) | Value::Register(_), Value::Const(_, _) | Value::Register(_)) => {
                    func.push_instr(Instr::Or(lhs.clone(), rhs));
                    Ok(Some(lhs))
                }
                ( Value::Register(_)|Value::Const(_, _) , Value::Memory(_, _) | Value::Register(_))=> {
                    func.push_instr(Instr::Or(rhs.clone(), lhs));
                    Ok(Some(rhs))
                }
                (Value::Memory(_, _), Value::Memory(size, _)) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), rhs));
                    func.push_instr(Instr::Or(lhs.clone(), reg));
                    Ok(Some(lhs))
                }
                (Value::Const(_, c1), Value::Const(_, c2)) => {
                    Ok(Some(Value::Const(SizeOperator::Byte, (*c1 != 0 || *c2 != 0) as i64)))
                }
                (lhs, rhs) => todo!("implement generation {lhs} || {rhs}"),
            },
            Operator::Plus => match (&lhs, &rhs) {
                (Value::Memory(_, _) | Value::Register(_), Value::Const(_, _) | Value::Register(_)) => {
                    func.push_instr(Instr::Add(lhs.clone(), rhs));
                    Ok(Some(lhs))
                }
                ( Value::Register(_)|Value::Const(_, _) , Value::Memory(_, _) | Value::Register(_))=> {
                    func.push_instr(Instr::Add(rhs.clone(), lhs));
                    Ok(Some(rhs))
                }
                (Value::Memory(_, _), Value::Memory(size, _)) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), rhs));
                    func.push_instr(Instr::Add(lhs.clone(), reg));
                    Ok(Some(lhs))
                }
                (Value::Const(_, c1), Value::Const(_, c2)) => {
                    Ok(Some(Value::Const(SizeOperator::Byte, *c1 + *c2)))
                }
                (lhs, rhs) => todo!("implement generation {lhs} + {rhs}"),
            },
            Operator::Minus => match (&lhs, &rhs) {
                (Value::Memory(_, _) | Value::Register(_), Value::Const(_, _) | Value::Register(_)) => {
                    func.push_instr(Instr::Sub(lhs.clone(), rhs));
                    Ok(Some(lhs))
                }
                ( Value::Register(_)|Value::Const(_, _) , Value::Memory(_, _) | Value::Register(_))=> {
                    func.push_instr(Instr::Sub(rhs.clone(), lhs));
                    Ok(Some(rhs))
                }
                (Value::Memory(_, _), Value::Memory(size, _)) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), rhs));
                    func.push_instr(Instr::Sub(lhs.clone(), reg));
                    Ok(Some(lhs))
                }
                (Value::Const(_, c1), Value::Const(_, c2)) => {
                    Ok(Some(Value::Const(SizeOperator::Byte, *c1 - *c2)))
                }
                (lhs, rhs) => todo!("implement generation {lhs} - {rhs}"),
            },
            Operator::Div => {
                if let (Value::Const(_, c1), Value::Const(_, c2)) = (&lhs, &rhs) {
                    if *c2 == 0 {
                        chs_error!("{} Cannot divide by 0 in constants.", loc)
                    }
                    return Ok(Some(Value::Const(SizeOperator::Byte, *c1 / *c2)))
                }
                func.push_instr(Instr::Mov(Value::Register(Register::Rax), lhs));
                let rhs = match rhs {
                    Value::Register(_) => rhs,
                    Value::Memory(_, _) => rhs,
                    Value::Const(_, _) => {
                        func.push_instr(Instr::Mov(Value::Register(Register::Rbx), rhs));
                        Value::Register(Register::Rbx)
                    }
                    _ => todo!("implement generation div {rhs}"),
                };
                func.push_instr(Instr::Div(rhs));
                Ok(Some(Value::Register(Register::Rax)))
            }
            Operator::Mod => {
                if let (Value::Const(_, c1), Value::Const(_, c2)) = (&lhs, &rhs) {
                    if *c2 == 0 {
                        chs_error!("{} Cannot divide by 0 in constants.", loc)
                    }
                    return Ok(Some(Value::Const(SizeOperator::Byte, *c1 / *c2)))
                }
                func.push_instr(Instr::Mov(Value::Register(Register::Rax), lhs));
                let rhs = match rhs {
                    Value::Register(_) => rhs,
                    Value::Memory(_, _) => rhs,
                    Value::Const(_, _) => {
                        func.push_instr(Instr::Mov(Value::Register(Register::Rbx), rhs));
                        Value::Register(Register::Rbx)
                    }
                    _ => todo!("implement generation mod {rhs}"),
                };
                func.push_instr(Instr::Div(rhs));
                Ok(Some(Value::Register(Register::Rdx)))
            }
            Operator::Mult => {
                if let (Value::Const(_, c1), Value::Const(_, c2)) = (&lhs, &rhs) {
                    return Ok(Some(Value::Const(SizeOperator::Byte, *c1 * *c2)))
                }
                func.push_instr(Instr::Mov(Value::Register(Register::Rax), lhs));
                let rhs = match rhs {
                    Value::Register(_) => rhs,
                    Value::Memory(_, _) => rhs,
                    Value::Const(_, _) => {
                        func.push_instr(Instr::Mov(Value::Register(Register::Rbx), rhs));
                        Value::Register(Register::Rbx)
                    }
                    _ => todo!("implement generation mul {rhs}"),
                };
                func.push_instr(Instr::Mul(rhs));
                Ok(Some(Value::Register(Register::Rax)))
            }
            op => {
                self.bf = true;
                match (&lhs, &rhs) {
                    (Value::Memory(_, _), Value::Const(_, _)) => {
                        func.push_instr(Instr::Cmp(lhs, rhs));
                    }
                    (Value::Memory(_, _), Value::Memory(size, _)) => {
                        let reg = Value::Register(size.register_for_size(Register::Rbx));
                        func.push_instr(Instr::Mov(reg.clone(), rhs));
                        func.push_instr(Instr::Cmp(lhs.clone(), reg));
                    }
                    (Value::Register(reg), Value::Const(size, _)) => {
                        let reg = Value::Register(size.register_for_size(*reg));
                        let treg = Value::Register(size.register_for_size(Register::Rbx));
                        func.push_instr(Instr::Mov(treg.clone(), rhs));
                        func.push_instr(Instr::Cmp(reg, treg));
                    }
                    (Value::Const(_, c1), Value::Const(_, c2)) => {
                        match op {
                            Operator::Eq => {
                                return Ok(Some(Value::Const(SizeOperator::Byte, (*c1 == *c2) as i64)))
                            }
                            Operator::NEq => {
                                return Ok(Some(Value::Const(SizeOperator::Byte, (*c1 != *c2) as i64)))
                            }
                            Operator::Gt => {
                                return Ok(Some(Value::Const(SizeOperator::Byte, (*c1 > *c2) as i64)))
                            }
                            Operator::Lt => {
                                return Ok(Some(Value::Const(SizeOperator::Byte, (*c1 < *c2) as i64)))
                            }
                            op => todo!("implement generation for {op:?}"),
                        }
                    }
                    (lhs, rhs) => todo!("implement generation {lhs} {op:?} {rhs}"),
                }
                match op {
                    Operator::Eq => {
                        func.push_instr(Instr::Set(Cond::E, Value::Register(Register::Al)));
                    }
                    Operator::NEq => {
                        func.push_instr(Instr::Set(Cond::NE, Value::Register(Register::Al)));
                    }
                    Operator::Gt => {
                        func.push_instr(Instr::Set(Cond::L, Value::Register(Register::Al)));
                    }
                    Operator::Lt => {
                        func.push_instr(Instr::Set(Cond::G, Value::Register(Register::Al)));
                    }
                    op => todo!("implement generation for {op:?}"),
                }
                Ok(Some(Value::Register(Register::Al)))
            } // _ => unreachable!(),
        }
    }

    fn generate_unop(
        &mut self,
        func: &mut fasm::Function,
        unop: &Unop,
    ) -> Result<Option<Value>, CHSError> {
        let Unop {
            loc,
            op,
            left,
            ttype,
        } = unop;
        let size = SizeOperator::from_chstype(ttype.as_ref().unwrap(), &self.type_map)?;
        let lhs = self.generate_expression(func, left)?.unwrap();
        match op {
            Operator::Negate => {
                let lhs = match lhs {
                    Value::Const(size, c1) => return Ok(Some(Value::Const(size, -c1))),
                    Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                    _ => {
                        let reg = Value::Register(size.register_for_size(Register::Rbx));
                        func.push_instr(Instr::Mov(reg.clone(), lhs));
                        reg
                    }
                };
                func.push_instr(Instr::Neg(lhs.clone()));
                return Ok(Some(lhs));
            }
            Operator::LNot => {
                let lhs = match lhs {
                    Value::Const(_, c1) => return Ok(Some(Value::Const(SizeOperator::Byte, (c1 == 0) as i64))),
                    Value::Register(_) => lhs,
                    _ => {
                        let reg = Value::Register(size.register_for_size(Register::Rbx));
                        func.push_instr(Instr::Mov(reg.clone(), lhs));
                        reg
                    }
                };
                func.push_instr(Instr::Not(lhs.clone()));
                return Ok(Some(lhs));
            }
            Operator::Refer => {
                let lhs = match lhs {
                    Value::Const(..) => {
                        chs_error!("Cannot take a reference to a literal")
                    }
                    _ => lhs,
                };
                func.push_instr(Instr::Lea(Value::Register(Register::Rax), lhs));
            }
            Operator::Deref => {
                let lhs = match lhs {
                    Value::Const(..) => {
                        chs_error!("{} Cannot take a deref a literal", loc)
                    }
                    Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                    _ => {
                        let reg = Value::Register(size.register_for_size(Register::Rbx));
                        func.push_instr(Instr::Mov(reg.clone(), lhs));
                        reg
                    }
                };
                let dst = Value::Register(size.register_for_size(Register::Rax));
                func.push_instr(Instr::Mov(dst, Value::Memory(size, lhs.to_string())));
            }
            _ => unreachable!(),
        }
        Ok(Some(Value::Register(Register::Rax)))
    }

    fn generate_if(
        &mut self,
        func: &mut fasm::Function,
        e: &IfExpression,
    ) -> Result<Option<Value>, CHSError> {
        let IfExpression { loc: _, cond, body } = e;
        let lafter = self.new_label();
        let v = self.generate_expression(func, cond)?.unwrap();
        if !self.bf {
            match &v {
                Value::Const(size, ..) | Value::Memory(size, ..) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), v.clone()));
                    func.push_instr(Instr::Test(reg.clone(), reg));
                }
                _ => {
                    func.push_instr(Instr::Test(v.clone(), v));
                }
            }
            self.bf = !self.bf
        }
        func.push_instr(Instr::J(Cond::NZ, Value::Label(lafter.clone())));
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(lafter);
        Ok(None)
    }

    fn generate_if_else(
        &mut self,
        func: &mut fasm::Function,
        e: &IfElseExpression,
    ) -> Result<Option<Value>, CHSError> {
        let IfElseExpression {
            loc: _,
            cond,
            body,
            else_body,
        } = e;
        let lafter = self.new_label();
        let lother = self.new_label();
        let v = self.generate_expression(func, cond)?.unwrap();
        if !self.bf {
            match &v {
                Value::Const(size, ..) | Value::Memory(size, ..) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), v.clone()));
                    func.push_instr(Instr::Test(reg.clone(), reg));
                }
                _ => {
                    func.push_instr(Instr::Test(v.clone(), v));
                }
            }
            self.bf = !self.bf
        }
        func.push_instr(Instr::J(Cond::NZ, Value::Label(lother.clone())));
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_instr(Instr::Jmp(Value::Label(lafter.clone())));
        func.push_block(lother);
        self.scopes.push(HashMap::new());
        for expr in else_body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(lafter);
        Ok(None) // TODO: Make the if-else return
    }

    #[allow(unreachable_code)]
    #[allow(unused)]
    fn generate_while(
        &mut self,
        func: &mut fasm::Function,
        e: &WhileExpression,
    ) -> Result<Option<Value>, CHSError> {
        let WhileExpression { loc: _, cond, body } = e;
        let lcond = self.new_label();
        let lbody = self.new_label();
        let lafter = self.new_label();
        func.push_instr(Instr::Jmp(Value::Label(lcond.clone())));
        func.push_block(lbody.clone());
        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(lcond);
        let v = self.generate_expression(func, cond)?.unwrap();
        if !self.bf {
            match &v {
                Value::Const(size, ..) | Value::Memory(size, ..) => {
                    let reg = Value::Register(size.register_for_size(Register::Rbx));
                    func.push_instr(Instr::Mov(reg.clone(), v.clone()));
                    func.push_instr(Instr::Test(reg.clone(), reg));
                }
                _ => {
                    func.push_instr(Instr::Test(v.clone(), v));
                }
            }
            self.bf = !self.bf
        }
        func.push_instr(Instr::J(Cond::NZ, Value::Label(lbody)));

        func.push_block(lafter);

        self.labels_free();
        Ok(None)
    }

    fn new_label(&mut self) -> String {
        self.label_count += 1;
        format!("L{}", self.label_count)
    }

    fn labels_free(&mut self) {
        self.label_count = 0;
    }

    fn generate_syscall(
        &mut self,
        func: &mut fasm::Function,
        e: &Syscall,
    ) -> CHSResult<Option<Value>> {
        let mut args = e.args.iter();
        let n = args.next().unwrap();
        let cc = Register::get_syscall_call_convention();
        for (i, arg) in args.enumerate().rev() {
            let reg = cc[i];
            let src = self.generate_expression(func, arg)?.unwrap();
            func.push_instr(Instr::Mov(Value::Register(reg), src));
        }
        match self.generate_expression(func, n)?.unwrap() {
            Value::Register(Register::Rax) => {}
            n => {
                func.push_instr(Instr::Mov(Value::Register(Register::Rax), n));
            }
        }
        func.push_instr(Instr::Syscall);
        Ok(Some(Value::Register(Register::Rax)))
    }
}
