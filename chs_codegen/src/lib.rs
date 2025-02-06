#![allow(unused)]
pub mod fasm;
use std::collections::HashMap;

use chs_ast::nodes::{
    self, Array, Assign, Binop, Call, Cast, ConstExpression, Expression, ExpressionList,
    IfElseExpression, IfExpression, Operator, Syscall, TypedModule, Unop, WhileExpression,
};
use chs_types::{CHSType, TypeMap};
use chs_util::{chs_error, CHSError, CHSResult};
use fasm::{Cond, DataDef, DataDirective, DataExpr, Instr, Register, SizeOperator, Value};

pub struct FasmGenerator {
    label_count: usize,
    str_count: usize,
    bool_flag: bool,
    scopes: Vec<HashMap<String, fasm::Value>>,
    datadefs: Vec<fasm::DataDef>,
    type_map: TypeMap,
    temp_regs: usize,
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
            bool_flag: false,
            scopes: vec![],
            datadefs: vec![],
            temp_regs: 0,
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
        self.labels_free();
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
            CHSType::Void => {}
            CHSType::Int | CHSType::UInt | CHSType::Pointer(_) => {
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
        match expr {
            Expression::ConstExpression(IntegerLiteral(v)) => {
                Ok(Some(Value::Const(SizeOperator::Qword, *v)))
            }
            Expression::ConstExpression(CharLiteral(v)) => {
                Ok(Some(Value::Const(SizeOperator::Byte, *v as i64)))
            }
            Expression::ConstExpression(BooleanLiteral(v)) => {
                Ok(Some(Value::Const(SizeOperator::Byte, *v as i64)))
            }
            Expression::ConstExpression(Symbol(v)) => Ok(Some(self.get_var(v)?.clone())),
            Expression::ConstExpression(StringLiteral(v)) => {
                self.str_count += 1;
                let name = format!("str{}", self.str_count);
                self.datadefs.push(DataDef {
                    name: name.clone(),
                    directive: DataDirective::Db,
                    items: vec![DataExpr::Str(v.clone()), DataExpr::Const(0)],
                });
                Ok(Some(Value::Label(name)))
            }
            Expression::Cast(v) => {
                let size = SizeOperator::from_chstype(&v.ttype, &self.type_map)?;
                Ok(Some(
                    match self.generate_expression(func, &v.casted)?.unwrap() {
                        Value::Const(_, v) => Value::Const(size, v),
                        Value::Register(reg) => Value::Register(size.register_for_size(reg)),
                        val => val,
                    },
                ))
            }
            Expression::VarDecl(v) => self.generater_var_decl(func, v),
            Expression::Assign(v) => self.generate_assign(func, v),
            Expression::Unop(v) => self.generate_unop(func, v),
            Expression::Binop(v) => self.generate_binop(func, v),
            Expression::Call(v) => self.generate_call(func, v),
            Expression::Syscall(v) => self.generate_syscall(func, v),
            Expression::Group(v) => self.generate_expression(func, v),
            Expression::WhileExpression(v) => self.generate_while(func, v),
            Expression::IfExpression(v) => self.generate_if(func, v),
            Expression::IfElseExpression(v) => self.generate_if_else(func, v),
            _ => todo!("generation for expression {:?}", expr),
        }
    }

    fn generater_var_decl(
        &mut self,
        func: &mut fasm::Function,
        v: &nodes::VarDecl,
    ) -> Result<Option<Value>, CHSError> {
        if v.name.as_str() == "_" {
            self.generate_expression(func, &v.value)?;
            return Ok(None);
        }

        let size = SizeOperator::from_chstype(v.ttype.as_ref().unwrap(), &self.type_map)?;
        let stack_pos = func.allocate_stack(size.byte_size());

        let dst = Value::Memory(size, format!("rbp-{stack_pos}"));

        let r = self.temp_regs;

        let src = self.generate_expression(func, &v.value)?.unwrap();
        let src = match src {
            Value::Memory(s, _) => {
                let treg = Value::from(s.register_for_size(self.alloc_register()));
                func.push_instr(Instr::Mov(treg.clone(), src));
                treg
            }
            _ => src,
        };

        if self.temp_regs.abs_diff(r) > 0 {
            self.free_all_registers();
        }

        let s = self.scopes.last_mut().expect("Expected scope");
        s.insert(v.name.clone(), dst.clone());

        func.push_instr(Instr::Mov(dst, src));

        Ok(None)
    }

    fn generate_assign(
        &mut self,
        func: &mut fasm::Function,
        e: &Assign,
    ) -> Result<Option<Value>, CHSError> {
        let rhs = self.generate_expression(func, &e.value)?.unwrap();

        match e.assigned {
            Expression::Unop(ref u) if u.op == Operator::Deref => {
                let lhs = self.generate_expression(func, &u.left)?.unwrap();
                let size = SizeOperator::from_chstype(u.ttype.as_ref().unwrap(), &self.type_map)?;
                match (&lhs, &rhs) {
                    (Value::Memory(..), Value::Memory(..)) => {
                        let treg = Value::from(size.register_for_size(self.alloc_register()));
                        func.push_raw_instr(format!("mov {treg}, {lhs}"));
                        func.push_raw_instr(format!("mov [{treg}], {rhs}"));
                        self.free_register();
                    }
                    (Value::Memory(..), Value::Const(s, _)) => {
                        let treg = Value::from(size.register_for_size(self.alloc_register()));
                        func.push_raw_instr(format!("mov {treg}, {lhs}"));
                        func.push_raw_instr(format!("mov {s} [{treg}], {rhs}"));
                        self.free_register();
                    }
                    (Value::Memory(size_operator, _), Value::Register(register)) => todo!(),
                    _ => func.push_raw_instr(format!("mov [{lhs}], {rhs}")),
                }
            }
            _ => {
                let lhs = self.generate_expression(func, &e.assigned)?.unwrap();
                if let Value::Memory(..) = &rhs {
                    let treg = Value::from(self.alloc_register());
                    func.push_raw_instr(format!("mov {treg}, {rhs}"));
                    func.push_raw_instr(format!("mov {lhs}, {treg}"));
                    self.free_register();
                } else {
                    func.push_raw_instr(format!("mov {lhs}, {rhs}"));
                }
            }
        }

        Ok(None)
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
        let lhs = self.generate_expression(func, left)?.unwrap();
        let rhs = self.generate_expression(func, right)?.unwrap();
        match op {
            Operator::Plus | Operator::Minus => {
                use Value::*;
                let inst = match op {
                    Operator::Plus => Instr::Add,
                    Operator::Minus => Instr::Sub,
                    _ => unreachable!(""),
                };
                match (&lhs, &rhs) {
                    (Const(_, l), Const(_, r)) => match op {
                        Operator::Plus => Ok(Some(Value::from((size, *l + *r)))),
                        Operator::Minus => Ok(Some(Value::from((size, *l - *r)))),
                        _ => unreachable!(""),
                    },
                    (Register(reg), Memory(..) | Const(..)) => {
                        func.push_instr(inst(lhs.clone(), rhs.clone()));
                        Ok(Some(Value::from(*reg)))
                    }
                    (Const(..) | Memory(..), Register(reg)) => {
                        func.push_instr(inst(rhs.clone(), lhs.clone()));
                        Ok(Some(Value::from(*reg)))
                    }
                    (Memory(..), Const(..) | Memory(..)) => {
                        let val = Value::from(self.alloc_register());
                        func.push_instr(Instr::Mov(val.clone(), lhs));
                        func.push_instr(inst(val.clone(), rhs));
                        self.free_register();
                        Ok(Some(val))
                    }
                    (Memory(..) | Const(..), Memory(..)) => {
                        let val = Value::from(self.alloc_register());
                        func.push_instr(Instr::Mov(val.clone(), lhs));
                        func.push_instr(inst(val.clone(), rhs));
                        self.free_register();
                        Ok(Some(val))
                    }
                    (Register(dst), Register(src)) => {
                        func.push_instr(inst(lhs.clone(), rhs));
                        Ok(Some(lhs))
                    }
                    _ => match op {
                        Operator::Plus => todo!("{loc} Generation of {lhs} + {rhs}"),
                        Operator::Minus => todo!("{loc} Generation of {lhs} - {rhs}"),
                        _ => unreachable!(""),
                    },
                }
            }
            Operator::Div | Operator::Mult | Operator::Mod => {
                let inst = match op {
                    Operator::Div | Operator::Mod => Instr::Div,
                    Operator::Mult => Instr::Mul,
                    _ => unreachable!(""),
                };
                match (&lhs, &rhs) {
                    (Value::Const(_, l), Value::Const(_, r)) => match op {
                        Operator::Div => Ok(Some(Value::from((size, *l / *r)))),
                        Operator::Mod => Ok(Some(Value::from((size, *l % *r)))),
                        Operator::Mult => Ok(Some(Value::from((size, *l * *r)))),
                        _ => unreachable!(""),
                    },
                    (Value::Const(..), Value::Memory(..)) => {
                        func.push_raw_instr("xor rdx, rdx");
                        func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                        func.push_instr(inst(rhs));
                        match op {
                            Operator::Mod => Ok(Some(Value::from(Register::Rdx))),
                            Operator::Mult | Operator::Div => Ok(Some(Value::from(Register::Rax))),
                            _ => unreachable!(""),
                        }
                    }
                    (Value::Memory(..), Value::Const(..)) => {
                        func.push_raw_instr("xor rdx, rdx");
                        func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                        let r = self.alloc_register();
                        func.push_instr(Instr::Mov(Value::from(r), rhs));
                        func.push_instr(inst(Value::from(r)));
                        self.free_register();
                        match op {
                            Operator::Mod => Ok(Some(Value::from(Register::Rdx))),
                            Operator::Mult | Operator::Div => Ok(Some(Value::from(Register::Rax))),
                            _ => unreachable!(""),
                        }
                    }
                    (Value::Memory(..), Value::Memory(..)) => {
                        func.push_raw_instr("xor rdx, rdx");
                        func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                        let r = self.alloc_register();
                        func.push_instr(Instr::Mov(Value::from(r), rhs));
                        func.push_instr(inst(Value::from(r)));
                        self.free_register();
                        match op {
                            Operator::Mod => Ok(Some(Value::from(Register::Rdx))),
                            Operator::Mult | Operator::Div => Ok(Some(Value::from(Register::Rax))),
                            _ => unreachable!(""),
                        }
                    }
                    (Value::Memory(..), Value::Memory(..)) if lhs == rhs => match op {
                        Operator::Div => Ok(Some(Value::from((size, 1)))),
                        Operator::Mod => Ok(Some(Value::from((size, 0)))),
                        Operator::Mult => {
                            func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                            func.push_instr(Instr::Mul(Value::from(Register::Rax)));
                            Ok(Some(Value::from(Register::Rax)))
                        }
                        _ => unreachable!(""),
                    },
                    (Value::Memory(..), Value::Memory(..)) => todo!(),
                    (Value::Register(reg), Value::Memory(..) | Value::Register(..)) => {
                        if *reg != Register::Rax {
                            func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                        }
                        func.push_instr(inst(rhs));
                        match op {
                            Operator::Mod => Ok(Some(Value::from(Register::Rdx))),
                            Operator::Mult | Operator::Div => Ok(Some(Value::from(Register::Rax))),
                            _ => unreachable!(""),
                        }
                    }
                    (Value::Register(reg), Value::Const(..)) => {
                        if *reg != Register::Rax {
                            func.push_instr(Instr::Mov(Value::from(Register::Rax), lhs));
                        }
                        let r = self.alloc_register();
                        func.push_instr(Instr::Mov(Value::from(r), rhs));
                        func.push_instr(inst(Value::from(r)));
                        self.free_register();
                        match op {
                            Operator::Mod => Ok(Some(Value::from(Register::Rdx))),
                            Operator::Mult | Operator::Div => Ok(Some(Value::from(Register::Rax))),
                            _ => unreachable!(""),
                        }
                    }
                    _ => match op {
                        Operator::Div => todo!("{loc} Generation of {lhs} / {rhs}"),
                        Operator::Mult => todo!("{loc} Generation of {lhs} * {rhs}"),
                        Operator::Mod => todo!("{loc} Generation of {lhs} % {rhs}"),
                        _ => unreachable!(""),
                    },
                }
            }
            Operator::LOr => match (&lhs, &rhs) {
                (Value::Const(_, l), Value::Const(_, r)) => {
                    Ok(Some(Value::from((size, (*l != 0 || *r != 0) as i64))))
                }
                (Value::Memory(..) | Value::Register(..), Value::Const(s, 1))
                | (Value::Const(s, 1), Value::Memory(..) | Value::Register(..)) => {
                    Ok(Some(Value::Const(*s, 1)))
                }
                (Value::Memory(..) | Value::Register(..), Value::Const(_, 0)) => Ok(Some(lhs)),
                (Value::Const(_, 0), Value::Memory(..) | Value::Register(..)) => Ok(Some(rhs)),
                (
                    Value::Memory(..) | Value::Register(..),
                    Value::Memory(..) | Value::Register(..),
                ) => {
                    func.push_instr(Instr::Mov(Value::from(Register::Al), lhs));
                    let r = size.register_for_size(self.alloc_register());
                    func.push_instr(Instr::Mov(Value::from(r), rhs));
                    func.push_instr(Instr::Or(Value::from(Register::Al), Value::from(r)));
                    self.free_register();
                    Ok(Some(Value::from(Register::Al)))
                }
                _ => todo!("{loc} Generation of {lhs} || {rhs}"),
            },
            Operator::LAnd => match (&lhs, &rhs) {
                (Value::Const(_, l), Value::Const(_, r)) => {
                    Ok(Some(Value::from((size, (*l != 0 && *r != 0) as i64))))
                }
                (Value::Memory(..) | Value::Register(..), Value::Const(s, 1))
                | (Value::Const(s, 1), Value::Memory(..) | Value::Register(..)) => {
                    Ok(Some(Value::Const(*s, 1)))
                }
                (Value::Memory(..) | Value::Register(..), Value::Const(s, 0))
                | (Value::Const(s, 0), Value::Memory(..) | Value::Register(..)) => {
                    Ok(Some(Value::Const(*s, 0)))
                }
                (
                    Value::Memory(..) | Value::Register(..),
                    Value::Memory(..) | Value::Register(..),
                ) => {
                    func.push_instr(Instr::Mov(Value::from(Register::Al), lhs));
                    let r = size.register_for_size(self.alloc_register());
                    func.push_instr(Instr::Mov(Value::from(r), rhs));
                    func.push_instr(Instr::And(Value::from(Register::Al), Value::from(r)));
                    self.free_register();
                    Ok(Some(Value::from(Register::Al)))
                }
                _ => todo!("{loc} Generation of {lhs} && {rhs}"),
            },
            Operator::Or => todo!(),
            Operator::And => todo!(),
            Operator::Eq | Operator::NEq | Operator::Gt | Operator::Lt => {
                let cond = match op {
                    Operator::Eq => Cond::E,
                    Operator::NEq => Cond::NE,
                    Operator::Gt => Cond::G,
                    Operator::Lt => Cond::L,
                    _ => unreachable!(""),
                };
                match (&lhs, &rhs) {
                    (Value::Const(_, l), Value::Const(_, r)) => match op {
                        Operator::Eq => Ok(Some(Value::from((size, (*l == *r) as i64)))),
                        Operator::NEq => Ok(Some(Value::from((size, (*l != *r) as i64)))),
                        Operator::Gt => Ok(Some(Value::from((size, (*l > *r) as i64)))),
                        Operator::Lt => Ok(Some(Value::from((size, (*l < *r) as i64)))),
                        _ => unreachable!(""),
                    },
                    (
                        Value::Memory(..) | Value::Register(..) | Value::Const(..),
                        Value::Memory(..) | Value::Register(..) | Value::Const(..),
                    ) => {
                        {
                            let rax = size.register_for_size(Register::Rax);
                            let treg = size.register_for_size(self.alloc_register());
                            func.push_instr(Instr::Mov(Value::from(rax), lhs));
                            func.push_instr(Instr::Mov(Value::from(treg), rhs));
                            func.push_instr(Instr::Cmp(Value::from(rax), Value::from(treg)));
                            self.free_register();
                        }
                        {
                            let rcx = Register::Rcx;
                            let treg = self.alloc_register();
                            func.push_instr(Instr::Mov(
                                Value::from(rcx),
                                Value::Const(SizeOperator::Qword, 0),
                            ));
                            func.push_instr(Instr::Mov(
                                Value::from(treg),
                                Value::Const(SizeOperator::Qword, 1),
                            ));
                            func.push_instr(Instr::Cmove(
                                cond,
                                Value::from(rcx),
                                Value::from(treg),
                            ));
                            self.free_register();
                        }
                        Ok(Some(Value::from(Register::Cl)))
                    }
                    _ => match op {
                        Operator::Eq => todo!("{loc} Generation of {lhs} == {rhs}"),
                        Operator::NEq => todo!("{loc} Generation of {lhs} != {rhs}"),
                        _ => unreachable!(),
                    },
                }
            }
            _ => unreachable!("Not binop"),
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
        match op {
            Operator::Negate => {
                let src = self.generate_expression(func, left)?.unwrap();
                match src {
                    Value::Memory(size_operator, _) => todo!(),
                    Value::Register(register) => todo!(),
                    Value::Const(s, n) => Ok(Some(Value::Const(s, -n))),
                    Value::Label(_) => todo!(),
                }
            }
            Operator::LNot => todo!(),
            Operator::Refer => match left {
                Expression::Binop(binop) => todo!(),
                Expression::Unop(unop) => todo!(),
                Expression::Call(call) => todo!(),
                Expression::Cast(cast) => todo!(),
                Expression::Syscall(syscall) => todo!(),
                Expression::Array(array) => todo!(),
                Expression::Len(expression) => todo!(),
                Expression::Group(expression) => todo!(),
                Expression::ConstExpression(ConstExpression::Symbol(sym)) => {
                    let dst = Value::from(self.alloc_register());
                    func.push_instr(Instr::Lea(dst.clone(), self.get_var(sym)?.clone()));
                    Ok(Some(dst))
                }
                Expression::ConstExpression(..) | Expression::ExpressionList(..) => {
                    todo!("Refer to `{{...}}` and consts")
                }
                _ => unreachable!("Refer void"),
            },
            Operator::Deref => {
                let ttype = ttype.as_ref().unwrap();
                let size = SizeOperator::from_chstype(ttype, &self.type_map)?;
                let dst = self.alloc_register();
                func.push_raw_instr(format!("xor {dst}, {dst}"));
                let src = self.generate_expression(func, left)?.unwrap();
                match &src {
                    Value::Memory(_, addr) => {
                        func.push_raw_instr(format!("mov {dst}, [{addr}]"));
                        let dst2 = Value::Register(size.register_for_size(dst));
                        func.push_raw_instr(format!("mov {dst2}, [{dst}]"));
                        Ok(Some(dst2))
                    }
                    _ => {
                        let dst = Value::Register(size.register_for_size(dst));
                        func.push_raw_instr(format!("mov {dst}, [{src}]"));
                        Ok(Some(dst))
                    }
                }
            }
            _ => unreachable!("Not Unop"),
        }
    }

    fn generate_if(
        &mut self,
        func: &mut fasm::Function,
        e: &IfExpression,
    ) -> Result<Option<Value>, CHSError> {
        let IfExpression { loc, cond, body } = e;
        let lafter = self.new_label();
        let vcond = self.generate_expression(func, &cond)?.unwrap();
        func.push_raw_instr(format!("xor rax, rax"));
        func.push_raw_instr(format!("mov al, {vcond}"));
        func.push_raw_instr(format!("test rax, rax"));
        func.push_raw_instr(format!("jz .{lafter}"));

        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_block(&lafter);
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
        let lelse = self.new_label();
        let vcond = self.generate_expression(func, &cond)?.unwrap();
        func.push_raw_instr(format!("xor rax, rax"));
        func.push_raw_instr(format!("mov al, {vcond}"));
        func.push_raw_instr(format!("test rax, rax"));
        func.push_raw_instr(format!("jz .{lelse}"));

        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_raw_instr(format!("jmp .{lafter}"));

        func.push_block(&lelse);
        self.scopes.push(HashMap::new());
        for expr in else_body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();
        func.push_block(&lafter);
        Ok(None)
    }

    #[allow(unreachable_code)]
    #[allow(unused)]
    fn generate_while(
        &mut self,
        func: &mut fasm::Function,
        e: &WhileExpression,
    ) -> Result<Option<Value>, CHSError> {
        let WhileExpression { loc, cond, body } = e;
        let lcond = self.new_label();
        func.push_instr(Instr::Jmp(lcond.clone()));
        let lbody = self.new_label();
        func.push_block(&lbody);

        self.scopes.push(HashMap::new());
        for expr in body {
            self.generate_expression(func, expr)?;
        }
        self.scopes.pop();

        func.push_block(&lcond);
        let vcond = self.generate_expression(func, &cond)?.unwrap();
        func.push_raw_instr(format!("xor rax, rax"));
        func.push_raw_instr(format!("mov al, {vcond}"));
        func.push_raw_instr(format!("test rax, rax"));
        func.push_raw_instr(format!("jnz .{lbody}"));
        // match cond {
        //     Expression::Binop(e) if e.op == Operator::Eq => {
        //         let lhs = self.generate_expression(func, &e.left)?.unwrap();
        //         let rhs = self.generate_expression(func, &e.right)?.unwrap();
        //         if lhs == rhs {
        //             func.push_instr(Instr::Jmp(lbody.clone()));
        //             return Ok(None);
        //         }
        //         func.push_instr(Instr::Cmp(lhs, rhs));
        //         func.push_instr(Instr::J(Cond::NE, lbody));
        //     }
        //     Expression::Binop(e) => chs_error!("{} Unimplemented binop in while condition", loc),
        //     _ => chs_error!("{} Forbiden expression in while condition", loc),
        // }
        Ok(None)
    }

    fn new_label(&mut self) -> String {
        self.label_count += 1;
        format!("L{}", self.label_count)
    }

    fn labels_free(&mut self) {
        self.label_count = 0;
    }

    fn alloc_register(&mut self) -> Register {
        assert!(self.temp_regs <= Register::get_callee_saved().len());
        self.temp_regs += 1;
        Register::get_callee_saved()[self.temp_regs - 1]
    }

    fn free_register(&mut self) {
        self.temp_regs -= 1;
    }

    fn free_all_registers(&mut self) {
        self.temp_regs = 0;
    }

    fn generate_expression_list(
        &mut self,
        _func: &mut fasm::Function,
        _e: &ExpressionList,
    ) -> CHSResult<Option<Value>> {
        todo!("Generation of expression list")
    }

    fn generate_array(&self, func: &mut fasm::Function, e: &Array) -> CHSResult<Option<Value>> {
        let s = func.allocate_stack(0);
        let size_operator = SizeOperator::from_chstype(&e.ttype, &self.type_map)?;
        func.allocate_stack(e.size as usize * size_operator.byte_size());
        let mem = Value::Memory(size_operator, format!("rbp-{s}"));
        func.push_instr(Instr::Lea(Value::Register(Register::Rax), mem));
        Ok(Some(Value::Register(Register::Rax)))
    }

    fn generate_call(
        &mut self,
        func: &mut fasm::Function,
        c: &Call,
    ) -> Result<Option<Value>, CHSError> {
        let cc = Register::get_syscall_call_convention();
        let f = match &c.caller {
            Expression::ConstExpression(ConstExpression::Symbol(sym)) => sym.clone(),
            _ => todo!("Do other types of call"),
        };
        for (i, arg) in c.args.iter().enumerate() {
            let src = self.generate_expression(func, arg)?.unwrap();
            if i <= cc.len() {
                let reg = cc[i];
                let dst = match src {
                    Value::Const(size, _) => Value::Register(size.register_for_size(reg)),
                    Value::Memory(size, _) => Value::Register(size.register_for_size(reg)),
                    _ => Value::Register(reg),
                };
                func.push_instr(Instr::Mov(dst, src));
            } else {
                func.push_instr(Instr::Push(src));
            }
        }
        func.push_instr(Instr::Call(f));
        // TODO: Make the expression contain the return type. For now it will always return on rax
        Ok(Some(Value::Register(Register::Rax)))
    }

    fn generate_syscall(
        &mut self,
        func: &mut fasm::Function,
        c: &Syscall,
    ) -> Result<Option<Value>, CHSError> {
        let cc = Register::get_syscall_call_convention_with_rax();
        for (i, arg) in c.args.iter().enumerate().rev() {
            let src = self.generate_expression(func, arg)?.unwrap();
            if i <= cc.len() {
                let reg = cc[i];
                let dst = match src {
                    Value::Const(size, _) => Value::Register(size.register_for_size(reg)),
                    Value::Memory(size, _) => Value::Register(size.register_for_size(reg)),
                    _ => Value::Register(reg),
                };
                func.push_instr(Instr::Mov(dst, src));
            } else {
                func.push_instr(Instr::Push(src));
            }
        }
        func.push_instr(Instr::Syscall);
        // TODO: Make the expression contain the return type. For now it will always return on rax
        Ok(Some(Value::Register(Register::Rax)))
    }
}
