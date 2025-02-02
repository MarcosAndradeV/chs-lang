use std::path::PathBuf;

use crate::nodes::{self, *};
use chs_lexer::{Lexer, Token, TokenKind};
use chs_types::CHSType;
use chs_util::{chs_error, CHSResult, Loc};

/// [Token] -> [Module]
#[derive(Default)]
pub struct Parser {
    lexer: Lexer,
    peeked: Option<Token>,
    module: Module,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            module: Module {
                file_path: lexer.get_filename().clone(),
                ..Default::default()
            },
            lexer,
            ..Default::default()
        }
    }

    fn next(&mut self) -> Token {
        loop {
            let token = self
                .peeked
                .take()
                .unwrap_or_else(|| self.lexer.next_token());
            if token.kind == TokenKind::Comment {
                continue;
            }
            return token;
        }
    }

    fn expect_kind(&mut self, kind: TokenKind) -> CHSResult<Token> {
        let token = self.next();
        if token.kind != kind {
            chs_error!(
                "{} Unexpected token {}('{}'), Expect: {}",
                token.loc,
                token.kind,
                token.value,
                kind
            )
        }
        Ok(token)
    }

    fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        self.peeked.as_ref().unwrap()
    }

    pub fn parse(mut self, root: Option<&PathBuf>) -> CHSResult<Module> {
        use chs_lexer::TokenKind::*;
        loop {
            let token = self.next();
            if token.kind.is_eof() {
                break;
            }
            if token.kind == Invalid {
                chs_error!("{} Invalid token '{}'", token.loc, token.value);
            }
            match token.kind {
                Keyword if token.val_eq("use") => {
                    let loc = token.loc;
                    let path = PathBuf::from(self.expect_kind(String)?.value);
                    if !path.exists() {
                        chs_error!("{} file \"{}\" cannot be found.", loc, path.display());
                    }
                    if root.is_some_and(|p| *p == path) {
                        chs_error!("{} Cannot import root file \"{}\" ", loc, path.display());
                    }
                    if path == *self.lexer.get_filename() {
                        chs_error!("{} Cannot import self \"{}\" ", loc, path.display());
                    }
                    if self.module.imported_modules.iter().any(|im| im.path == path) {
                        chs_error!("{} Cannot import file \"{}\" again.", loc, path.display());
                    }
                    let mut p = Parser::new(Lexer::new(path.clone())?);
                    p.module.imported_modules.push(UseModuleDecl {
                        loc,
                        path
                    });
                    let m = p.parse(Some(self.lexer.get_filename()))?;
                    self.module.function_decls.extend(m.function_decls);
                    self.module.global_decls.extend(m.global_decls);
                    self.module.type_decls.extend(m.type_decls);
                    self.module.imported_modules.extend(m.imported_modules);
                }
                Keyword if token.val_eq("fn") => {
                    let loc = token.loc;
                    let token = self.expect_kind(Ident)?;
                    let name = token.value;
                    self.expect_kind(ParenOpen)?;
                    let (args, ret_type) = self.parse_fn_type()?;
                    let body = self.parse_expr_list(|tk| tk.val_eq("end"))?;
                    let fn_type = CHSType::Function(
                        args.clone().into_iter().map(|(_, t)| t).collect(),
                        ret_type.clone().into(),
                    );
                    self.module.global_decls.push(GlobalDecl {
                        loc: loc.clone(),
                        name: name.clone(),
                        ttype: fn_type,
                    });
                    self.module.function_decls.push(FunctionDecl {
                        loc,
                        name,
                        args,
                        ret_type,
                        body,
                    });
                }
                Keyword if token.val_eq("type") => {
                    let token = self.expect_kind(Ident)?;
                    let name = token.value;
                    let chs_type = self.parse_type()?;
                    self.module.type_decls.push(TypeDecl {
                        loc: token.loc,
                        name,
                        ttype: chs_type,
                    });
                }
                _ => {
                    chs_error!(
                        "{} Invalid Expression on top level {}('{}')",
                        token.loc,
                        token.kind,
                        token.value
                    )
                }
            }
        }
        Ok(self.module)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
        let token = self.next();
        let mut left: Expression = match token.kind {
            Ident if self.peek().kind == Colon => {
                self.next();
                let ttype = if self.peek().kind == Assign {
                    self.expect_kind(Assign)?;
                    None
                } else {
                    let chstype = self.parse_type()?;
                    self.expect_kind(Assign)?;
                    Some(chstype)
                };
                let value = self.parse_expression(Precedence::Lowest)?;
                let name = token.value;
                Expression::VarDecl(Box::new(VarDecl {
                    loc: token.loc,
                    name,
                    ttype,
                    value,
                }))
            }
            Keyword if token.val_eq("set") => {
                let loc = token.loc;
                let assigned = self.parse_expression(Precedence::Lowest)?;
                self.expect_kind(Assign)?;
                let value = self.parse_expression(Precedence::Lowest)?;
                Expression::Assign(Box::new(nodes::Assign {
                    loc,
                    assigned,
                    value,
                    ttype: None,
                }))
            }
            Keyword if token.val_eq("if") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let cond = self.parse_expression(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                return self.parse_if_expression(loc, cond);
            }
            Keyword if token.val_eq("while") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let cond = self.parse_expression(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                let body = self.parse_expr_list(|t| t.val_eq("end"))?;
                return Ok(Expression::WhileExpression(Box::new(WhileExpression {
                    loc,
                    cond,
                    body,
                })));
            }
            String | Ident | Integer | Character => Expression::from_literal_token(token)?,
            Keyword if token.val_eq("true") || token.val_eq("false") => {
                Expression::from_literal_token(token)?
            }
            Keyword if token.val_eq("len") => {
                Expression::Len(Box::new(self.parse_expression(Precedence::Prefix)?))
            }
            Keyword if token.val_eq("cast") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let ttype = self.parse_type()?;
                self.expect_kind(ParenClose)?;
                let casted = self.parse_expression(Precedence::Prefix)?;
                Expression::Cast(Box::new(Cast { loc, ttype, casted }))
            }
            Keyword if token.val_eq("syscall") => {
                let ptoken = self.next();
                let args = self.parse_expr_list(|tk| tk.kind == ParenClose)?;
                Expression::Syscall(
                    Syscall {
                        loc: ptoken.loc,
                        arity: args.len(),
                        args,
                    }
                    .into(),
                )
            }
            Ampersand | Asterisk => {
                let expr = self.parse_expression(Precedence::Prefix)?;
                Expression::Unop(
                    Unop {
                        op: Operator::from_token(&token, true)?,
                        loc: token.loc,
                        left: expr,
                        ttype: None,
                    }
                    .into(),
                )
            }
            Bang | Minus => {
                let expr = self.parse_expression(Precedence::Prefix)?;
                Expression::Unop(
                    Unop {
                        op: Operator::from_token(&token, true)?,
                        loc: token.loc,
                        left: expr,
                        ttype: None,
                    }
                    .into(),
                )
            }
            ParenOpen if self.peek().kind == ParenClose => {
                Expression::ConstExpression(ConstExpression::Void)
            }
            ParenOpen => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                Expression::Group(expr.into())
            }
            CurlyOpen => self.parse_init_list()?,
            _ => chs_error!(
                "{} Unexpected token {}('{}')",
                token.loc,
                token.kind,
                token.value
            ),
        };
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                ParenOpen => {
                    let ptoken = self.next();
                    let args = self.parse_expr_list(|tk| tk.kind == ParenClose)?;
                    let call = Expression::Call(
                        Call {
                            loc: ptoken.loc,
                            caller: left,
                            args,
                        }
                        .into(),
                    );
                    left = call;
                    return Ok(left);
                }
                k if k.is_op() => {
                    let operator = Operator::from_token(ptoken, false)?;
                    if precedence < operator.precedence() {
                        let loc = self.next().loc;
                        let infix = self.parse_infix_expression(loc, operator, left)?;
                        left = infix
                    } else {
                        return Ok(left);
                    }
                }
                _ => return Ok(left),
            }
        }
    }

    fn parse_infix_expression(
        &mut self,
        loc: Loc,
        op: Operator,
        left: Expression,
    ) -> CHSResult<Expression> {
        let right = self.parse_expression(op.precedence())?;
        Ok(Expression::Binop(
            Binop {
                loc,
                op,
                right,
                left,
                ttype: None,
            }
            .into(),
        ))
    }

    fn parse_init_list(&mut self) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
        let mut exprs = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                CurlyClose => {
                    let loc = self.next().loc;
                    return Ok(Expression::ExpressionList(ExpressionList { loc, exprs, ttype: None }));
                }
                Ident => {
                    self.next();
                    let ntoken = self.next();
                        self.peeked = Some(ntoken);
                }
                Comma => {
                    self.next();
                    continue;
                }
                _ => {}
            }
            let value = self.parse_expression(Precedence::Lowest)?;
            exprs.push(value);
        }
    }

    fn parse_if_expression(&mut self, loc: Loc, cond: Expression) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
        let mut body = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                Keyword if ptoken.val_eq("else") => {
                    self.next();
                    return Ok(Expression::IfElseExpression(Box::new(IfElseExpression {
                        loc,
                        cond,
                        body,
                        else_body: self.parse_expr_list(|t| t.val_eq("end"))?,
                    })));
                }
                Keyword if ptoken.val_eq("end") => {
                    self.next();
                    return Ok(Expression::IfExpression(Box::new(IfExpression {
                        loc,
                        cond,
                        body,
                    })));
                }
                Comma | Semicolon => {
                    self.next();
                    continue;
                }
                EOF => chs_error!("Expect closing token found `EOF`"),
                _ => {
                    let value = self.parse_expression(Precedence::Lowest)?;
                    body.push(value);
                }
            }
        }
    }

    fn parse_expr_list<F>(&mut self, pred: F) -> CHSResult<Vec<Expression>>
    where
        F: Fn(&Token) -> bool,
    {
        use chs_lexer::TokenKind::*;
        let mut args = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                _ if pred(ptoken) => {
                    self.next();
                    return Ok(args);
                }
                Comma | Semicolon => {
                    self.next();
                    continue;
                }
                EOF => chs_error!("Expect closing token found `EOF`"),
                _ => {
                    let value = self.parse_expression(Precedence::Lowest)?;
                    args.push(value);
                }
            }
        }
    }

    fn parse_fn_type(&mut self) -> CHSResult<(Vec<(String, CHSType)>, CHSType)> {
        use chs_lexer::TokenKind::*;
        let mut list = vec![];
        let mut ret_type = CHSType::Void;
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                ParenClose => {
                    self.next();
                    let ptoken = self.peek();
                    if ptoken.kind == Arrow {
                        self.next();
                        let value = self.parse_type()?;
                        ret_type = value;
                    }
                    return Ok((list, ret_type));
                }
                Comma => {
                    self.next();
                    continue;
                }
                Ident => {
                    let token = self.next();
                    self.expect_kind(Colon)?;
                    let value = self.parse_type()?;
                    list.push((token.value, value));
                }
                _ => chs_error!(""),
            }
        }
    }

    fn parse_fn_type_no_args(&mut self) -> CHSResult<(Vec<CHSType>, CHSType)> {
        use chs_lexer::TokenKind::*;
        let mut list = vec![];
        let mut ret_type = CHSType::Void;
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                ParenClose => {
                    self.next();
                    let ptoken = self.peek();
                    if ptoken.kind == Arrow {
                        self.next();
                        let value = self.parse_type()?;
                        ret_type = value;
                    }
                    return Ok((list, ret_type));
                }
                Comma => {
                    self.next();
                    continue;
                }
                _ => {
                    list.push(self.parse_type()?);
                }
            }
        }
    }

    fn parse_type(&mut self) -> CHSResult<CHSType> {
        use chs_lexer::TokenKind::*;
        let ttoken = self.next();
        let ttype = match ttoken.kind {
            Ident if ttoken.val_eq("int") => CHSType::Int,
            Ident if ttoken.val_eq("uint") => CHSType::UInt,
            Ident if ttoken.val_eq("void") => CHSType::Void,
            Ident if ttoken.val_eq("bool") => CHSType::Boolean,
            Ident if ttoken.val_eq("char") => CHSType::Char,
            Ident if ttoken.val_eq("string") => CHSType::String,
            Ident => CHSType::Alias(ttoken.value),
            Asterisk => {
                let ttp = self.parse_type()?;
                CHSType::Pointer(ttp.into())
            }
            Keyword if ttoken.val_eq("fn") => {
                self.next();
                let (args, ret) = self.parse_fn_type_no_args()?;
                CHSType::Function(args, Box::new(ret))
            }
            Keyword if ttoken.val_eq("distinct") => {
                let ttype = self.parse_type()?;
                CHSType::Distinct(Box::new(ttype))
            }
            _ => chs_error!("Type not implemented {}", ttoken),
        };
        Ok(ttype)
    }
}
