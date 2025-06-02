use std::str::FromStr;

use crate::{
    chs_ast::nodes::{FunctionDecl, ModuleItem},
    // chs_lexer::{Lexer, Span, Token, TokenKind},
    chs_types::CHSType,
    chs_util::{CHSError, CHSResult},
    return_chs_error,
};

use chslexer::*;

use super::{RawModule, nodes::*};

pub struct Parser<'src> {
    raw_module: &'src RawModule,
    lexer: PeekableLexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(module: &'src RawModule) -> Self {
        let lexer = PeekableLexer::new(&module.source);
        Self {
            raw_module: module,
            lexer,
        }
    }

    fn get_file_path(&self) -> &str {
        &self.raw_module.file_path
    }

    fn next(&mut self) -> Token {
        self.lexer.next_token()
    }

    fn expect_kind(&mut self, kind: TokenKind) -> CHSResult<Token> {
        let token = self.next();
        if token.kind != kind {
            return_chs_error!(
                "{} Unexpected token {}, Expect: {:?}",
                token.loc,
                token,
                kind
            )
        }
        Ok(token)
    }

    fn peek(&mut self) -> &Token {
        self.lexer.peek_token()
    }

    /// Top-level parser loop.
    pub fn parse(mut self) -> CHSResult<ASTModule<'src>> {
        use TokenKind::*;
        let mut items: Vec<ModuleItem> = vec![];
        while {
            let token = self.next();
            if token.is_eof() {
                false
            } else {
                if token.is_invalid() {
                    return_chs_error!(
                        "{}:{} Invalid token '{}'",
                        self.get_file_path(),
                        token.loc,
                        token
                    );
                }
                match token.kind {
                    Keyword if &token.source == "fn" => {
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (params, ret_type, is_variadic) = self.parse_fn_type_iterative()?;
                        let body = self.parse_expr_list_iterative(|tk| {
                            tk.kind == Keyword && &token.source == "end"
                        })?;
                        let fn_type = if is_variadic {
                            CHSType::VariadicFunction(
                                params.iter().map(|t| t.ty.clone()).collect(),
                                Box::new(ret_type.clone()),
                            )
                        } else {
                            CHSType::Function(
                                params.iter().map(|t| t.ty.clone()).collect(),
                                Box::new(ret_type.clone()),
                            )
                        };
                        let function_decl = FunctionDecl {
                            is_variadic,
                            name: ident_token,
                            fn_type,
                            params,
                            ret_type,
                            body,
                        };
                        items.push(ModuleItem::Function(function_decl));
                    }
                    Keyword if &token.source == "extern" => {
                        self.expect_kind(Keyword)?;
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (params, ret_type, is_variadic) = self.parse_fn_type_iterative()?;
                        let fn_type = if is_variadic {
                            CHSType::VariadicFunction(
                                params.iter().map(|t| t.ty.clone()).collect(),
                                Box::new(ret_type.clone()),
                            )
                        } else {
                            CHSType::Function(
                                params.iter().map(|t| t.ty.clone()).collect(),
                                Box::new(ret_type.clone()),
                            )
                        };
                        let function_decl = ExternFunctionDecl {
                            name: ident_token,
                            fn_type,
                        };
                        items.push(ModuleItem::ExternFunction(function_decl));
                    }
                    _ => {
                        return_chs_error!(
                            "{}:{} Invalid Expression on top level `{}`",
                            self.get_file_path(),
                            token.loc,
                            token
                        )
                    }
                }
                true
            }
        } {}
        Ok(ASTModule {
            raw_module: self.raw_module,
            items,
        })
    }

    fn parse_macro_in_top_level(
        &mut self,
        items: &mut Vec<ModuleItem>,
        token: Token,
    ) -> Result<(), CHSError> {
        let src = &token.source;
        let (macro_, args) = src
            .split_once("(")
            .expect("macro with args always have `(`");
        match macro_ {
            "use" => {
                let (args, _) = args
                    .split_once(")")
                    .expect("macro with args always have `)`");
                items.push(ModuleItem::MacroCall(MacroCall::macro_use(
                    token.clone(),
                    ImportSyntax::from_str(args)?,
                )));
            }
            _ => {
                return_chs_error!(
                    "{}:{} Unknown macro '{}'",
                    self.get_file_path(),
                    token.loc,
                    macro_
                );
            }
        }
        Ok(())
    }

    fn parse_macro(&mut self, _items: &mut Vec<Expression>, token: Token) -> Result<(), CHSError> {
        let src = &token.source;
        let (macro_, _args) = src
            .split_once("(")
            .expect("macro with args always have `(`");
        match macro_ {
            _ => {
                return_chs_error!(
                    "{}:{} Unknown macro '{}'",
                    self.get_file_path(),
                    token.loc,
                    macro_
                );
            }
        }
    }

    pub fn parse_expression_iterative(
        &mut self,
        lowest_precedence: Precedence,
    ) -> CHSResult<Expression> {
        // Operand stack
        let mut expr_stack: Vec<Expression> = Vec::new();
        // Operator stack: each operator paired with its token location.
        let mut op_stack: Vec<(Operator, Token)> = Vec::new();

        // First parse a “primary” expression (which itself may be built iteratively).
        expr_stack.push(self.parse_primary_iterative()?);

        loop {
            let ptoken = self.peek().clone();
            // Handle non-binary postfix operators such as function calls or indexing:
            match ptoken.kind {
                TokenKind::OpenParen => {
                    self.next();
                    let args =
                        self.parse_expr_list_iterative(|tk| tk.kind == TokenKind::CloseParen)?;
                    let callee = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::Call(Box::new(Call {
                        token: ptoken,
                        callee,
                        args,
                    })));
                    continue;
                }
                TokenKind::OpenBracket => {
                    self.next(); // consume '['
                    let index = self.parse_expression_iterative(Precedence::Lowest)?;
                    self.expect_kind(TokenKind::CloseBracket)?;
                    let left = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::Index(Box::new(Index {
                        token: ptoken,
                        base: left,
                        index,
                    })));
                    continue;
                }
                _ => {}
            }

            // Check for a binary operator
            if self.lexer.is_binop(&ptoken.kind) {
                let operator = Operator::from_token(&ptoken, false)?;
                if operator.precedence() > lowest_precedence {
                    op_stack.push((operator, ptoken));
                    self.next(); // consume operator
                    // Parse next primary expression as the right-hand side.
                    expr_stack.push(self.parse_primary_iterative()?);
                    // After pushing, combine operators if the next operator has lower precedence.
                    while let Some((top_op, _)) = op_stack.last() {
                        if let Some(next_token) = self.peek_if_op() {
                            let next_op = Operator::from_token(next_token, false)?;
                            if next_op.precedence() >= top_op.precedence() {
                                break;
                            }
                        }
                        // Combine the top operator with the top two expressions.
                        if op_stack.is_empty() || expr_stack.len() < 2 {
                            break;
                        }
                        let (op, token) = op_stack.pop().unwrap();
                        let right = expr_stack.pop().unwrap();
                        let left = expr_stack.pop().unwrap();
                        if op == Operator::Assign {
                            expr_stack.push(Expression::Assign(Box::new(Assign {
                                token,
                                target: left,
                                value: right,
                            })));
                        } else {
                            expr_stack.push(Expression::Binop(Box::new(Binop {
                                token,
                                op,
                                left,
                                right,
                            })));
                        }
                    }
                    continue;
                }
            }
            break;
        }
        // Combine any remaining operators.
        while let Some((op, token)) = op_stack.pop() {
            if expr_stack.len() < 2 {
                return_chs_error!(
                    "{}:{} Insufficient operands for operator at {}",
                    self.get_file_path(),
                    token.loc,
                    (&token)
                );
            }
            let right = expr_stack.pop().unwrap();
            let left = expr_stack.pop().unwrap();
            expr_stack.push(Expression::Binop(Box::new(Binop {
                token,
                op,
                left,
                right,
            })));
        }
        if expr_stack.len() != 1 {
            return_chs_error!(
                "{}:{} Error combining expression, remaining stack: {:?}",
                self.get_file_path(),
                expr_stack[0].loc(),
                expr_stack
            );
        }
        Ok(expr_stack.pop().unwrap())
    }

    // Helper: if the next token is an operator, return it without consuming.
    fn peek_if_op(&mut self) -> Option<&Token> {
        let token = self.peek();
        if is_binop_default(&token.kind) {
            Some(token)
        } else {
            None
        }
    }

    /// Parses a primary expression iteratively (including handling of prefix operators).
    fn parse_primary_iterative(&mut self) -> CHSResult<Expression> {
        use TokenKind::*;
        let token = self.next();
        match token.kind {
            Identifier if self.peek().kind == Colon => {
                self.next(); // consume colon
                let ttype = if self.peek().kind == Assign {
                    self.expect_kind(Assign)?;
                    None
                } else {
                    let chstype = self.parse_type_iterative()?;
                    self.expect_kind(Assign)?;
                    Some(chstype)
                };
                let value = self.parse_expression_iterative(Precedence::Lowest)?;
                Ok(Expression::VarDecl(Box::new(VarDecl {
                    token,
                    ty: ttype,
                    value,
                })))
            }
            Keyword if &token.source == "if" => {
                self.expect_kind(OpenParen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                self.parse_if_expression_iterative(token, cond)
            }
            Keyword if &token.source == "return" => {
                let expr = if self.peek().kind == TokenKind::SemiColon {
                    None
                } else {
                    Some(self.parse_expression_iterative(Precedence::Lowest)?)
                };
                Ok(Expression::ReturnExpression(Box::new(ReturnExpression {
                    token,
                    expr,
                })))
            }
            Keyword if &token.source == "while" => {
                self.expect_kind(OpenParen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                let body = self.parse_expr_list_iterative(|t| {
                    t.kind == TokenKind::Keyword && &t.source == "end"
                })?;
                Ok(Expression::WhileExpression(Box::new(WhileExpression {
                    token,
                    cond,
                    body,
                })))
            }
            UnknowIntergerLiteral
            | IntegerNumber
            | U32Number
            | U64Number
            | I64Number
            | CharacterLiteral => Expression::from_literal_token(token),
            Keyword if &token.source == "true" || &token.source == "false" => Ok(
                Expression::ConstExpression(ConstExpression::BooleanLiteral(token)),
            ),
            Identifier => Ok(Expression::ConstExpression(ConstExpression::Identifier(
                token,
            ))),
            StringLiteral => Ok(Expression::ConstExpression(ConstExpression::StringLiteral(
                token,
            ))),
            Keyword if &token.source == "cast" => {
                self.expect_kind(OpenParen)?;
                let ttype = self.parse_type_iterative()?;
                self.expect_kind(CloseParen)?;
                let casted = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Cast(Box::new(Cast {
                    token,
                    to_type: ttype,
                    casted,
                })))
            }
            Keyword if &token.source == "syscall" => {
                let ptoken = self.next();
                let args = self.parse_expr_list_iterative(|tk| tk.kind == CloseParen)?;
                if !args.is_empty() {
                    return_chs_error!(
                        "{}:{} Syscall must have at least one argument the syscall number",
                        self.get_file_path(),
                        ptoken.loc
                    );
                }
                Ok(Expression::Syscall(Box::new(Syscall {
                    token: ptoken,
                    arity: args.iter().skip(1).len(),
                    args,
                })))
            }
            // Handle prefix operators
            Ampersand | Asterisk | Bang | Minus => {
                let expr = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Unop(Box::new(Unop {
                    op: Operator::from_token(&token, true)?,
                    token,
                    operand: expr,
                })))
            }
            OpenParen => {
                let expr = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                Ok(Expression::Group(Box::new(expr)))
            }
            // CurlyOpen => self.parse_init_list_iterative(),
            _ => return_chs_error!(
                "{}:{} Unexpected token {}",
                self.get_file_path(),
                token.loc,
                (&token)
            ),
        }
    }

    // Iterative version of parsing an initialization list.
    // fn parse_init_list_iterative(&mut self) -> CHSResult<Expression> {
    //     use chs_lexer::TokenKind::*;
    //     let mut exprs = vec![];
    //     loop {
    //         let ptoken = self.peek();
    //         match ptoken.kind {
    //             CurlyClose => {
    //                 let loc = self.next().loc;
    //                 return Ok(Expression::ExpressionList(ExpressionList {
    //                     loc,
    //                     exprs,
    //                     ttype: None,
    //                 }));
    //             }
    //             Identifier => {
    //                 // In this example we simply advance the token stream.
    //                 self.next();
    //                 let ntoken = self.next();
    //                 self.peeked = Some(ntoken);
    //             }
    //             Comma => {
    //                 self.next();
    //                 continue;
    //             }
    //             _ => {}
    //         }
    //         let value = self.parse_expression_iterative(Precedence::Lowest)?;
    //         exprs.push(value);
    //     }
    // }

    /// Iterative version of parsing an if-expression.
    fn parse_if_expression_iterative(
        &mut self,
        token: Token,
        cond: Expression,
    ) -> CHSResult<Expression> {
        use TokenKind::*;
        let mut body = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                Keyword if &token.source == "else" => {
                    self.next();
                    let else_body = self.parse_expr_list_iterative(|t| {
                        t.kind == Keyword && &token.source == "end"
                    })?;
                    return Ok(Expression::IfElseExpression(Box::new(IfElseExpression {
                        token,
                        cond,
                        body,
                        else_body,
                    })));
                }
                Keyword if &token.source == "end" => {
                    self.next();
                    return Ok(Expression::IfExpression(Box::new(IfExpression {
                        token,
                        cond,
                        body,
                    })));
                }
                Comma | SemiColon => {
                    self.next();
                    continue;
                }
                TokenKind::EOF => return_chs_error!(
                    "{}:{} Expected `end|else` closing token, found EOF",
                    self.get_file_path(),
                    token.loc
                ),
                _ => {
                    let value = self.parse_expression_iterative(Precedence::Lowest)?;
                    body.push(value);
                }
            }
        }
    }

    /// Iterative version of parsing a list of expressions.
    fn parse_expr_list_iterative<F>(&mut self, pred: F) -> CHSResult<Vec<Expression>>
    where
        F: Fn(&Token) -> bool,
    {
        use TokenKind::*;
        let mut exprs = vec![];
        loop {
            let ptoken = self.peek();
            if pred(ptoken) {
                self.next();
                break;
            }

            match ptoken.kind {
                Comma => {
                    self.next(); // skip comma, but don't treat it as end
                    continue;
                }
                SemiColon => {
                    self.next(); // skip `;`, but also treat it as end of current expr
                    continue;
                }
                EOF => {
                    let token = self.next();
                    return_chs_error!(
                        "{}:{} Expected `end` closing token, found EOF",
                        self.get_file_path(),
                        token.loc
                    )
                }
                _ => {
                    let value = self.parse_expression_iterative(Precedence::Lowest)?;
                    exprs.push(value);

                    // Check next token after parsing expression
                    let sep = self.peek();
                    if sep.kind == SemiColon {
                        self.next(); // consume `;`
                        continue;
                    } else if pred(sep) {
                        self.next(); // closing token
                        break;
                    }
                }
            }
        }

        Ok(exprs)
    }

    /// Iterative version of parsing a function type.
    fn parse_fn_type_iterative(&mut self) -> CHSResult<(Vec<Param>, CHSType, bool)> {
        use TokenKind::*;
        let mut list = vec![];
        let mut ret_type = CHSType::Void;
        let mut is_variadic = false;
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                CloseParen => {
                    self.next();
                    if self.peek().kind == Arrow {
                        self.next();
                        ret_type = self.parse_type_iterative()?;
                    }
                    return Ok((list, ret_type, is_variadic));
                }
                Comma => {
                    self.next();
                    continue;
                }
                Identifier if is_variadic => {
                    let token = self.next();
                    return_chs_error!(
                        "{}:{} Unexpected token in function type `{}` after variadic parameter",
                        self.get_file_path(),
                        token.loc,
                        (&token)
                    )
                }
                Identifier => {
                    let token = self.next();
                    self.expect_kind(Colon)?;
                    let value = self.parse_type_iterative()?;
                    list.push(Param {
                        name: token,
                        ty: value,
                    });
                }
                Splat => {
                    self.next();
                    is_variadic = true;
                }
                _ => {
                    let token = self.next();
                    return_chs_error!(
                        "{}:{} Unexpected token in function type `{}`",
                        self.get_file_path(),
                        token.loc,
                        (&token)
                    )
                }
            }
        }
    }

    /// Iterative version of parsing a function type.
    // fn parse_generic_type_iterative(&mut self) -> CHSResult<Vec<CHSType>> {
    //     use TokenKind::*;
    //     let mut list = vec![];
    //     loop {
    //         let ptoken = self.peek();
    //         match ptoken.kind {
    //             Gt => {
    //                 self.next();
    //                 return Ok(list);
    //             }
    //             Comma => {
    //                 self.next();
    //                 continue;
    //             }
    //             _ => {
    //                 let value = self.parse_type_iterative()?;
    //                 list.push(value);
    //             }
    //         }
    //     }
    // }

    /// Iterative version of parsing a type.
    fn parse_type_iterative(&mut self) -> CHSResult<CHSType> {
        use TokenKind::*;
        let ttoken = self.next();
        let ttype = match ttoken.kind {
            Identifier if &ttoken.source == "int" => CHSType::I32,
            Identifier if &ttoken.source == "uint" => CHSType::U32,
            Identifier if &ttoken.source == "i32" => CHSType::I32,
            Identifier if &ttoken.source == "u32" => CHSType::U32,
            Identifier if &ttoken.source == "i64" => CHSType::I64,
            Identifier if &ttoken.source == "u64" => CHSType::U64,
            Identifier if &ttoken.source == "any" => CHSType::Any,
            Identifier if &ttoken.source == "bool" => CHSType::Boolean,
            Identifier if &ttoken.source == "char" => CHSType::Char,
            Identifier if &ttoken.source == "string" => CHSType::String,
            OpenBracket => {
                // TODO: Add support for [<type>; <size>] array types
                let ttp = self.parse_type_iterative()?;
                self.expect_kind(CloseBracket)?;
                CHSType::Slice(Box::new(ttp))
            }
            Asterisk => {
                let ttp = self.parse_type_iterative()?;
                CHSType::Pointer(Box::new(ttp))
            }
            Keyword if &ttoken.source == "fn" => {
                self.next();
                let (args, ret, _) = self.parse_fn_type_iterative()?;
                CHSType::Function(args.into_iter().map(|t| t.ty).collect(), Box::new(ret))
            }
            _ => return_chs_error!(
                "{}:{} Type not implemented {}",
                self.get_file_path(),
                ttoken.loc,
                ttoken
            ),
        };
        Ok(ttype)
    }
}
