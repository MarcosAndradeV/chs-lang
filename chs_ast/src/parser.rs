use crate::{nodes::*, RawModule};
use chs_lexer::{Lexer, Span, Token, TokenKind};
use chs_types::CHSType;
use chs_util::{return_chs_error, CHSResult};

pub struct Parser<'src> {
    module: &'src RawModule,
    lexer: Lexer<'src>,
    peeked: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(module: &'src RawModule) -> Self {
        let lexer = Lexer::new(&module.source);
        Self {
            module,
            lexer,
            peeked: None,
        }
    }

    fn next(&mut self) -> Token {
        self.peeked.take().unwrap_or_else(|| self.lexer.next())
    }

    fn expect_kind(&mut self, kind: TokenKind) -> CHSResult<Token> {
        let token = self.next();
        if token.kind != kind {
            return_chs_error!(
                "{} Unexpected token {}, Expect: {:?}",
                token.loc,
                &self.module[&token],
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

    /// Top-level parser loop.
    pub fn parse(mut self) -> CHSResult<Module<'src>> {
        use chs_lexer::TokenKind::*;
        let mut items: Vec<ModuleItem> = vec![];
        while {
            let token = self.next();
            if token.is_eof() {
                false
            } else {
                if token.is_invalid() {
                    return_chs_error!("{} Invalid token '{}'", token.loc, &self.module[&token]);
                }
                match token.kind {
                    KeywordFn => {
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (params, ret_type) = self.parse_fn_type_iterative()?;
                        let body = self.parse_expr_list_iterative(|tk| tk.kind == KeywordEnd)?;
                        let fn_type = CHSType::Function(
                            params.iter().map(|t| t.ty.clone()).collect(),
                            Box::new(ret_type.clone()),
                        );
                        let function_decl = FunctionDecl {
                            name: ident_token,
                            fn_type,
                            params,
                            ret_type,
                            body,
                        };
                        items.push(ModuleItem::Function(function_decl));
                    }
                    KeywordExtern => {
                        self.expect_kind(KeywordFn)?;
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (args, ret_type) = self.parse_fn_type_iterative()?;
                        let fn_type = CHSType::Function(
                            args.into_iter().map(|t| t.ty).collect(),
                            Box::new(ret_type),
                        );
                        let function_decl = ExternFunctionDecl {
                            name: ident_token,
                            fn_type,
                        };
                        items.push(ModuleItem::ExternFunction(function_decl));
                    }
                    _ => {
                        return_chs_error!(
                            "{} Invalid Expression on top level `{}`",
                            token.loc,
                            &self.module[&token]
                        )
                    }
                }
                true
            }
        } {}
        Ok(Module {
            raw_module: self.module,
            items,
        })
    }

    fn parse_expression_iterative(
        &mut self,
        lowest_precedence: Precedence,
    ) -> CHSResult<Expression> {
        // Operand stack
        let mut expr_stack: Vec<Expression> = Vec::new();
        // Operator stack: each operator paired with its token location.
        let mut op_stack: Vec<(Operator, Token)> = Vec::new();

        // First parse a “primary” expression (which itself may be built iteratively).
        expr_stack.push(self.parse_primary_iterative()?);

        // Check for assignment immediately after a primary expression
        // if self.peek().kind == TokenKind::Assign {
        //     let assign_token = self.next(); // consume '='
        //     let value = self.parse_expression_iterative(Precedence::Lowest)?;
        //     let target = expr_stack.pop().unwrap();
        //     return Ok(Expression::Assign(Box::new(nodes::Assign {
        //         token: assign_token,
        //         target,
        //         value,
        //     })));
        // }

        loop {
            let ptoken = self.peek().clone();
            // Handle non-binary postfix operators such as function calls or indexing:
            match ptoken.kind {
                TokenKind::OpenParen => {
                    // Function call: consume the '(' then parse arguments.
                    self.next(); // consume ParenOpen
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
                TokenKind::OpenSquare => {
                    self.next(); // consume '['
                    let index = self.parse_expression_iterative(Precedence::Lowest)?;
                    self.expect_kind(TokenKind::CloseSquare)?;
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
            if ptoken.kind.is_binop() {
                let operator = Operator::from_token(&ptoken, false)?;
                if operator.precedence() > lowest_precedence {
                    op_stack.push((operator, ptoken));
                    self.next(); // consume operator
                                 // Parse next primary expression as the right-hand side.
                    expr_stack.push(self.parse_primary_iterative()?);
                    // After pushing, combine operators if the next operator has lower precedence.
                    while let Some(&(ref top_op, _)) = op_stack.last() {
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
                    "Insufficient operands for operator at {}",
                    &self.module[&token]
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
                "Error combining expression, remaining stack: {:?}",
                expr_stack
            );
        }
        Ok(expr_stack.pop().unwrap())
    }

    // Helper: if the next token is an operator, return it without consuming.
    fn peek_if_op(&mut self) -> Option<&Token> {
        let token = self.peek();
        if token.kind.is_binop() {
            Some(token)
        } else {
            None
        }
    }

    /// Parses a primary expression iteratively (including handling of prefix operators).
    fn parse_primary_iterative(&mut self) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
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
            KeywordIf => {
                self.expect_kind(OpenParen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                self.parse_if_expression_iterative(token, cond)
            }
            KeywordReturn => {
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
            KeywordWhile => {
                self.expect_kind(OpenParen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                let body = self.parse_expr_list_iterative(|t| t.kind == TokenKind::KeywordEnd)?;
                Ok(Expression::WhileExpression(Box::new(WhileExpression {
                    token,
                    cond,
                    body,
                })))
            }
            IntegerNumber | CharacterLiteral => Expression::from_literal_token(token),
            KeywordTrue | KeywordFalse => Ok(Expression::ConstExpression(
                ConstExpression::BooleanLiteral(Span::from(token)),
            )),
            Identifier => Ok(Expression::ConstExpression(ConstExpression::Identifier(
                Span::from(token),
            ))),
            StringLiteral => Ok(Expression::ConstExpression(ConstExpression::StringLiteral(
                Span::from(token),
            ))),
            KeywordCast => {
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
            KeywordSyscall => {
                let ptoken = self.next();
                let args = self.parse_expr_list_iterative(|tk| tk.kind == CloseParen)?;
                if args.len() != 0 {
                    return_chs_error!(
                        "{} Syscall must have at least one argument the syscall number",
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
            _ => return_chs_error!("{} Unexpected token {}", token.loc, &self.module[&token]),
        }
    }

    /// Iterative version of parsing an initialization list.
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
        use chs_lexer::TokenKind::*;
        let mut body = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                KeywordElse => {
                    self.next();
                    let else_body = self.parse_expr_list_iterative(|t| t.kind == KeywordEnd)?;
                    return Ok(Expression::IfElseExpression(Box::new(IfElseExpression {
                        token,
                        cond,
                        body,
                        else_body,
                    })));
                }
                KeywordEnd => {
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
                TokenKind::EOF => return_chs_error!("Expected closing token, found EOF"),
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
        use chs_lexer::TokenKind::*;
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
                EOF => return_chs_error!("Expected closing token, found EOF"),
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
    fn parse_fn_type_iterative(&mut self) -> CHSResult<(Vec<Param>, CHSType)> {
        use chs_lexer::TokenKind::*;
        let mut list = vec![];
        let mut ret_type = CHSType::Void;
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                CloseParen => {
                    self.next();
                    if self.peek().kind == Arrow {
                        self.next();
                        ret_type = self.parse_type_iterative()?;
                    }
                    return Ok((list, ret_type));
                }
                Comma => {
                    self.next();
                    continue;
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
                _ => {
                    let token = self.next();
                    return_chs_error!(
                        "{} Unexpected token in function type `{}`",
                        token.loc,
                        &self.module[&token]
                    )
                }
            }
        }
    }

    /// Iterative version of parsing a function type.
    fn parse_generic_type_iterative(&mut self) -> CHSResult<Vec<CHSType>> {
        use chs_lexer::TokenKind::*;
        let mut list = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                Gt => {
                    self.next();
                    return Ok(list);
                }
                Comma => {
                    self.next();
                    continue;
                }
                _ => {
                    let value = self.parse_type_iterative()?;
                    list.push(value);
                }
            }
        }
    }

    /// Iterative version of parsing a type.
    fn parse_type_iterative(&mut self) -> CHSResult<CHSType> {
        use chs_lexer::TokenKind::*;
        let ttoken = self.next();
        let ttype = match ttoken.kind {
            Identifier if &self.module[&ttoken] == "int" => CHSType::Int,
            Identifier if &self.module[&ttoken] == "uint" => CHSType::UInt,
            Identifier if &self.module[&ttoken] == "void" => CHSType::Void,
            Identifier if &self.module[&ttoken] == "bool" => CHSType::Boolean,
            Identifier if &self.module[&ttoken] == "char" => CHSType::Char,
            Identifier if &self.module[&ttoken] == "string" => CHSType::String,
            Identifier if self.peek().kind == TokenKind::Lt => {
                self.next();
                let v = self.parse_generic_type_iterative()?;
                CHSType::Generic((&self.module[&ttoken]).to_string(), v)
            }
            Identifier => CHSType::Generic((&self.module[&ttoken]).to_string(), vec![]),
            OpenSquare => {
                // TODO: Add support for [<type>; <size>] array types
                let ttp = self.parse_type_iterative()?;
                self.expect_kind(CloseSquare)?;
                CHSType::Slice(Box::new(ttp))
            }
            Asterisk => {
                let ttp = self.parse_type_iterative()?;
                CHSType::Pointer(Box::new(ttp))
            }
            KeywordFn => {
                self.next();
                let (args, ret) = self.parse_fn_type_iterative()?;
                CHSType::Function(args.into_iter().map(|t| t.ty).collect(), Box::new(ret))
            }
            _ => return_chs_error!(
                "{} Type not implemented {}",
                ttoken.loc,
                &self.module[&ttoken]
            ),
        };
        Ok(ttype)
    }
}
