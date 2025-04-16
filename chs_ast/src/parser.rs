use crate::{
    nodes::{self, *},
    RawModule,
};
use chs_lexer::{Lexer, Token, TokenKind};
use chs_types::CHSType;
use chs_util::{chs_error, CHSResult};

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
            chs_error!(
                "{} Unexpected token {}, Expect: {:?}",
                token.loc,
                token.source,
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
        let mut global_decls: Vec<GlobalDecl> = vec![];
        let mut function_decls: Vec<FunctionDecl> = vec![];
        while {
            let token = self.next();
            if token.is_eof() {
                false
            } else {
                if token.is_invalid() {
                    chs_error!("{} Invalid token '{}'", token.loc, token.source);
                }
                match token.kind {
                    KeywordFn => {
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (args, ret_type) = self.parse_fn_type_iterative()?;
                        let body = self.parse_expr_list_iterative(|tk| tk.kind == KeywordEnd)?;
                        let fn_type = CHSType::Function(
                            args.iter().map(|(_, t)| t.clone()).collect(),
                            Box::new(ret_type.clone()),
                        );
                        global_decls.push(GlobalDecl {
                            name: ident_token.clone(),
                            extrn: false,
                            ttype: fn_type,
                        });
                        function_decls.push(FunctionDecl {
                            name: ident_token,
                            args,
                            ret_type,
                            body,
                        });
                    }
                    KeywordExtern => {
                        self.expect_kind(KeywordFn)?;
                        let ident_token = self.expect_kind(Identifier)?;
                        self.expect_kind(OpenParen)?;
                        let (args, ret_type) = self.parse_fn_type_iterative()?;
                        let fn_type = CHSType::Function(
                            args.iter().map(|(_, t)| t.clone()).collect(),
                            Box::new(ret_type.clone()),
                        );
                        global_decls.push(GlobalDecl {
                            name: ident_token,
                            extrn: true,
                            ttype: fn_type,
                        });
                    }
                    _ => {
                        chs_error!(
                            "{} Invalid Expression on top level `{}`",
                            token.loc,
                            token.source
                        )
                    }
                }
                true
            }
        } {}
        Ok(Module {
            raw_module: self.module,
            imported_modules: Default::default(),
            type_decls: Default::default(),
            global_decls,
            function_decls
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

        loop {
            let ptoken = self.peek().clone();
            // Handle non-binary postfix operators such as function calls or indexing:
            match ptoken.kind {
                TokenKind::OpenParen => {
                    // Function call: consume the '(' then parse arguments.
                    self.next(); // consume ParenOpen
                    let args =
                        self.parse_expr_list_iterative(|tk| tk.kind == TokenKind::CloseParen)?;
                    let caller = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::Call(Box::new(Call {
                        token: ptoken,
                        caller,
                        args,
                        ttype: None,
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
                        left,
                        index,
                        ttype: None,
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
                        expr_stack.push(Expression::Binop(Box::new(Binop {
                            token,
                            op,
                            left,
                            right,
                            ttype: None,
                        })));
                    }
                    continue;
                }
            }
            break;
        }
        // Combine any remaining operators.
        while let Some((op, token)) = op_stack.pop() {
            if expr_stack.len() < 2 {
                chs_error!("Insufficient operands for operator at {}", token.source);
            }
            let right = expr_stack.pop().unwrap();
            let left = expr_stack.pop().unwrap();
            expr_stack.push(Expression::Binop(Box::new(Binop {
                token,
                op,
                left,
                right,
                ttype: None,
            })));
        }
        if expr_stack.len() != 1 {
            chs_error!(
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
                    ttype,
                    value,
                })))
            }
            KeywordSet => {
                let assigned = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(Assign)?;
                let value = self.parse_expression_iterative(Precedence::Lowest)?;
                Ok(Expression::Assign(Box::new(nodes::Assign {
                    token,
                    assigned,
                    value,
                    ttype: None,
                })))
            }
            KeywordIf => {
                self.expect_kind(OpenParen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                self.parse_if_expression_iterative(token, cond)
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
            KeywordTrue => Ok(Expression::ConstExpression(
                ConstExpression::BooleanLiteral(true),
            )),
            KeywordFalse => Ok(Expression::ConstExpression(
                ConstExpression::BooleanLiteral(true),
            )),
            Identifier => Ok(Expression::ConstExpression(ConstExpression::Symbol(
                token.source.to_string(),
            ))),
            StringLiteral => Ok(Expression::ConstExpression(ConstExpression::StringLiteral(
                token.source.to_string(),
            ))),
            KeywordCast => {
                self.expect_kind(OpenParen)?;
                let ttype = self.parse_type_iterative()?;
                self.expect_kind(CloseParen)?;
                let casted = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Cast(Box::new(Cast {
                    token,
                    ttype,
                    casted,
                })))
            }
            KeywordSyscall => {
                let ptoken = self.next();
                let args = self.parse_expr_list_iterative(|tk| tk.kind == CloseParen)?;
                Ok(Expression::Syscall(Box::new(Syscall {
                    token: ptoken,
                    arity: args.len(),
                    args,
                })))
            }
            // Handle prefix operators
            Ampersand | Asterisk | Bang | Minus => {
                let expr = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Unop(Box::new(Unop {
                    op: Operator::from_token(&token, true)?,
                    token,
                    left: expr,
                    ttype: None,
                })))
            }
            OpenParen => {
                let expr = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(CloseParen)?;
                Ok(Expression::Group(Box::new(expr)))
            }
            // CurlyOpen => self.parse_init_list_iterative(),
            _ => chs_error!("{} Unexpected token {}", token.loc, token.source),
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
                TokenKind::EOF => chs_error!("Expected closing token, found EOF"),
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
        let mut args = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                _ if pred(ptoken) => {
                    self.next();
                    return Ok(args);
                }
                Comma | SemiColon => {
                    self.next();
                    continue;
                }
                EOF => chs_error!("Expected closing token, found EOF"),
                _ => {
                    let value = self.parse_expression_iterative(Precedence::Lowest)?;
                    args.push(value);
                }
            }
        }
    }

    /// Iterative version of parsing a function type.
    fn parse_fn_type_iterative(&mut self) -> CHSResult<(Vec<(String, CHSType)>, CHSType)> {
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
                    list.push((token.source.to_string(), value));
                }
                _ => chs_error!("{} Unexpected token in function type `{}`", ptoken.loc, ptoken.source),
            }
        }
    }

    /// Iterative version of parsing a type.
    fn parse_type_iterative(&mut self) -> CHSResult<CHSType> {
        use chs_lexer::TokenKind::*;
        let ttoken = self.next();
        let ttype = match ttoken.kind {
            Identifier if ttoken.source.as_ref() == "int" => CHSType::Int,
            Identifier if ttoken.source.as_ref() == "uint" => CHSType::UInt,
            Identifier if ttoken.source.as_ref() == "void" => CHSType::Void,
            Identifier if ttoken.source.as_ref() == "bool" => CHSType::Boolean,
            Identifier if ttoken.source.as_ref() == "char" => CHSType::Char,
            Identifier if ttoken.source.as_ref() == "string" => CHSType::String,
            Asterisk => {
                let ttp = self.parse_type_iterative()?;
                CHSType::Pointer(Box::new(ttp))
            }
            KeywordFn => {
                self.next();
                let (args, ret) = self.parse_fn_type_iterative()?;
                CHSType::Function(args.into_iter().map(|(_, t)| t).collect(), Box::new(ret))
            }
            _ => chs_error!("{} Type not implemented {}", ttoken.loc, ttoken.source),
        };
        Ok(ttype)
    }
}
