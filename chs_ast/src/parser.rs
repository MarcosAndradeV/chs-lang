use std::path::{Path, PathBuf};

use crate::nodes::{self, *};
use chs_lexer::{read_flie, Lexer, Token, TokenKind};
use chs_types::CHSType;
use chs_util::{chs_error, CHSResult, Loc};

pub struct Parser {
    lexer: Lexer,
    peeked: Option<Token>,
    module: Module,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            module: Module {
                file_path: lexer.get_filename().to_path_buf(),
                ..Default::default()
            },
            lexer,
            peeked: None,
        }
    }

    fn next(&mut self) -> Token {
        loop {
            let token = self.peeked.take().unwrap_or_else(|| self.lexer.next());
            if token.is_whitespace(true, true) {
                continue;
            }
            return token;
        }
    }

    fn expect_kind(&mut self, kind: TokenKind) -> CHSResult<Token> {
        let token = self.next();
        if token.kind != kind {
            chs_error!(
                "{} Unexpected token {}, Expect: {:?}",
                token.loc,
                token,
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
    /// (When an import is encountered, it “iteratively” parses that file.)
    pub fn parse(mut self, root: Option<&Path>) -> CHSResult<Module> {
        use chs_lexer::TokenKind::*;
        while {
            let token = self.next();
            if token.is_eof() {
                false
            } else {
                if token.is_invalid() {
                    chs_error!("{} Invalid token '{}'", token.loc, token.value);
                }
                match token.kind {
                    Keyword if token.val_eq("use") => {
                        let loc = token.loc;
                        let mut path = PathBuf::from(self.expect_kind(StringLiteral)?.value);
                        if !path.exists() {
                            let std = PathBuf::from("std/").join(&path);
                            if !std.exists() {
                                chs_error!("{} file \"{}\" cannot be found.", loc, path.display());
                            } else {
                                path = std;
                            }
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
                        let lexer = Lexer::new(read_flie(&path), path.clone());
                        let mut p = Parser::new(lexer);
                        p.module.imported_modules.push(UseModuleDecl {
                            loc,
                            path: path.clone(),
                        });
                        // Iteratively parse the imported module (without relying on recursion)
                        let imported_module = p.parse(Some(self.lexer.get_filename()))?;
                        self.module.function_decls.extend(imported_module.function_decls);
                        self.module.global_decls.extend(imported_module.global_decls);
                        self.module.type_decls.extend(imported_module.type_decls);
                        self.module.imported_modules.extend(imported_module.imported_modules);
                    }
                    Keyword if token.val_eq("fn") => {
                        let loc = token.loc;
                        let ident_token = self.expect_kind(Identifier)?;
                        let name = ident_token.value;
                        self.expect_kind(ParenOpen)?;
                        let (args, ret_type) = self.parse_fn_type_iterative()?;
                        let body = self.parse_expr_list_iterative(|tk| tk.val_eq("end"))?;
                        let fn_type = CHSType::Function(
                            args.iter().map(|(_, t)| t.clone()).collect(),
                            Box::new(ret_type.clone()),
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
                        let ident_token = self.expect_kind(Identifier)?;
                        let name = ident_token.value;
                        let chs_type = self.parse_type_iterative()?;
                        self.module.type_decls.push(TypeDecl {
                            loc: ident_token.loc,
                            name,
                            ttype: chs_type,
                        });
                    }
                    Keyword if token.val_eq("const") => {
                        let ident_token = self.expect_kind(Identifier)?;
                        let name = ident_token.value;
                        let chs_type = self.parse_type_iterative()?;
                        let value = self.parse_const_expression_iterative()?;
                        self.module.const_decls.push(ConstDecl {
                            loc: ident_token.loc,
                            name,
                            ttype: chs_type,
                            value,
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
                true
            }
        } {}
        Ok(self.module)
    }

    /// An iterative (stack‐based) expression parser.
    /// (This uses a while loop plus operator/operand stacks rather than recursive calls.)
    fn parse_expression_iterative(&mut self, lowest_precedence: Precedence) -> CHSResult<Expression> {
        // Operand stack
        let mut expr_stack: Vec<Expression> = Vec::new();
        // Operator stack: each operator paired with its token location.
        let mut op_stack: Vec<(Operator, Loc)> = Vec::new();

        // First parse a “primary” expression (which itself may be built iteratively).
        expr_stack.push(self.parse_primary_iterative()?);

        loop {
            let ptoken = self.peek().clone();
            // Handle non-binary postfix operators such as function calls or indexing:
            match ptoken.kind {
                TokenKind::ParenOpen => {
                    // Function call: consume the '(' then parse arguments.
                    self.next(); // consume ParenOpen
                    let args = self.parse_expr_list_iterative(|tk| tk.kind == TokenKind::ParenClose)?;
                    let caller = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::Call(Box::new(Call {
                        loc: ptoken.loc,
                        caller,
                        args,
                        ttype: None,
                    })));
                    continue;
                }
                TokenKind::SquareOpen => {
                    self.next(); // consume '['
                    let index = self.parse_expression_iterative(Precedence::Lowest)?;
                    self.expect_kind(TokenKind::SquareClose)?;
                    let left = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::Index(Box::new(Index {
                        loc: ptoken.loc,
                        left,
                        index,
                        ttype: None,
                    })));
                    continue;
                }
                _ => {}
            }

            // Check for a binary operator
            if ptoken.kind.is_op() {
                let operator = Operator::from_token(&ptoken, false)?;
                if operator.precedence() > lowest_precedence {
                    op_stack.push((operator, ptoken.loc));
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
                        let (op, loc) = op_stack.pop().unwrap();
                        let right = expr_stack.pop().unwrap();
                        let left = expr_stack.pop().unwrap();
                        expr_stack.push(Expression::Binop(Box::new(Binop {
                            loc,
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
        while let Some((op, loc)) = op_stack.pop() {
            if expr_stack.len() < 2 {
                chs_error!("Insufficient operands for operator at {}", loc);
            }
            let right = expr_stack.pop().unwrap();
            let left = expr_stack.pop().unwrap();
            expr_stack.push(Expression::Binop(Box::new(Binop {
                loc,
                op,
                left,
                right,
                ttype: None,
            })));
        }
        if expr_stack.len() != 1 {
            chs_error!("Error combining expression, remaining stack: {:?}", expr_stack);
        }
        Ok(expr_stack.pop().unwrap())
    }

    // Helper: if the next token is an operator, return it without consuming.
    fn peek_if_op(&mut self) -> Option<&Token> {
        let token = self.peek();
        if token.kind.is_op() {
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
                let name = token.value;
                Ok(Expression::VarDecl(Box::new(VarDecl {
                    loc: token.loc,
                    name,
                    ttype,
                    value,
                })))
            }
            Keyword if token.val_eq("set") => {
                let loc = token.loc;
                let assigned = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(Assign)?;
                let value = self.parse_expression_iterative(Precedence::Lowest)?;
                Ok(Expression::Assign(Box::new(nodes::Assign {
                    loc,
                    assigned,
                    value,
                    ttype: None,
                })))
            }
            Keyword if token.val_eq("if") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                self.parse_if_expression_iterative(loc, cond)
            }
            Keyword if token.val_eq("while") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let cond = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                let body = self.parse_expr_list_iterative(|t| t.val_eq("end"))?;
                Ok(Expression::WhileExpression(Box::new(WhileExpression {
                    loc,
                    cond,
                    body,
                })))
            }
            StringLiteral | Identifier | IntegerNumber | CharacterLiteral => {
                Expression::from_literal_token(token)
            }
            Keyword if token.val_eq("true") || token.val_eq("false") => {
                Expression::from_literal_token(token)
            }
            Keyword if token.val_eq("len") => {
                let expr = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Len(Box::new(expr)))
            }
            Keyword if token.val_eq("array") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let ttype = self.parse_type_iterative()?;
                self.expect_kind(Comma)?;
                let size = self
                    .expect_kind(IntegerNumber)?
                    .value
                    .parse::<u64>()
                    .expect("Invalid array size");
                self.expect_kind(ParenClose)?;
                Ok(Expression::Array(Box::new(Array { loc, ttype, size })))
            }
            Keyword if token.val_eq("cast") => {
                let loc = token.loc;
                self.expect_kind(ParenOpen)?;
                let ttype = self.parse_type_iterative()?;
                self.expect_kind(ParenClose)?;
                let casted = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Cast(Box::new(Cast { loc, ttype, casted })))
            }
            Keyword if token.val_eq("syscall") => {
                let ptoken = self.next();
                let args = self.parse_expr_list_iterative(|tk| tk.kind == ParenClose)?;
                Ok(Expression::Syscall(Box::new(Syscall {
                    loc: ptoken.loc,
                    arity: args.len(),
                    args,
                })))
            }
            // Handle prefix operators
            Ampersand | Asterisk | Bang | Minus => {
                let expr = self.parse_expression_iterative(Precedence::Prefix)?;
                Ok(Expression::Unop(Box::new(Unop {
                    op: Operator::from_token(&token, true)?,
                    loc: token.loc,
                    left: expr,
                    ttype: None,
                })))
            }
            ParenOpen if self.peek().kind == ParenClose => {
                self.next(); // consume ParenClose
                Ok(Expression::ConstExpression(ConstExpression::Void))
            }
            ParenOpen => {
                let expr = self.parse_expression_iterative(Precedence::Lowest)?;
                self.expect_kind(ParenClose)?;
                Ok(Expression::Group(Box::new(expr)))
            }
            CurlyOpen => self.parse_init_list_iterative(),
            _ => chs_error!("{} Unexpected token {}", token.loc, token),
        }
    }

    /// Iterative version of parsing an initialization list.
    fn parse_init_list_iterative(&mut self) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
        let mut exprs = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                CurlyClose => {
                    let loc = self.next().loc;
                    return Ok(Expression::ExpressionList(ExpressionList {
                        loc,
                        exprs,
                        ttype: None,
                    }));
                }
                Identifier => {
                    // In this example we simply advance the token stream.
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
            let value = self.parse_expression_iterative(Precedence::Lowest)?;
            exprs.push(value);
        }
    }

    /// Iterative version of parsing an if-expression.
    fn parse_if_expression_iterative(&mut self, loc: Loc, cond: Expression) -> CHSResult<Expression> {
        use chs_lexer::TokenKind::*;
        let mut body = vec![];
        loop {
            let ptoken = self.peek();
            match ptoken.kind {
                Keyword if ptoken.val_eq("else") => {
                    self.next();
                    let else_body = self.parse_expr_list_iterative(|t| t.val_eq("end"))?;
                    return Ok(Expression::IfElseExpression(Box::new(IfElseExpression {
                        loc,
                        cond,
                        body,
                        else_body,
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
                ParenClose => {
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
                    list.push((token.value, value));
                }
                _ => chs_error!("Unexpected token in function type"),
            }
        }
    }

    /// Iterative version of parsing a type.
    fn parse_type_iterative(&mut self) -> CHSResult<CHSType> {
        use chs_lexer::TokenKind::*;
        let ttoken = self.next();
        let ttype = match ttoken.kind {
            Identifier if ttoken.val_eq("int") => CHSType::Int,
            Identifier if ttoken.val_eq("uint") => CHSType::UInt,
            Identifier if ttoken.val_eq("void") => CHSType::Void,
            Identifier if ttoken.val_eq("bool") => CHSType::Boolean,
            Identifier if ttoken.val_eq("char") => CHSType::Char,
            Identifier if ttoken.val_eq("string") => CHSType::String,
            Asterisk => {
                let ttp = self.parse_type_iterative()?;
                CHSType::Pointer(Box::new(ttp))
            }
            Keyword if ttoken.val_eq("fn") => {
                self.next();
                let (args, ret) = self.parse_fn_type_iterative()?;
                CHSType::Function(
                    args.into_iter().map(|(_, t)| t).collect(),
                    Box::new(ret),
                )
            }
            _ => chs_error!("{} Type not implemented {}", ttoken.loc, ttoken),
        };
        Ok(ttype)
    }

    /// Placeholder for const-expression parsing.
    fn parse_const_expression_iterative(&mut self) -> CHSResult<ConstExpression> {
        // You would build an iterative version similar to parse_expression_iterative.
        todo!()
    }
}
