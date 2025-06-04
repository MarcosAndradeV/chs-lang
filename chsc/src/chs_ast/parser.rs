
use chslexer::*;
use thiserror::Error;

use crate::chs_types::CHSType;

use super::ast::{
    Ast, AstNode, BinaryOperator, Expression, FunctionDecl, LetStatement, Param, Params, Precedence, ReturnStatement, Statement
};

#[derive(Debug, Error, Clone)]
pub enum ParseError {
    #[error("Cannot Read file {0}")]
    CannotReadFile(String),

    #[error("{0}: Expected {1}")]
    Expected(Loc, String),

    #[error("Invalid operator token {0}")]
    InvalidOperator(Token)
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: &'a mut PeekableLexer<'a>,
}

pub fn is_keyword(k: &str) -> bool {
    matches!(
        k,
        "fn" | "if"
            | "else"
            | "while"
            | "import"
            | "let"
            | "new"
            | "true"
            | "false"
            | "nil"
            | "return"
    )
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut PeekableLexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> ParseResult<Token> {
        let token = self.lexer.next_token();
        if token.kind != kind {
            return Err(ParseError::Expected(
                token.loc,
                format!("{:?} but got {}", kind, token),
            ));
        }
        Ok(token)
    }

    pub fn peek(&mut self) -> &Token {
        self.lexer.peek_token()
    }

    pub fn next(&mut self) -> Token {
        self.lexer.next_token()
    }

    pub fn current_location(&mut self) -> Loc {
        self.peek().loc
    }

    pub fn parse(&mut self) -> ParseResult<Ast> {
        let mut ast = Ast::new();

        loop {
            if self.peek().is_eof() {
                break;
            }

            let p = self.peek();
            match p.kind {
                TokenKind::Keyword if &p.source == "fn" => {
                    self.next();
                    ast.push(self.parse_fn()?);
                }
                _ => {
                    return Err(ParseError::Expected(
                        p.loc,
                        format!("Declaration but found {}", p),
                    ));
                }
            }
        }

        Ok(ast)
    }

    fn parse_fn(&mut self) -> ParseResult<AstNode> {
        let name = self.expect_token(TokenKind::Identifier)?;

        let parameters = self.parse_parameters()?;

        let return_type = if self.inspect(TokenKind::Arrow)? {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_statment()?;

        Ok(AstNode::FunctionDecl(FunctionDecl {
            name,
            parameters,
            return_type,
            body,
        }))
    }

    fn parse_parameters(&mut self) -> ParseResult<Params> {
        self.expect_token(TokenKind::OpenParen)?;
        let mut parameters = Params::new();
        loop {
            let t = self.next();
            match t.kind {
                TokenKind::CloseParen => break,
                TokenKind::EOF => {
                    return Err(ParseError::Expected(
                        t.loc,
                        "Unexpected EOF in parameters".to_string(),
                    ));
                }
                TokenKind::Identifier => {
                    let name = t;
                    self.expect_token(TokenKind::Colon)?;
                    let type_ = self.parse_type()?;
                    parameters.push(Param { name, type_ });
                }
                _ => {
                    return Err(ParseError::Expected(
                        t.loc,
                        format!("Identifier but got {}", t),
                    ));
                }
            }
        }

        Ok(parameters)
    }

    fn inspect(&mut self, kind: TokenKind) -> ParseResult<bool> {
        let token = self.lexer.peek_token();
        if token.is_eof() {
            return Err(ParseError::Expected(
                token.loc,
                format!("{:?} but got EOF", kind),
            ));
        }
        Ok(token.kind == kind)
    }

    fn parse_type(&mut self) -> ParseResult<CHSType> {
        let t = self.next();
        match t.kind {
            TokenKind::Identifier if &t.source == "int" => Ok(CHSType::I32),
            TokenKind::Identifier if &t.source == "i32" => Ok(CHSType::I32),
            _ => {
                return Err(ParseError::Expected(t.loc, format!("Type but got {}", t)));
            }
        }
    }

    fn parse_statment(&mut self) -> ParseResult<Statement> {
        let p = self.peek();
        match p.kind {
            TokenKind::OpenBrace => self.parse_block_statment(),
            TokenKind::Keyword if &p.source == "let" => self.parse_let_statment(),
            TokenKind::Keyword if &p.source == "return" => self.parse_return_statment(),
            TokenKind::SemiColon => {
                self.next();
                Ok(Statement::Empty)
            }
            _ => self.parse_expresssion_statment(),
        }
    }

    fn parse_expresssion_statment(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expresssion()?;
        self.expect_token(TokenKind::SemiColon)?;
        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expresssion(&mut self) -> ParseResult<Expression> {
        let mut expression = self.parse_primary_expresssion()?;
        while expression.precedence() > Precedence::Lowest {
            let p = self.peek();
            match p.kind {
                TokenKind::Dot => {
                    self.next();
                    let member = self.expect_token(TokenKind::Identifier)?;
                    expression = Expression::MemberAccess {
                        object: Box::new(expression),
                        member,
                    };
                }
                TokenKind::OpenParen => {
                    let args = self.parse_arg_list()?;
                    expression = Expression::FunctionCall {
                        callee: Box::new(expression),
                        args,
                    };
                }
                k if is_binop_default(&k) => {
                    let left = expression.into();
                    let op = self.next();
                    let right = self.parse_expresssion()?.into();
                    expression = Expression::BinaryOp { left, operator: BinaryOperator::try_from(op)?, right } ;
                }
                _ => break,
            }
        }

        Ok(expression)
    }

    fn parse_primary_expresssion(&mut self) -> ParseResult<Expression> {
        let p = self.peek();
        match p.kind {
            TokenKind::Keyword if &p.source == "nil" => {
                self.next();
                Ok(Expression::Nil)
            }
            TokenKind::IntegerNumber => {
                let t = self.next();
                Ok(Expression::Int(t))
            }
            TokenKind::RealNumber => {
                let t = self.next();
                Ok(Expression::Float(t))
            }
            TokenKind::StringLiteral => {
                let t = self.next();
                Ok(Expression::String(t))
            }
            TokenKind::Identifier => {
                let ident = self.next();
                let p = self.peek();
                match p.kind {
                    _ => Ok(Expression::Identifier(ident)),
                }
            }
            _ => {
                return Err(ParseError::Expected(
                    p.loc,
                    format!("Expression but got {}", p),
                ));
            }
        }
    }

    fn parse_return_statment(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Keyword)?;
        let ret = if self.inspect(TokenKind::SemiColon)? {
            self.expect_token(TokenKind::SemiColon)?;
            ReturnStatement::Return
        } else {
            let ret = ReturnStatement::ReturnExpression(self.parse_expresssion()?);
            self.expect_token(TokenKind::SemiColon)?;
            ret
        };
        Ok(Statement::ReturnStatement(ret))
    }

    fn parse_let_statment(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Keyword)?;
        let name = self.expect_token(TokenKind::Identifier)?;
        let type_ = if self.inspect(TokenKind::Colon)? {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };
        let value = if self.inspect(TokenKind::Assign)? {
            self.next();
            Some(self.parse_expresssion()?)
        } else {
            None
        };
        self.expect_token(TokenKind::SemiColon)?;
        Ok(Statement::LetStatement(LetStatement { name, type_, value }))
    }

    fn parse_block_statment(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::OpenBrace)?;
        let mut block = vec![];
        loop {
            let p = self.peek();
            match p.kind {
                TokenKind::CloseBrace => {
                    self.next();
                    break Ok(Statement::BlockStatement(block));
                }
                TokenKind::EOF => {
                    return Err(ParseError::Expected(
                        p.loc,
                        "Unexpected EOF in block".to_string(),
                    ));
                }
                _ => {
                    block.push(self.parse_statment()?);
                }
            }
        }
    }

    fn parse_arg_list(&mut self) -> ParseResult<Vec<Expression>> {
        self.expect_token(TokenKind::OpenParen)?;
        let mut args = vec![];
        loop {
            let p = self.peek();
            match p.kind {
                TokenKind::CloseParen => {
                    self.next();
                    break Ok(args);
                }
                TokenKind::EOF => {
                    return Err(ParseError::Expected(
                        p.loc,
                        "Unexpected EOF in args list".to_string(),
                    ));
                }
                _ => {
                    let expresssion = self.parse_expresssion()?;
                    args.push(expresssion);
                }
            }
        }
    }
}
