use std::sync::Arc;

use crate::tokenizer::{Operator, Token, TokenList, Value};

#[derive(Debug)]
pub struct Program {
    code: Vec<Expr>,
}

impl From<TokenList> for Program {
    fn from(value: TokenList) -> Self {
        let mut code = vec![];
        let mut parser = Parser::new(value.0);
        while !parser.is_done() {
            dbg!(parser.pos);
            let x = parser.parse_expr();
            code.push(x);
        }

        Program { code }
    }
}

#[derive(Debug)]
pub enum Expr {
    Lit(LiteralValue),
    Return(Arc<Expr>),
    Assign(Arc<Expr>, Vec<Expr>),
    Variable(String),
    Lambda(Arc<Expr>, Vec<Expr>),
    FunctionCall(Arc<Expr>, Vec<Expr>),
    Block(Vec<Expr>),
}

#[derive(Debug)]
pub enum LiteralValue {
    String(String),
    Number(isize),
    Char(char),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn parse_expr(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Keyword(s)) if s == "return" => self.parse_return(),
            Some(Token::ValueLit(_)) => match self.peek_ahead(1) {
                Some(Token::Operator(_)) => self.parse_operation(),
                _ => self.parse_literal(),
            },
            Some(Token::Identifier(_)) => match self.peek_ahead(1) {
                Some(Token::LParen) => self.parse_assign(),
                Some(Token::Operator(x)) if x == &Operator::Assign => self.parse_assign(),
                Some(Token::Operator(_)) => self.parse_operation(),
                _ => self.parse_variable(),
            },
            Some(Token::BackwardSlash) => self.parse_lambda(),
            _ => panic!("parse_expr got to the end with {:?}", self.peek()),
        }
    }

    pub fn parse_literal(&mut self) -> Expr {
        let lit = match self.next() {
            Some(Token::ValueLit(a)) => a,
            _ => panic!("expected Value literal, found {:?}", self.peek()),
        };

        match lit {
            Value::Int(i) => Expr::Lit(LiteralValue::Number(i)),
            Value::String(i) => Expr::Lit(LiteralValue::String(i)),
            Value::Char(i) => Expr::Lit(LiteralValue::Char(i)),
        }
    }

    pub fn parse_operation(&mut self) -> Expr {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> Expr {
        let mut left = self.parse_primary();

        loop {
            let op = match self.peek() {
                Some(Token::Operator(op)) => op.clone(),
                _ => break,
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            // consome o operador
            self.next();

            // lê o operando da direita
            let mut right = self.parse_primary();

            // se o próximo operador tem precedência maior, trata ele primeiro
            while let Some(Token::Operator(next_op)) = self.peek() {
                if next_op.precedence() > prec {
                    right = self.parse_binary_expr(next_op.precedence());
                } else {
                    break;
                }
            }

            left = Expr::FunctionCall(Arc::new(Expr::Variable(op.to_string())), vec![left, right]);
        }

        left
    }

    fn parse_primary(&mut self) -> Expr {
        match self.next() {
            Some(Token::ValueLit(Value::Int(n))) => Expr::Lit(LiteralValue::Number(n)),
            Some(Token::Identifier(id)) => Expr::Variable(id),
            Some(Token::LParen) => {
                let expr = self.parse_expr();
                self.expect(&Token::RParen, "parse_primary");
                expr
            }
            other => panic!("esperado número ou parênteses, encontrado {:?}", other),
        }
    }

    pub fn parse_return(&mut self) -> Expr {
        self.expect(&Token::Keyword("return".into()), "parse_return");

        let expr = self.parse_expr();

        Expr::Return(Arc::new(expr))
    }

    pub fn parse_assign(&mut self) -> Expr {
        let id = self.parse_variable();

        let mut params = vec![];
        if self.peek() == Some(&Token::LParen) {
            self.next();
            while self.peek() != Some(&Token::RParen) {
                match self.next() {
                    Some(Token::Identifier(s)) => params.push(Expr::Variable(s)),
                    _ => panic!("Expected id found {:?}", self.peek()),
                };
                self.resolve(&Token::Comma);
            }
        }

        let mut code = vec![];
        match self.peek() {
            Some(Token::LBrace) => {
                self.next();
                while self.peek() != Some(&Token::RBrace) {
                    let expr = self.parse_expr();
                    code.push(expr);
                    self.expect(&Token::Semicolon, "parse_assign resolving code");
                }
            }
            Some(Token::Operator(Operator::Assign)) => {
                self.next();
                let expr = self.parse_expr();
                code.push(expr);
                self.expect(&Token::Semicolon, "parse_assign resolving code");
            }
            _ => panic!("Expected block ('=' or '{{') but found {:?}", self.peek()),
        }

        let code = if params.is_empty() {
            code
        } else {
            let mut result = code;
            for param in params.into_iter().rev() {
                result = vec![Expr::Lambda(Arc::new(param), result)];
            }
            result
        };

        Expr::Assign(Arc::new(id), code)
    }

    pub fn parse_variable(&mut self) -> Expr {
        match self.next() {
            Some(Token::Identifier(i)) => Expr::Variable(i),
            _ => panic!("Expected a Variable identifier, found {:?}", self.peek()),
        }
    }

    // just for already defined lambdas
    // \x {x + 1}
    // functions will be resolved in the parse_assign
    pub fn parse_lambda(&mut self) -> Expr {
        todo!()
    }

    // f(x, y)
    pub fn parse_function_call(&mut self) -> Expr {
        todo!()
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.pos).cloned();
        self.pos += 1;
        token
    }
    fn is_done(&self) -> bool {
        dbg!(self.pos);
        self.pos >= self.tokens.len()
    }
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
    }

    fn expect(&mut self, expected: &Token, error_message: &str) {
        if self.peek() != Some(expected) {
            panic!(
                "Expected {:?} but found {:?}.   ({})",
                expected,
                self.peek(),
                error_message
            );
        }
        self.next();
    }

    fn resolve(&mut self, expected: &Token) {
        if self.peek() == Some(expected) {
            self.next();
        }
    }
}
