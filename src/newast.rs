use std::sync::Arc;

use crate::tokenizer::{Token, TokenList};

#[derive(Debug)]
pub struct Program {
    code: Vec<Expr>,
}

impl From<TokenList> for Program {
    fn from(value: TokenList) -> Self {
        let mut code = vec![];
        let mut parser = Parser::new(value.0);
        while !parser.is_done() {
            let x = parser.parse_expr();
            code.push(x);
        }

        Program { code }
    }
}

#[derive(Debug)]
pub enum Expr {
    Return(Arc<Expr>),
    Assign(Arc<Expr>, Vec<Expr>),
    Variable(String),
    Lambda(Arc<Expr>, Vec<Expr>),
    FunctionCall(Arc<Expr>, Vec<Expr>),
    Block(Vec<Expr>),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn parse_expr(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Keyword(s)) if s == "return" => self.parse_return(),
            Some(Token::Identifier(_)) => match self.peek_ahead(1) {
                Some(Token::LParen) => self.parse_assign(),
                Some(Token::Operator(x)) if x == "=" => self.parse_assign(),
                Some(Token::Operator(_)) => self.parse_operation(),
                _ => self.parse_variable(),
            },
            Some(Token::Operator(op)) if op == "\\" => self.parse_lambda(),
            _ => panic!("parse_expr got to the end with {:?}", self.peek()),
        }
    }

    pub fn parse_operation(&mut self) -> Expr {
        // 5 + 4 => (+) (5, 4)
        todo!()
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
            Some(Token::Operator(x)) if x == "=" => {
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
        let v = match self.peek() {
            Some(Token::Identifier(i)) => i,
            _ => panic!("Expected a Variable identifier, found {:?}", self.peek()),
        };

        Expr::Variable(v.clone())
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
