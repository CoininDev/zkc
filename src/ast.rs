use std::sync::Arc;

use crate::tokenizer::{Token, TokenList, Value, get_types};

#[derive(Debug)]
pub struct Program {
    pub zewages: Vec<Zewage>,
    pub functions: Vec<Expr>,
}

impl From<TokenList> for Program {
    fn from(value: TokenList) -> Self {
        let mut parser = Parser::new(value.0);
        let mut functions = vec![];
        let mut zewages = vec![];

        while !parser.is_done() {
            if parser.peek() == Some(&Token::Keyword("zewage".into())) {
                zewages.push(parser.parse_zewage());
            }
            let expr = parser.parse_expr();

            functions.push(expr);
        }

        Program { zewages, functions }
    }
}

#[derive(Debug)]
pub struct Zewage {
    pub name: String,
    pub functions: Vec<Expr>,
}
#[derive(Clone, Debug)]
pub enum Type {
    Int,
    String,
    Char,
}

#[derive(Debug)]
pub struct Expr {
    expr_type: Type,
    expr_data: ExprData,
}

#[derive(Debug)]
pub enum ExprData {
    Assign {
        identifier: Arc<Expr>,
        expr: Vec<Expr>,
    },
    Variable {
        name: String,
    },
    // operators are functions too: 7 + 4
    Function {
        name: String,          // operator`+`
        parameters: Vec<Expr>, // [7, 4]
        pure: bool,            // true
        zimports: Vec<String>,
    },
    Return {
        expr: Arc<Expr>,
    },
    Literal {
        value: Value,
    },
    EndOfFile,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
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
        self.pos >= self.tokens.len()
    }
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self, expected: &Token) {
        if self.peek() != Some(expected) {
            panic!("Expected {:?} but found {:?}", expected, self.peek());
        }
        self.next();
    }
}

impl Parser {
    fn parse_variable(&mut self) -> Expr {
        if let Some(Token::Identifier(name)) = self.next() {
            Expr {
                expr_type: Type::Int, //TODO: type inference
                expr_data: ExprData::Variable { name },
            }
        } else {
            panic!("Expected identifier. pos={:?}", self.peek());
        }
    }

    fn parse_literal(&mut self) -> Expr {
        if let Some(Token::ValueLit(value)) = self.next() {
            let expr_type = match &value {
                Value::Int(_) => Type::Int,
                Value::String(_) => Type::String,
                Value::Char(_) => Type::Char,
            };

            Expr {
                expr_type,
                expr_data: ExprData::Literal { value },
            }
        } else {
            panic!("Expected literal. pos={:?}", self.peek());
        }
    }

    //function_calling
    fn parse_function(&mut self) -> Expr {
        let name = match self.next() {
            Some(Token::Identifier(s)) => s,
            _ => panic!("Expected function name. pos={:?}", self.peek()),
        };

        match self.peek() {
            Some(Token::LParen) => self.next(),
            _ => {
                return Expr {
                    expr_type: Type::Int, //TODO: inference
                    expr_data: ExprData::Variable { name },
                };
            }
        };

        let mut args = vec![];
        while self.peek() != Some(&Token::RParen) {
            args.push(self.parse_expr());
        }

        Expr {
            expr_type: Type::Int, //TODO: inference
            expr_data: ExprData::Function {
                name,
                parameters: args,
                pure: true, //TODO: inference
                zimports: vec![],
            },
        }
    }
    //function/variable assignment
    fn parse_assign(&mut self) -> Expr {
        // 1. type or let
        let explicit_type = match self.peek() {
            Some(Token::Keyword(kw)) if kw == "let" => None,
            Some(Token::Keyword(kw)) => {
                Some(match kw.as_str() {
                    "int" => Type::Int,
                    "string" => Type::String,
                    "char" => Type::Char,
                    "void" => Type::Int, // treat void as Int-Unit for now
                    _ => panic!("Unknown type keyword {}", kw),
                })
            }
            _ => panic!(
                "Expected assignment start (let or type) but found {:?}",
                self.peek()
            ),
        };
        self.next(); //type 

        // 2. identifier
        let name = match self.next() {
            Some(Token::Identifier(n)) => n,
            _ => panic!("Expected identifier"),
        };

        // 3. optional parameter list
        let mut params = vec![];
        if self.peek() == Some(&Token::LParen) {
            self.next(); // consume '('
            while self.peek() != Some(&Token::RParen) {
                params.push(self.parse_param());
            }
            //self.next();
            //self.next();
            self.consume(&Token::RParen);
        }

        let zimports = self.parse_zimports();

        // 5. either '=' <expr> ';'   OR   '{' <expr>* '}'
        match self.peek() {
            Some(Token::Operator(op)) if op == "=" => {
                self.next(); // consume '='
                let expr = self.parse_expr();
                self.consume(&Token::Semicolon);
                let my_type = explicit_type.clone().unwrap_or(expr.expr_type.clone());

                if params.is_empty() && zimports.is_empty() {
                    return Expr {
                        expr_type: my_type.clone(),
                        expr_data: ExprData::Assign {
                            identifier: Arc::new(Expr {
                                expr_type: my_type,
                                expr_data: ExprData::Variable { name },
                            }),
                            expr: vec![expr],
                        },
                    };
                }

                Expr {
                    expr_type: explicit_type.clone().unwrap_or(expr.expr_type.clone()),
                    expr_data: ExprData::Assign {
                        identifier: Arc::new(Expr {
                            expr_type: explicit_type.clone().unwrap_or(expr.expr_type.clone()),
                            expr_data: ExprData::Function {
                                name,
                                parameters: params,
                                pure: zimports.is_empty(), // || !impure,
                                zimports,
                            },
                        }),
                        expr: vec![expr],
                    },
                }
            }

            Some(Token::LBrace) => {
                self.next(); // consume '{'
                let mut body = vec![];
                while self.peek() != Some(&Token::RBrace) {
                    body.push(self.parse_expr());
                }
                self.consume(&Token::RBrace);
                let my_type = explicit_type.clone().unwrap_or(match body.last() {
                    Some(k) => k.expr_type.clone(),
                    None => Type::Int,
                });

                Expr {
                    expr_type: my_type.clone(),
                    expr_data: ExprData::Assign {
                        identifier: Arc::new(Expr {
                            expr_type: my_type,
                            expr_data: ExprData::Function {
                                name,
                                parameters: params,
                                pure: zimports.is_empty(), // || !impure,
                                zimports,
                            },
                        }),
                        expr: body,
                    },
                }
            }

            _ => panic!(
                "Expected '=' or '{{' in function assignment, pos={:?}",
                self.peek()
            ),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        dbg!(&self.peek());
        match self.peek() {
            Some(Token::Keyword(k)) if k == "let" => self.parse_assign(),
            Some(Token::Keyword(k)) if get_types().contains(&k.as_str()) => self.parse_assign(),
            Some(Token::Keyword(k)) if k == "return" => self.parse_return(),
            Some(Token::ValueLit(_)) => self.parse_literal(),
            Some(Token::Identifier(_)) => self.parse_function(),
            None => Expr {
                expr_type: Type::Int,
                expr_data: ExprData::EndOfFile,
            },
            _ => panic!("Unexpected token {:?}, pos={}", self.peek(), self.pos),
        }
    }

    fn parse_param(&mut self) -> Expr {
        let explicit_type = match self.peek() {
            Some(Token::Keyword(kw)) if kw == "let" => None,
            Some(Token::Keyword(kw)) => Some(match kw.as_str() {
                "int" => Type::Int,
                "string" => Type::String,
                "char" => Type::Char,
                "void" => Type::Int,
                _ => panic!("Unknown type keyword {}", kw),
            }),
            _ => panic!(
                "Expected assignment start (let or type) but found {:?}",
                self.peek()
            ),
        };
        self.next();

        let name = match self.next() {
            Some(Token::Identifier(n)) => n,
            _ => panic!("Expected identifier"),
        };

        let my_type = explicit_type.unwrap_or(Type::Int);

        Expr {
            expr_type: my_type,
            expr_data: ExprData::Variable { name },
        }
    }

    fn parse_return(&mut self) -> Expr {
        self.consume(&Token::Keyword("return".into()));

        let expr = self.parse_expr();

        self.consume(&Token::Semicolon);

        Expr {
            expr_type: expr.expr_type.clone(),
            expr_data: ExprData::Return {
                expr: Arc::new(expr),
            },
        }
    }

    fn parse_zimports(&mut self) -> Vec<String> {
        let mut zimports = vec![];

        loop {
            match self.peek() {
                Some(Token::Operator(op)) if op == "+" => {
                    self.next(); //consume operator
                    let zimport = match self.next() {
                        // consume name
                        Some(Token::Identifier(p)) => p,
                        _ => panic!("Expected zimport name after +. {:?}", self.peek()),
                    };
                    //self.next();
                    zimports.push(zimport);
                }

                _ => break,
            }
        }

        zimports
    }

    fn parse_zewage(&mut self) -> Zewage {
        let name = match self.next() {
            Some(Token::Keyword(k)) if k == "zewage" => self.next(),
            _ => panic!("Expected zewage. pos={:?}", self.peek()),
        };
        let name = match name {
            Some(Token::Identifier(s)) => s,
            _ => panic!("Expected zewage name. pos={:?}", self.peek()),
        };

        dbg!(&name);

        let mut functions = vec![];
        match self.peek() {
            Some(Token::LBrace) => {
                self.next();
                while self.peek() != Some(&Token::RBrace) {
                    let expr = self.parse_expr();
                    functions.push(expr);
                }
                self.next();
            }
            Some(Token::Semicolon) => {
                self.next();
                while !self.is_done() {
                    let expr = self.parse_expr();
                    functions.push(expr);
                }
            }
            _ => panic!("Expected body after zewage or ';'"),
        }

        Zewage { name, functions }
    }
}
