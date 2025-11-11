#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(String),    // e.g., int, return
    Identifier(String), // e.g., main, IOputs
    ValueLit(Value),    // e.g., 0, "Olá mundo"
    LBrace,             // {
    RBrace,             // }
    LParen,             // (
    RParen,             // )
    Semicolon,          // ;
    BackwardSlash,
    Operator(String), // +, -, *, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Char(char),
    Int(isize),
}

#[derive(Debug, PartialEq)]
pub struct TokenList(pub Vec<Token>);
impl TokenList {
    pub fn new() -> Self {
        TokenList(Vec::new())
    }

    pub fn push(&mut self, token: Token) {
        self.0.push(token);
    }
}

fn get_keywords() -> Vec<&'static str> {
    vec!["int", "char", "return", "module", "string", "float", "if"].into()
}

fn is_op_char(c: char) -> bool {
    // Allowed operator characters (Haskell style):
    !c.is_alphanumeric()
        && !c.is_whitespace()
        && !matches!(c, '_' | '(' | ')' | '{' | '}' | '[' | ']' | '"' | '\'')
}

impl From<String> for TokenList {
    fn from(value: String) -> Self {
        let mut list = TokenList::new();
        let mut chars = value.chars().peekable();

        while let Some(&c) = chars.peek() {
            match c {
                // Skip whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    chars.next();
                }

                // Block delimiters
                '{' => {
                    list.push(Token::LBrace);
                    chars.next();
                }
                '}' => {
                    list.push(Token::RBrace);
                    chars.next();
                }
                '(' => {
                    list.push(Token::LParen);
                    chars.next();
                }
                ')' => {
                    list.push(Token::RParen);
                    chars.next();
                }
                ';' => {
                    list.push(Token::Semicolon);
                    chars.next();
                }
                '\\' => {
                    list.push(Token::BackwardSlash);
                    chars.next();
                }

                // String literal
                '"' => {
                    chars.next(); // skip opening "
                    let mut s = String::new();
                    while let Some(ch) = chars.next() {
                        if ch == '"' {
                            break;
                        }
                        s.push(ch);
                    }
                    list.push(Token::ValueLit(Value::String(s)));
                }

                '\'' => {
                    chars.next();
                    let c = chars
                        .next()
                        .expect("Unexpected None trying to get a char in tokenizer.");
                    chars.next();
                    list.push(Token::ValueLit(Value::Char(c)));
                }

                // Numbers
                '0'..='9' => {
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_numeric() {
                            num.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    match num.parse::<isize>() {
                        Ok(n) => list.push(Token::ValueLit(Value::Int(n))),
                        Err(_) => panic!("Invalid integer literal: {}", num),
                    }
                }

                // Identifiers (allow embedded backtick sections)
                _ if c.is_alphabetic() || c == '_' => {
                    let mut ident = String::new();

                    // consume first char
                    ident.push(c);
                    chars.next();

                    while let Some(&ch) = chars.peek() {
                        match ch {
                            // normal identifier characters
                            _ if ch.is_alphanumeric() || ch == '_' => {
                                ident.push(ch);
                                chars.next();
                            }

                            // start of backtick segment: copy everything until closing `
                            '`' => {
                                ident.push(ch);
                                chars.next(); // consume `

                                // read until the next backtick
                                while let Some(&inside) = chars.peek() {
                                    ident.push(inside);
                                    chars.next();
                                    if inside == '`' {
                                        break;
                                    }
                                }
                            }

                            // anything else ends identifier
                            _ => break,
                        }
                    }

                    // keywords only apply if *no* backticks occur
                    if ident.contains('`') {
                        list.push(Token::Identifier(ident));
                    } else if get_keywords().contains(&ident.as_str()) {
                        list.push(Token::Keyword(ident));
                    } else {
                        list.push(Token::Identifier(ident));
                    }
                }
                // comments
                '/' => {
                    chars.next(); // consume '/'
                    if let Some(&next) = chars.peek() {
                        if next == '/' {
                            // single-line comment
                            chars.next(); // consume second '/'
                            while let Some(ch) = chars.next() {
                                if ch == '\n' {
                                    break;
                                } // stop at newline
                            }
                        } else if next == '*' {
                            // multi-line comment
                            chars.next(); // consume '*'
                            while let Some(ch) = chars.next() {
                                if ch == '*' {
                                    if let Some(&next_ch) = chars.peek() {
                                        if next_ch == '/' {
                                            chars.next(); // consume '/'
                                            break; // exit comment loop
                                        }
                                    }
                                }
                            }
                        } else {
                            // it's actually just the '/' operator
                            list.push(Token::Operator("/".to_string()));
                        }
                    } else {
                        // '/' at end of input
                        list.push(Token::Operator("/".to_string()));
                    }
                }

                // Operators
                _ => {
                    let mut op = String::new();

                    while let Some(&next) = chars.peek() {
                        if is_op_char(next) {
                            op.push(next);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    list.push(Token::Operator(op));
                }
            }
        }

        list
    }
}
