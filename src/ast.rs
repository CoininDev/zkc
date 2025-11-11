use crate::tokenizer::{Token, TokenList};

#[derive(Debug, Clone)]
pub enum Type {
    String,
    Int,
    Char,
}

#[derive(Debug)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    modules: Vec<ModuleImports>,
    return_type: Type,
    body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Parameter {
    name: String,
    param_type: Type,
}

#[derive(Debug)]
pub enum Statement {
    ExprStmt(Expr),
    Assign(String, Expr),
    Return(Expr),
    If {
        condition: Expr,
        block: Vec<Statement>,
        else_block: Vec<Statement>,
    },
}

#[derive(Debug)]
pub enum Expr {
    Number(isize),
    String(String),
    Char(char),
    Variable(String),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug)]
pub struct ModuleImports(String);

#[derive(Debug)]
pub enum Program {
    Basic { functions: Vec<Function> },
    Module { functions: Vec<Function> },
}

impl From<TokenList> for Program {
    fn from(value: TokenList) -> Self {}
}
