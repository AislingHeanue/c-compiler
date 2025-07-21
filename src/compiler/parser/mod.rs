use super::lexer::{Token, Type};
use std::{collections::VecDeque, error::Error};

mod display;
mod parse;

pub struct ProgramNode {
    pub function: FunctionNode,
    has_comments: bool,
}

pub struct FunctionNode {
    pub name: String,
    pub body: StatementNode,
}

pub enum StatementNode {
    Return(ExpressionNode),
}

pub enum ExpressionNode {
    Constant(Type),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    Binary(BinaryOperatorNode, Box<ExpressionNode>, Box<ExpressionNode>),
}

pub enum UnaryOperatorNode {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperatorNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

trait Parse
where
    Self: Sized,
{
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>>;
}

pub fn parse(mut lexed: VecDeque<Token>) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(&mut lexed)
}
