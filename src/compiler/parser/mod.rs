use super::lexer::Token;
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

mod display;
mod parse;
mod validate;

pub struct ProgramNode {
    pub function: FunctionNode,
    has_comments: bool,
}

pub struct FunctionNode {
    pub name: String,
    pub body: Block,
}

type Block = Vec<BlockItemNode>;

#[derive(Debug)]
pub enum BlockItemNode {
    Statement(StatementNode),
    Declaration(DeclarationNode),
}

#[derive(Debug)]
pub enum DeclarationNode {
    Declaration(Type, String, Option<ExpressionNode>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Integer,
}

#[derive(Debug)]
pub enum StatementNode {
    Expression(ExpressionNode),
    Pass, // null statement, just a semicolon
    Return(ExpressionNode),
}

#[derive(Clone, Debug)]
pub enum ExpressionNode {
    IntegerConstant(usize),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    Binary(BinaryOperatorNode, Box<ExpressionNode>, Box<ExpressionNode>),
    Var(String),
    Assignment(Box<ExpressionNode>, Box<ExpressionNode>),
}

#[derive(Clone, Debug)]
pub enum UnaryOperatorNode {
    Complement,
    Negate,
    Not,
    PrefixIncrement,
    PrefixDecrement,
    SuffixIncrement,
    SuffixDecrement,
}

#[derive(Debug, Clone)]
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
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>>;
}

struct ParseContext {
    variables: HashMap<String, String>,
    num_variables: usize,
}

pub fn parse(mut lexed: VecDeque<Token>) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(
        &mut lexed,
        &mut ParseContext {
            variables: HashMap::new(),
            num_variables: 0,
        },
    )
}

trait Validate
where
    Self: Sized,
{
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>>;
}

struct ValidateContext {}

pub fn validate(parsed: ProgramNode) -> Result<ProgramNode, Box<dyn Error>> {
    parsed.validate(&mut ValidateContext {})
}
