use parse::do_parse;
use validate::do_validate;

use super::types::{Constant, OrdinalStatic, StorageClass, StorageInfo, Type};

use super::{lexer::Token, types::SymbolInfo};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

mod display;
mod parse;
mod validate;

pub struct ProgramNode {
    pub declarations: Vec<DeclarationNode>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub function_type: Type,
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionNode(pub ExpressionWithoutType, pub Option<Type>);

#[derive(Debug)]
pub struct VariableDeclaration {
    pub variable_type: Type,
    pub name: String,
    pub init: Option<InitialiserNode>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub struct InitialiserNode(pub InitialiserWithoutType, pub Option<Type>);

#[derive(Debug, Clone)]
pub enum InitialiserWithoutType {
    Single(ExpressionNode),
    Compound(Vec<InitialiserNode>),
}

pub type Block = Vec<BlockItemNode>;

#[derive(Debug)]
pub enum BlockItemNode {
    Statement(StatementNode),
    Declaration(DeclarationNode),
}

#[derive(Debug)]
pub enum DeclarationNode {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Debug)]
pub enum StatementNode {
    // labels are tied to a statement, and NOT a declaration
    // from C23 onwards, they may also be tied to a declaration
    Label(String, Box<StatementNode>),
    Goto(String),
    Expression(ExpressionNode),
    Compound(Block),
    // condition, then, else
    If(
        ExpressionNode,
        Box<StatementNode>,
        Box<Option<StatementNode>>,
    ),
    Pass, // null statement, just a semicolon
    Return(ExpressionNode),
    // name of enclosing loop (controls where this jumps to)
    Break(Option<String>),
    // name of enclosing loop (controls where this jumps to)
    Continue(Option<String>),
    // third field is the auto-generated name of the label for this loop,
    // which we create during the validation step.
    While(ExpressionNode, Box<StatementNode>, Option<String>),
    // third field is the auto-generated name of the label for this loop,
    // which we create during the validation step.
    DoWhile(Box<StatementNode>, ExpressionNode, Option<String>),
    // fifth field is the auto-generated name of the label for this loop,
    // which we create during the validation step.
    For(
        ForInitialiserNode,
        Option<ExpressionNode>,
        Option<ExpressionNode>,
        Box<StatementNode>,
        Option<String>,
    ),
    Switch(
        ExpressionNode,
        Box<StatementNode>,
        Option<String>,
        Option<HashMap<SwitchMapKey, String>>,
    ),
    Case(ExpressionNode, Box<StatementNode>, Option<String>),
    Default(Box<StatementNode>, Option<String>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SwitchMapKey {
    Default,
    Constant(OrdinalStatic),
}

#[derive(Debug)]
pub enum ForInitialiserNode {
    Declaration(VariableDeclaration),
    Expression(Option<ExpressionNode>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionWithoutType {
    Constant(Constant),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    // eg a + 5
    Binary(BinaryOperatorNode, Box<ExpressionNode>, Box<ExpressionNode>),
    // eg a += 5
    // op, left, right
    Compound(BinaryOperatorNode, Box<ExpressionNode>, Box<ExpressionNode>),
    // eg a[b]
    Subscript(Box<ExpressionNode>, Box<ExpressionNode>),
    Var(String),
    Assignment(Box<ExpressionNode>, Box<ExpressionNode>),
    // condition ? then : otherwise
    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
    ),
    // function name, args...
    FunctionCall(String, Vec<ExpressionNode>),
    // target type to cast expression to
    Cast(Type, Box<ExpressionNode>),
    Dereference(Box<ExpressionNode>),
    AddressOf(Box<ExpressionNode>),
    String(Vec<i8>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperatorNode {
    Complement,
    Negate,
    Not,
    PrefixIncrement,
    PrefixDecrement,
    SuffixIncrement,
    SuffixDecrement,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug)]
pub enum Declarator {
    Name(String),
    Pointer(Box<Declarator>),
    // params info and any further declarator to apply to the parent type
    Function(Box<Declarator>, Vec<(Type, Declarator)>),
    // containing type and size
    Array(Box<Declarator>, u64),
}

#[derive(Debug)]
pub enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, u64),
    Base,
}

pub fn parse(lexed: VecDeque<Token>, do_not_validate: bool) -> Result<ProgramNode, Box<dyn Error>> {
    do_parse(lexed, do_not_validate)
}

pub fn validate(parsed: &mut ProgramNode) -> Result<HashMap<String, SymbolInfo>, Box<dyn Error>> {
    do_validate(parsed)
}
