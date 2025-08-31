use parse::do_parse;
use validate::do_validate;

use super::types::{ComparableStatic, Constant, EnumMember, MemberEntry, StorageClass, Type};

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
    pub _inline: bool,
    pub output_const: bool,
    pub output_volatile: bool,
    pub params: Vec<String>,
    pub body: Option<Vec<BlockItemNode>>,
    pub storage_class: Option<StorageClass>,
    pub inline_declarations: Vec<InlineDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
// third field marks that the expression is constant
pub struct ExpressionNode(pub ExpressionWithoutType, pub Option<Type>, pub bool);

#[derive(Debug)]
pub struct VariableDeclaration {
    pub variable_type: Type,
    pub name: String,
    pub constant: bool,
    pub volatile: bool,
    pub init: Option<InitialiserNode>,
    pub storage_class: Option<StorageClass>,
    // variable declarations may also include an inline struct declaration, make sure to include
    // them here
    pub inline_declarations: Vec<InlineDeclaration>,
}

#[derive(Debug)]
pub struct TypeDeclaration {
    pub target_type: Type,
    pub name: String,
    pub inline_declarations: Vec<InlineDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InlineDeclaration {
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: String,
    pub members: Option<Vec<StructMember>>,
    pub is_union: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDeclaration {
    pub name: String,
    pub members: Vec<EnumMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    pub member_type: Type,
    pub name: Option<String>,
    pub inline_declarations: Vec<InlineDeclaration>,
    pub _num_bits: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct InitialiserNode(pub InitialiserWithoutType, pub Option<Type>);

#[derive(Debug, Clone)]
pub enum InitialiserWithoutType {
    Single(ExpressionNode),
    Compound(Vec<InitialiserNode>),
}

#[derive(Debug)]
pub enum BlockItemNode {
    Statement(StatementNode),
    Declaration(DeclarationNode),
}

#[derive(Debug)]
pub enum DeclarationNode {
    Type(TypeDeclaration),
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
}

#[derive(Debug)]
pub enum StatementNode {
    // labels are tied to a statement, and NOT a declaration
    // from C23 onwards, they may also be tied to a declaration
    Label(String, Box<StatementNode>),
    Goto(String),
    Expression(ExpressionNode),
    Compound(Vec<BlockItemNode>),
    // condition, then, else
    If(
        ExpressionNode,
        Box<StatementNode>,
        Box<Option<StatementNode>>,
    ),
    Pass, // null statement, just a semicolon
    Return(Option<ExpressionNode>),
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
    Constant(ComparableStatic),
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
    Compound(
        BinaryOperatorNode,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Option<Type>,
    ),
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
    IndirectFunctionCall(Box<ExpressionNode>, Vec<ExpressionNode>),
    BuiltinFunctionCall(BuiltinVa),
    // target type to cast expression to
    Cast(Type, Box<ExpressionNode>, Vec<InlineDeclaration>),
    Dereference(Box<ExpressionNode>),
    AddressOf(Box<ExpressionNode>),
    String(Vec<i8>),
    SizeOf(Box<ExpressionNode>),
    SizeOfType(Type, Vec<InlineDeclaration>),
    // operates only on structs
    Dot(Box<ExpressionNode>, String),
    // operates only on pointers (to structs)
    Arrow(Box<ExpressionNode>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinVa {
    Start(Box<ExpressionNode>, Box<ExpressionNode>),
    Arg(Box<ExpressionNode>, Type, Vec<InlineDeclaration>),
    End(Box<ExpressionNode>),
    Copy(Box<ExpressionNode>, Box<ExpressionNode>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperatorNode {
    Complement,
    Negate,
    Identity,
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

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub alignment: u64,
    pub size: u64,
    pub members: Vec<MemberEntry>,
    // pub is_union: bool,
}

#[derive(Debug, Clone)]
pub enum Declarator {
    Name(String),
    Base,
    // bool denotes whether the pointer declaration is marked as 'restrict'
    // See https://en.wikipedia.org/wiki/Restrict
    Pointer(Box<Declarator>, bool),
    ConstPointer(Box<Declarator>),
    // params info and any further declarator to apply to the parent type, is_variadic
    Function(Box<Declarator>, Vec<(Type, Declarator)>, bool),
    // containing type and size
    Array(Box<Declarator>, Option<ExpressionNode>),
}

// declarator, struct_declarations
pub type DeclaratorWithInline = (Declarator, Vec<InlineDeclaration>);

pub fn parse(lexed: VecDeque<Token>, do_not_validate: bool) -> Result<ProgramNode, Box<dyn Error>> {
    do_parse(lexed, do_not_validate)
}

type ValidateResult = (HashMap<String, SymbolInfo>, HashMap<String, StructInfo>);

pub fn validate(parsed: &mut ProgramNode) -> Result<ValidateResult, Box<dyn Error>> {
    do_validate(parsed)
}
