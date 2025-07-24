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

pub type Block = Vec<BlockItemNode>;

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
}

#[derive(Debug)]
pub enum ForInitialiserNode {
    Declaration(DeclarationNode),
    Expression(Option<ExpressionNode>),
}

#[derive(Clone, Debug)]
pub enum ExpressionNode {
    IntegerConstant(usize),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    Binary(BinaryOperatorNode, Box<ExpressionNode>, Box<ExpressionNode>),
    Var(String),
    Assignment(Box<ExpressionNode>, Box<ExpressionNode>),
    // condition ? then : otherwise
    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
    ),
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
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
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
    current_scope_variables: HashMap<String, String>,
    outer_scope_variables: HashMap<String, String>,
    num_variables: usize,
    do_not_validate: bool,
    // labels on declarations are only allowed in C23+
}

pub fn parse(
    mut lexed: VecDeque<Token>,
    do_not_validate: bool,
) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(
        &mut lexed,
        &mut ParseContext {
            current_scope_variables: HashMap::new(),
            outer_scope_variables: HashMap::new(),
            num_variables: 0,
            do_not_validate,
        },
    )
}

trait Validate
where
    Self: Sized,
{
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>>;
}

#[derive(Clone)]
enum ValidationPass {
    // variable resolution is covered by parse.rs
    CheckLvalues,
    ReadLabels,
    ValidateLabels,
}

struct ValidateContext {
    pass: ValidationPass,
    // Function name -> user-defined name -> label name for birds
    labels: HashMap<String, HashMap<String, String>>,
    num_labels: usize,
    function_name: Option<String>,
}

pub fn validate(mut parsed: ProgramNode) -> Result<ProgramNode, Box<dyn Error>> {
    let passes: Vec<ValidationPass> = vec![
        ValidationPass::CheckLvalues,
        ValidationPass::ReadLabels,
        ValidationPass::ValidateLabels,
    ];
    let mut validate_context = ValidateContext {
        pass: passes.first().unwrap().clone(),
        labels: HashMap::new(),
        function_name: None,
        num_labels: 0,
    };
    for pass in passes {
        validate_context.pass = pass;
        parsed = parsed.validate(&mut validate_context)?;
    }

    Ok(parsed)
}
