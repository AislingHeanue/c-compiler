use super::lexer::Token;
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
    pub out_type: Type,
    pub name: String,
    pub params: Vec<(Type, String)>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub out_type: Type,
    pub name: String,
    pub init: Option<ExpressionNode>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone)]
pub enum StorageInfo {
    // is_defined and global (ie non-static)
    Function(bool, bool),
    // initializer and global
    Static(InitialValue, bool),
    Automatic,
}

#[derive(Debug, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(usize),
    None,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Function(Box<Type>, Vec<Type>),
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
    Expression(ExpressionNode),
}

#[derive(Debug)]
pub enum ForInitialiserNode {
    Declaration(VariableDeclaration),
    Expression(Option<ExpressionNode>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    // function name, args...
    FunctionCall(String, Vec<ExpressionNode>),
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
    // map from string to sting-as-seen-in-assembly and is-externally-linked
    current_scope_identifiers: HashMap<String, (String, bool)>,
    outer_scope_identifiers: HashMap<String, (String, bool)>,
    num_variables: usize,
    do_not_validate: bool,
    // this prevent creating an extra new scope entering function bodies
    current_block_is_function_body: bool,
    current_scope_is_file: bool,
}

pub fn parse(
    mut lexed: VecDeque<Token>,
    do_not_validate: bool,
) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(
        &mut lexed,
        &mut ParseContext {
            current_scope_identifiers: HashMap::new(),
            outer_scope_identifiers: HashMap::new(),
            num_variables: 0,
            do_not_validate,
            current_block_is_function_body: false,
            current_scope_is_file: true,
        },
    )
}

trait Validate
where
    Self: Sized,
{
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>>;
}

#[derive(Clone, Debug)]
enum ValidationPass {
    // variable resolution is covered by parse.rs
    CheckLvalues,
    ReadLabels,
    ValidateLabels,
    LabelLoops,
    ConstantCases,
    TypeChecking,
}

#[derive(Debug)]
struct ValidateContext {
    pass: ValidationPass,
    // Function name -> user-defined name -> label name for birds
    num_labels: usize,
    num_loops: usize,
    num_switches: usize,
    num_switch_labels: usize,
    labels: HashMap<String, HashMap<String, String>>,
    current_function_name: Option<String>,
    // all loops + switch
    current_enclosing_loop_name_for_break: Option<String>,
    // all loops (not switch)
    current_enclosing_loop_name_for_case: Option<String>,
    current_enclosing_loop_name_for_continue: Option<String>,
    current_switch_labels: Option<HashMap<SwitchMapKey, String>>,
    symbols: HashMap<String, SymbolInfo>,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub symbol_type: Type,
    pub storage: StorageInfo,
}

pub fn validate(
    mut parsed: ProgramNode,
) -> Result<(ProgramNode, HashMap<String, SymbolInfo>), Box<dyn Error>> {
    let passes: Vec<ValidationPass> = vec![
        ValidationPass::CheckLvalues,
        ValidationPass::ReadLabels,
        ValidationPass::ValidateLabels,
        ValidationPass::LabelLoops,
        ValidationPass::ConstantCases,
        ValidationPass::TypeChecking,
    ];
    let mut validate_context = ValidateContext {
        pass: passes.first().unwrap().clone(),
        num_labels: 0,
        num_loops: 0,
        num_switches: 0,
        num_switch_labels: 0,
        labels: HashMap::new(),
        current_function_name: None,
        current_enclosing_loop_name_for_break: None,
        current_enclosing_loop_name_for_case: None,
        current_enclosing_loop_name_for_continue: None,
        current_switch_labels: None,
        symbols: HashMap::new(),
    };
    for pass in passes {
        validate_context.pass = pass;
        parsed = parsed.validate(&mut validate_context)?;
    }

    Ok((parsed, validate_context.symbols))
}

trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Clone)]
pub struct DisplayContext {
    indent: usize,
}

impl DisplayContext {
    fn indent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent += 4;
        s
    }
    fn unindent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent -= 4;
        s
    }
    fn new_line_start(&self) -> String {
        format!("\n{:indent$}", "", indent = self.indent)
    }
}
