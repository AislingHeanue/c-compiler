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
    pub function_type: Type,
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionNode(pub ExpressionWithoutType, pub Option<Type>);

#[derive(Debug)]
pub struct VariableDeclaration {
    pub variable_type: Type,
    pub name: String,
    pub init: Option<ExpressionNode>,
    pub storage_class: Option<StorageClass>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Long,
    UnsignedInteger,
    UnsignedLong,
    // return type, param types
    Function(Box<Type>, Vec<Type>),
}

impl Type {
    fn get_size(&self) -> i32 {
        match self {
            Type::Integer => 32,
            Type::Long => 64,
            Type::UnsignedInteger => 32,
            Type::UnsignedLong => 64,
            Type::Function(_, _) => unreachable!(),
        }
    }

    fn is_signed(&self) -> bool {
        match self {
            Type::Integer => true,
            Type::Long => true,
            Type::UnsignedInteger => false,
            Type::UnsignedLong => false,
            Type::Function(_, _) => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
}

#[derive(Debug, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInitial),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StaticInitial {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
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
    Constant(StaticInitial),
}

#[derive(Debug)]
pub enum ForInitialiserNode {
    Declaration(VariableDeclaration),
    Expression(Option<ExpressionNode>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExpressionWithoutType {
    Constant(Constant),
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
    // target type to cast expression to
    Cast(Type, Box<ExpressionNode>),
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
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

#[derive(Clone, Debug)]
enum ValidationPass {
    // variable resolution is covered by parse.rs
    CheckLvalues,
    ReadLabels,
    ValidateLabels,
    LabelLoops,
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
    current_switch_type: Option<Type>,
    symbols: HashMap<String, SymbolInfo>,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub symbol_type: Type,
    pub storage: StorageInfo,
}

pub fn validate(parsed: &mut ProgramNode) -> Result<HashMap<String, SymbolInfo>, Box<dyn Error>> {
    let passes: Vec<ValidationPass> = vec![
        ValidationPass::CheckLvalues,
        ValidationPass::ReadLabels,
        ValidationPass::ValidateLabels,
        ValidationPass::LabelLoops,
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
        current_switch_type: None,
        symbols: HashMap::new(),
    };
    for pass in passes {
        validate_context.pass = pass;
        parsed.validate(&mut validate_context)?;
    }

    Ok(validate_context.symbols)
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
