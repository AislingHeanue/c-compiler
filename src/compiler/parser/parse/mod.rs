use super::{
    BinaryOperatorNode, BlockItemNode, DeclarationNode, Declarator, ExpressionNode,
    ExpressionWithoutType, ForInitialiserNode, InitialiserNode, InitialiserWithoutType,
    ProgramNode, StatementNode, StructMember, Type, UnaryOperatorNode,
};
use crate::compiler::{
    lexer::{Token, TokenVector},
    types::{Constant, EnumMember},
};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

mod block_item_node;
mod constant;
mod declaration_node;
mod declarator;
mod expression_node;
mod parsed_types;
mod statement_node;

trait Parse<T>
where
    Self: Sized,
{
    fn parse(&mut self, context: &mut ParseContext) -> Result<T, Box<dyn Error>>;
}

pub struct ParseContext {
    // map from string to sting-as-seen-in-assembly and is-externally-linked
    current_scope_identifiers: HashMap<String, Identity>,
    outer_scope_identifiers: HashMap<String, Identity>,
    // map from struct name to unique name and whether it's a union
    current_struct_names: HashMap<String, (String, StructKind)>,
    outer_struct_names: HashMap<String, (String, StructKind)>,
    enums: HashMap<String, Vec<EnumMember>>,
    last_enum_number: i32,
    // all_struct_types: HashMap<String, StructTypeEntry>,
    num_variables: usize,
    num_structs: usize,
    do_not_validate: bool,
    // this prevent creating an extra new scope entering function bodies
    current_block_is_function_body: bool,
    current_scope_is_file: bool,
    parsing_param: bool,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum StructKind {
    Struct,
    Union,
    Enum,
}

#[derive(Clone, Debug)]
pub enum Identity {
    // internal name, is_linked
    Variable(String, bool),
    TypeAlias(Type),
}

pub fn do_parse(
    mut lexed: VecDeque<Token>,
    do_not_validate: bool,
) -> Result<ProgramNode, Box<dyn Error>> {
    lexed.parse(&mut ParseContext {
        current_scope_identifiers: HashMap::new(),
        outer_scope_identifiers: HashMap::new(),
        current_struct_names: HashMap::new(),
        outer_struct_names: HashMap::new(),
        enums: HashMap::new(),
        last_enum_number: -1,
        // all_struct_types: HashMap::new(),
        num_variables: 0,
        num_structs: 0,
        // num_anonymous_params: 0,
        do_not_validate,
        current_block_is_function_body: false,
        current_scope_is_file: true,
        parsing_param: false,
    })
}

impl Parse<ProgramNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<ProgramNode, Box<dyn Error>> {
        let mut declarations: Vec<DeclarationNode> = Vec::new();
        while !self.is_empty() {
            declarations.append(&mut self.parse(context)?)
        }

        Ok(ProgramNode { declarations })
    }
}

impl Parse<ForInitialiserNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<ForInitialiserNode, Box<dyn Error>> {
        if self.peek()?.is_start_of_declaration(context) {
            let block_item = self.parse(context)?;
            if let BlockItemNode::Declaration(ds) = block_item {
                let mut vs = Vec::new();
                for d in ds.into_iter() {
                    match d {
                        DeclarationNode::Variable(v) => {
                            if !context.do_not_validate && v.storage_class.is_some() {
                                return Err(
                                    "For initialiser must not be declared as static or extern"
                                        .into(),
                                );
                            }
                            vs.push(v)
                        }
                        _ => {
                            return Err(
                                "For initialiser must be a variable or list of variables".into()
                            );
                        }
                    }
                }
                Ok(ForInitialiserNode::Declaration(vs))
            } else if let BlockItemNode::Statement(StatementNode::Expression(expression)) =
                block_item
            {
                Ok(ForInitialiserNode::Expression(Some(expression)))
            } else {
                Err("Unexpected function declaration in the initialiser of a for loop".into())
            }
        } else {
            let expression = ForInitialiserNode::Expression(self.parse(context)?);
            // pop the extra semi-colon off of self, since expressions themselves don't
            // expect semi-colons
            self.expect(Token::SemiColon)?;
            Ok(expression)
        }
    }
}

impl Parse<InitialiserNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<InitialiserNode, Box<dyn Error>> {
        let init = match self.peek()? {
            Token::OpenBrace => {
                self.expect(Token::OpenBrace)?;
                let mut initialisers = Vec::new();
                initialisers.push(self.parse(context)?);
                while matches!(self.peek()?, Token::Comma) {
                    self.expect(Token::Comma)?;
                    if matches!(self.peek()?, Token::CloseBrace) {
                        //baited, no new init here
                        break;
                    }
                    initialisers.push(self.parse(context)?);
                }
                self.expect(Token::CloseBrace)?;
                InitialiserWithoutType::Compound(initialisers)
            }
            _ => InitialiserWithoutType::Single(self.parse(context)?),
        };
        Ok(init.into())
    }
}

impl From<InitialiserWithoutType> for InitialiserNode {
    fn from(val: InitialiserWithoutType) -> Self {
        InitialiserNode(val, None)
    }
}

impl Parse<Constant> for VecDeque<Token> {
    fn parse(&mut self, _context: &mut ParseContext) -> Result<Constant, Box<dyn Error>> {
        match self.read()? {
            Token::IntegerConstant(v) => Ok(if v <= i32::MAX.try_into().unwrap() {
                Constant::Integer(v.try_into().unwrap())
            } else if v <= i64::MAX.try_into().unwrap() {
                Constant::Long(v.try_into().unwrap())
            } else {
                Constant::UnsignedLong(v)
            }),
            Token::UnsignedIntegerConstant(v) => Ok(if v <= u32::MAX.into() {
                Constant::UnsignedInteger(v.try_into().unwrap())
            } else {
                Constant::UnsignedLong(v)
            }),
            Token::LongConstant(v) => Ok(if v <= i64::MAX.try_into().unwrap() {
                Constant::Long(v.try_into().unwrap())
            } else {
                Constant::UnsignedLong(v)
            }),
            Token::LongLongConstant(v) => Ok(if v <= i64::MAX.try_into().unwrap() {
                Constant::LongLong(v.try_into().unwrap())
            } else {
                Constant::UnsignedLongLong(v)
            }),
            Token::UnsignedLongConstant(v) => Ok(Constant::UnsignedLong(v)),
            Token::UnsignedLongLongConstant(v) => Ok(Constant::UnsignedLongLong(v)),
            Token::FloatConstant(v) => Ok(Constant::Float(v)),
            Token::DoubleConstant(v) => Ok(Constant::Double(v)),
            Token::LongDoubleConstant(v) => Ok(Constant::LongDouble(v)),
            Token::CharacterConstant(num) => Ok(Constant::Integer(num.into())),
            t => Err(format!(
                "Single token of a constant expression was not a constant, got: {:?}",
                t
            )
            .into()),
        }
    }
}

impl Parse<UnaryOperatorNode> for VecDeque<Token> {
    fn parse(&mut self, _context: &mut ParseContext) -> Result<UnaryOperatorNode, Box<dyn Error>> {
        match self.read()? {
            Token::Hyphen => Ok(UnaryOperatorNode::Negate),
            Token::Tilde => Ok(UnaryOperatorNode::Complement),
            Token::Not => Ok(UnaryOperatorNode::Not),
            Token::Plus => Ok(UnaryOperatorNode::Identity),
            Token::Increment => Ok(UnaryOperatorNode::PrefixIncrement),
            Token::Decrement => Ok(UnaryOperatorNode::PrefixDecrement),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }
}

impl UnaryOperatorNode {
    fn parse_as_suffix(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match tokens.read()? {
            Token::Increment => Ok(UnaryOperatorNode::SuffixIncrement),
            Token::Decrement => Ok(UnaryOperatorNode::SuffixDecrement),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }
}

impl Parse<BinaryOperatorNode> for VecDeque<Token> {
    fn parse(&mut self, _context: &mut ParseContext) -> Result<BinaryOperatorNode, Box<dyn Error>> {
        Ok(match self.read()? {
            Token::Star => BinaryOperatorNode::Multiply,
            Token::Slash => BinaryOperatorNode::Divide,
            Token::Percent => BinaryOperatorNode::Mod,

            Token::Plus => BinaryOperatorNode::Add,
            Token::Hyphen => BinaryOperatorNode::Subtract,

            Token::ShiftLeft => BinaryOperatorNode::ShiftLeft,
            Token::ShiftRight => BinaryOperatorNode::ShiftRight,

            Token::Less => BinaryOperatorNode::Less,
            Token::LessEqual => BinaryOperatorNode::LessEqual,
            Token::Greater => BinaryOperatorNode::Greater,
            Token::GreaterEqual => BinaryOperatorNode::GreaterEqual,

            Token::Equal => BinaryOperatorNode::Equal,
            Token::NotEqual => BinaryOperatorNode::NotEqual,

            Token::BitwiseAnd => BinaryOperatorNode::BitwiseAnd,
            Token::BitwiseXor => BinaryOperatorNode::BitwiseXor,
            Token::BitwiseOr => BinaryOperatorNode::BitwiseOr,

            Token::And => BinaryOperatorNode::And,

            Token::Or => BinaryOperatorNode::Or,

            a => return Err(format!("token is not a binary operation: {:?}", a).into()),
        })
    }
}
impl BinaryOperatorNode {
    fn precedence(tokens: &mut VecDeque<Token>) -> Result<Option<usize>, Box<dyn Error>> {
        if tokens.is_empty() {
            Ok(None)
        } else {
            Ok(Some(match tokens.peek()? {
                Token::Increment => 60,
                Token::Decrement => 60,

                Token::Star => 50,
                Token::Slash => 50,
                Token::Percent => 50,

                Token::Plus => 45,
                Token::Hyphen => 45,

                Token::ShiftLeft => 40,
                Token::ShiftRight => 40,

                Token::Less => 35,
                Token::LessEqual => 35,
                Token::Greater => 35,
                Token::GreaterEqual => 35,

                Token::Equal => 30,
                Token::NotEqual => 30,

                Token::BitwiseAnd => 25,
                Token::BitwiseXor => 20,
                Token::BitwiseOr => 15,

                Token::And => 10,

                Token::Or => 5,

                Token::Question => 3,

                Token::Assignment => 1,
                Token::AddAssign => 1,
                Token::SubtractAssign => 1,
                Token::MultiplyAssign => 1,
                Token::DivideAssign => 1,
                Token::ModAssign => 1,
                Token::BitwiseAndAssign => 1,
                Token::BitwiseXorAssign => 1,
                Token::BitwiseOrAssign => 1,
                Token::ShiftLeftAssign => 1,
                Token::ShiftRightAssign => 1,

                _ => return Ok(None),
            }))
        }
    }
}
