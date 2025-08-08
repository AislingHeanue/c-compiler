use super::{
    AbstractDeclarator, BinaryOperatorNode, BlockItemNode, DeclarationNode, Declarator,
    ExpressionNode, ExpressionWithoutType, ForInitialiserNode, InitialiserNode,
    InitialiserWithoutType, ProgramNode, StatementNode, Type, UnaryOperatorNode,
};
use crate::compiler::{lexer::Token, types::Constant};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    mem::discriminant,
};

mod abstract_declarator;
mod block_item_node;
mod declaration_node;
mod declarator;
mod expression_node;
mod parsed_types;
mod statement_node;

trait Parse
where
    Self: Sized,
{
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>>;
}

pub struct ParseContext {
    // map from string to sting-as-seen-in-assembly and is-externally-linked
    current_scope_identifiers: HashMap<String, (String, bool)>,
    outer_scope_identifiers: HashMap<String, (String, bool)>,
    num_variables: usize,
    do_not_validate: bool,
    // this prevent creating an extra new scope entering function bodies
    current_block_is_function_body: bool,
    current_scope_is_file: bool,
}

pub fn do_parse(
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

fn expect(tokens: &mut VecDeque<Token>, expected: Token) -> Result<(), Box<dyn Error>> {
    let token = read(tokens)?;
    if discriminant(&expected) != discriminant(&token) {
        return Err(format!(
            "Unexpected token, got {:?}, expecting {:?}",
            token, expected
        )
        .into());
    }
    Ok(())
}

fn read(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    // println!("popping {:?}", tokens.front());
    tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())
}

fn peek(tokens: &VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    // println!("peeking {:?}", tokens.front());
    Ok(tokens
        .front()
        .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())?
        .clone())
}

impl Parse for ProgramNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut declarations: Vec<DeclarationNode> = Vec::new();
        while !tokens.is_empty() {
            declarations.push(DeclarationNode::parse(tokens, context)?)
        }

        Ok(ProgramNode { declarations })
    }
}

impl Parse for ForInitialiserNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        if peek(tokens)?.is_specifier() {
            let block_item = BlockItemNode::parse(tokens, context)?;
            if let BlockItemNode::Declaration(DeclarationNode::Variable(v)) = block_item {
                if !context.do_not_validate && v.storage_class.is_some() {
                    return Err("For initialiser must not be declared as static or extern".into());
                }
                Ok(ForInitialiserNode::Declaration(v))
            } else if let BlockItemNode::Statement(StatementNode::Expression(expression)) =
                block_item
            {
                Ok(ForInitialiserNode::Expression(Some(expression)))
            } else {
                Err("Unexpected function declaration in the initialiser of a for loop".into())
            }
        } else {
            let expression =
                ForInitialiserNode::Expression(Option::<ExpressionNode>::parse(tokens, context)?);
            // pop the extra semi-colon off of tokens, since expressions themselves don't
            // expect semi-colons
            expect(tokens, Token::SemiColon)?;
            Ok(expression)
        }
    }
}

impl Parse for InitialiserNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let init = match peek(tokens)? {
            Token::OpenBrace => {
                expect(tokens, Token::OpenBrace)?;
                let mut initialisers = Vec::new();
                initialisers.push(InitialiserNode::parse(tokens, context)?);
                while matches!(peek(tokens)?, Token::Comma) {
                    expect(tokens, Token::Comma)?;
                    if matches!(peek(tokens)?, Token::CloseBrace) {
                        //baited, no new init here
                        break;
                    }
                    initialisers.push(InitialiserNode::parse(tokens, context)?);
                }
                expect(tokens, Token::CloseBrace)?;
                InitialiserWithoutType::Compound(initialisers)
            }
            _ => InitialiserWithoutType::Single(ExpressionNode::parse(tokens, context)?),
        };
        Ok(init.into())
    }
}

impl From<InitialiserWithoutType> for InitialiserNode {
    fn from(val: InitialiserWithoutType) -> Self {
        InitialiserNode(val, None)
    }
}

impl Parse for Constant {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read(tokens)? {
            Token::IntegerConstant(v) => Ok(if v <= i32::MAX.into() {
                Constant::Integer(v.try_into().unwrap())
            } else {
                Constant::Long(v)
            }),
            Token::UnsignedIntegerConstant(v) => Ok(if v <= u32::MAX.into() {
                Constant::UnsignedInteger(v.try_into().unwrap())
            } else {
                Constant::UnsignedLong(v)
            }),
            Token::LongConstant(v) => Ok(Constant::Long(v)),
            Token::UnsignedLongConstant(v) => Ok(Constant::UnsignedLong(v)),
            Token::DoubleConstant(v) => Ok(Constant::Double(v)),
            Token::CharacterConstant(num) => Ok(Constant::Integer(num.into())),
            _ => unreachable!(),
        }
    }
}

impl Parse for UnaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read(tokens)? {
            Token::Hyphen => Ok(UnaryOperatorNode::Negate),
            Token::Tilde => Ok(UnaryOperatorNode::Complement),
            Token::Not => Ok(UnaryOperatorNode::Not),
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
        match read(tokens)? {
            Token::Increment => Ok(UnaryOperatorNode::SuffixIncrement),
            Token::Decrement => Ok(UnaryOperatorNode::SuffixDecrement),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }

    fn precedence() -> usize {
        55
    }
}

impl Parse for BinaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(match read(tokens)? {
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
        Ok(Some(match peek(tokens)? {
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
