use super::lexer::{Keyword, Token, Type};
use std::{collections::VecDeque, error::Error, mem::discriminant};

mod display;

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

pub fn parse(mut lexed: VecDeque<Token>) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(&mut lexed)
}

fn expect(expected: Token, input: Token) -> Result<(), Box<dyn Error>> {
    if discriminant(&expected) != discriminant(&input) {
        return Err(format!(
            "Unexpected token, got: {:?}, expecting {:?}",
            input, expected
        )
        .into());
    }
    Ok(())
}

fn read(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Too few tokens in identifier node".into())
}

fn read_until_token(
    tokens: &mut VecDeque<Token>,
    expected: Token,
) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut output = VecDeque::new();
    while !tokens.is_empty() {
        if discriminant(&expected) == discriminant(&tokens[0]) {
            if output.is_empty() {
                return Err(format!(
                    "Didn't find any tokens between current token and {:?}",
                    expected
                )
                .into());
            } else {
                return Ok(output);
            }
        }

        output.push_back(tokens.pop_front().unwrap());
    }
    Err(format!("Could not find expected token: {:?}", expected).into())
}

fn read_until_paren(tokens: &mut VecDeque<Token>) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut output = VecDeque::new();
    let mut nesting_level = 0;
    while !tokens.is_empty() {
        let d = discriminant(&tokens[0]);
        if d == discriminant(&Token::CloseParen) {
            if output.is_empty() {
                return Err(format!(
                    "Didn't find any tokens between current token and {:?}",
                    &Token::CloseParen
                )
                .into());
            } else if nesting_level == 0 {
                return Ok(output);
            } else {
                nesting_level -= 1;
            }
        } else if d == discriminant(&Token::OpenParen) {
            nesting_level += 1;
        }

        output.push_back(tokens.pop_front().unwrap());
    }
    Err(format!("Could not find expected token: {:?}", Token::CloseParen).into())
}

fn read_last(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    let token = tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Too few tokens in identifier node".into())?;
    if !tokens.is_empty() {
        return Err(format!("Unexpected extra tokens in definition, got: {:?}", tokens).into());
    };
    Ok(token)
}

fn identifier_to_string(token: Token) -> Result<String, Box<dyn Error>> {
    if let Token::Identifier(string) = token {
        Ok(string)
    } else {
        Err(format!(
            "Wrong token received, expected: {:?} got: {:?}",
            Token::Identifier(String::new()),
            token
        )
        .into())
    }
}

trait Parse
where
    Self: Sized,
{
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>>;
}

impl Parse for ProgramNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        let function = FunctionNode::parse(tokens)?;

        Ok(ProgramNode {
            function,
            has_comments: false,
        })
    }
}

impl Parse for FunctionNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        expect(Token::Keyword(Keyword::Int), read(tokens)?)?;
        let name = identifier_to_string(read(tokens)?)?;
        expect(Token::OpenParen, read(tokens)?)?;
        expect(Token::Keyword(Keyword::Void), read(tokens)?)?;
        expect(Token::CloseParen, read(tokens)?)?;
        expect(Token::OpenBrace, read(tokens)?)?;
        let mut body_tokens: VecDeque<Token> = VecDeque::new();
        while !tokens.is_empty() && tokens[0] != Token::CloseBrace {
            body_tokens.push_back(read(tokens)?);
        }
        let body = StatementNode::parse(&mut body_tokens)?;
        expect(Token::CloseBrace, read_last(tokens)?)?;

        Ok(FunctionNode { name, body })
    }
}

impl Parse for StatementNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        expect(Token::Keyword(Keyword::Return), read(tokens)?)?;
        let expression = ExpressionNode::parse(&mut read_until_token(tokens, Token::SemiColon)?)?;
        expect(Token::SemiColon, read_last(tokens)?)?;

        Ok(StatementNode::Return(expression))
    }
}

impl Parse for ExpressionNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        let output = ExpressionNode::parse_with_level(tokens, 0)?;
        if !tokens.is_empty() {
            Err(format!(
                "Extra characters found in expression after expression: {:?}",
                tokens
            )
            .into())
        } else {
            Ok(output)
        }
    }
}
impl ExpressionNode {
    fn parse_with_level(
        tokens: &mut VecDeque<Token>,
        level: usize,
    ) -> Result<Self, Box<dyn Error>> {
        let mut left = ExpressionNode::parse_factor(tokens)?;
        while let Some((node, precedence)) = BinaryOperatorNode::peek_operator(tokens, level) {
            // delete the first token from the list (we just parsed it above)
            let _ = read(tokens)?;
            let right = ExpressionNode::parse_with_level(tokens, precedence)?;
            left = ExpressionNode::Binary(node, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        let token = read(tokens)?;
        match token {
            Token::Constant(value) => Ok(ExpressionNode::Constant(value)),
            Token::OpenParen => {
                let expression = ExpressionNode::parse(&mut read_until_paren(tokens)?)?;
                expect(Token::CloseParen, read(tokens)?)?;
                Ok(expression)
            }
            Token::Hyphen | Token::Tilde => {
                let operator = UnaryOperatorNode::parse(&mut vec![token].into())?;
                // don't evaluate other binary expressions here, unary operations take precedence
                // over everything
                let expression = ExpressionNode::parse_factor(tokens)?;
                Ok(ExpressionNode::Unary(operator, Box::new(expression)))
            }
            _ => Err(format!("Invalid token at start of expression: {:?}", token).into()),
        }
    }
}

impl Parse for UnaryOperatorNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        match read_last(tokens)? {
            Token::Hyphen => Ok(UnaryOperatorNode::Negate),
            Token::Tilde => Ok(UnaryOperatorNode::Complement),
            _ => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                tokens[0],
            )
            .into()),
        }
    }
}

impl Parse for BinaryOperatorNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        match read_last(tokens)? {
            Token::Hyphen => Ok(BinaryOperatorNode::Add),
            Token::Tilde => Ok(BinaryOperatorNode::Divide),
            _ => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                tokens[0],
            )
            .into()),
        }
    }
}

impl BinaryOperatorNode {
    fn peek_operator(
        tokens: &mut VecDeque<Token>,
        level: usize,
    ) -> Option<(BinaryOperatorNode, usize)> {
        if tokens.is_empty() {
            return None;
        }
        match tokens[0] {
            Token::Star if level < 50 => Some((BinaryOperatorNode::Multiply, 50)),
            Token::Slash if level < 50 => Some((BinaryOperatorNode::Divide, 50)),
            Token::Percent if level < 50 => Some((BinaryOperatorNode::Mod, 50)),

            Token::Plus if level < 45 => Some((BinaryOperatorNode::Add, 45)),
            Token::Hyphen if level < 45 => Some((BinaryOperatorNode::Subtract, 45)),

            _ => None,
        }
    }
}
