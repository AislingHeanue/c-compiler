use super::{
    lexer::{Keyword, Token, Type},
    IndentDisplay,
};
use std::{collections::VecDeque, error::Error, fmt::Display, mem::discriminant};

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
}

#[derive(Debug)]
pub enum BinaryOperatorNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
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

fn steal(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Too few tokens in identifier node".into())
}

fn steal_until(
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

fn steal_until_paren(tokens: &mut VecDeque<Token>) -> Result<VecDeque<Token>, Box<dyn Error>> {
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

fn parse_string(token: Token) -> Result<String, Box<dyn Error>> {
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

fn steal_last(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    let token = tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Too few tokens in identifier node".into())?;
    if !tokens.is_empty() {
        return Err(format!("Unexpected extra tokens in definition, got: {:?}", tokens).into());
    };
    Ok(token)
}

trait Parse
where
    Self: Sized,
{
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>>;
}

impl Display for ProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Program (\n{}\n)",
            self.function.fmt_indent(4, self.has_comments)
        )
    }
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

impl IndentDisplay for FunctionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        format!(
            // TODO: reformat when functions can have multiple statements
            "{:indent$}Function (\n{:indent$}    name: {},\n{:indent$}    body: {},\n{:indent$})",
            "",
            "",
            self.name,
            "",
            self.body.fmt_indent(indent + 4, comments),
            "",
            indent = indent,
        )
    }
}

impl Parse for FunctionNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        expect(Token::Keyword(Keyword::Int), steal(tokens)?)?;
        let name = parse_string(steal(tokens)?)?;
        expect(Token::OpenParen, steal(tokens)?)?;
        expect(Token::Keyword(Keyword::Void), steal(tokens)?)?;
        expect(Token::CloseParen, steal(tokens)?)?;
        expect(Token::OpenBrace, steal(tokens)?)?;
        let mut body_tokens: VecDeque<Token> = VecDeque::new();
        while !tokens.is_empty() && tokens[0] != Token::CloseBrace {
            body_tokens.push_back(steal(tokens)?);
        }
        let body = StatementNode::parse(&mut body_tokens)?;
        expect(Token::CloseBrace, steal_last(tokens)?)?;

        Ok(FunctionNode { name, body })
    }
}

impl IndentDisplay for StatementNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            Self::Return(value) => {
                format!("Return {}", value.fmt_indent(indent + 4, comments),)
            }
        }
    }
}

impl Parse for StatementNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        expect(Token::Keyword(Keyword::Return), steal(tokens)?)?;
        let expression = ExpressionNode::parse(&mut steal_until(tokens, Token::SemiColon)?)?;
        expect(Token::SemiColon, steal_last(tokens)?)?;

        Ok(StatementNode::Return(expression))
    }
}

impl IndentDisplay for ExpressionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            Self::Constant(value) => value.fmt_indent(indent + 4, comments),
            Self::Unary(operator, exp) => {
                format!(
                    "({}{})",
                    operator.fmt_indent(indent + 4, comments),
                    exp.fmt_indent(indent + 4, comments),
                )
            }
            Self::Binary(operator, left, right) => {
                format!(
                    "({} {} {})",
                    left.fmt_indent(indent + 4, comments),
                    operator.fmt_indent(indent + 4, comments),
                    right.fmt_indent(indent + 4, comments)
                )
            }
        }
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
        let mut left = Self::parse_factor(tokens)?;
        while let Some((node, precedence)) = BinaryOperatorNode::peek_operator(tokens, level) {
            // delete the first token from the list (we just parsed it above)
            let _ = steal(tokens)?;
            let right = Self::parse_with_level(tokens, precedence)?;
            left = ExpressionNode::Binary(node, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        let token = steal(tokens)?;
        match token {
            Token::Constant(value) => Ok(ExpressionNode::Constant(value)),
            Token::OpenParen => {
                let expression = ExpressionNode::parse(&mut steal_until_paren(tokens)?)?;
                expect(Token::CloseParen, steal(tokens)?)?;
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

impl IndentDisplay for UnaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Self::Complement => "~".to_string(),
            Self::Negate => "-".to_string(),
        }
    }
}

impl Parse for UnaryOperatorNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        match steal_last(tokens)? {
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

impl IndentDisplay for BinaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            BinaryOperatorNode::Add => "+".to_string(),
            BinaryOperatorNode::Subtract => "-".to_string(),
            BinaryOperatorNode::Multiply => "*".to_string(),
            BinaryOperatorNode::Divide => "/".to_string(),
            BinaryOperatorNode::Mod => "%".to_string(),
        }
    }
}

impl Parse for BinaryOperatorNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        match steal_last(tokens)? {
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
