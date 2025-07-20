use super::{
    lexer::{Keyword, Token, Type},
    IndentDisplay,
};
use std::{collections::VecDeque, error::Error, fmt::Display, mem::discriminant};

#[derive(Clone)]
pub struct ProgramNode {
    pub function: FunctionNode,
    has_comments: bool,
}

#[derive(Clone)]
pub struct FunctionNode {
    pub name: String,
    pub body: StatementNode,
}

#[derive(Clone)]
pub enum StatementNode {
    Return(ExpressionNode),
}

#[derive(Clone)]
pub enum ExpressionNode {
    Constant(Type),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
}

#[derive(Clone)]
pub enum UnaryOperatorNode {
    Complement,
    Negate,
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
        return Err(format!(
            "Unexpected extra tokens in identifier definition, got: {:?}",
            tokens
        )
        .into());
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
            "{:indent$}Function (\n{:indent$}    name: {},\n{:indent$}    body: {}\n{:indent$})",
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
                format!(
                    "Return (\n{:indent$}    {}\n{:indent$})",
                    "",
                    value.fmt_indent(indent + 4, comments),
                    "",
                    indent = indent
                )
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
            Self::Constant(value) => {
                format!("Constant({})", value.fmt_indent(indent + 4, comments),)
            }
            Self::Unary(operator, exp) => {
                format!(
                    "Operation({}, {})",
                    operator.fmt_indent(indent + 4, comments),
                    exp.fmt_indent(indent + 4, comments),
                )
            }
        }
    }
}

impl Parse for ExpressionNode {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Box<dyn Error>> {
        let token = steal(tokens)?;
        match token {
            Token::Constant(value) => {
                if !tokens.is_empty() {
                    Err(format!(
                        "Extra characters found in expression after constant: {:?}",
                        tokens
                    )
                    .into())
                } else {
                    Ok(ExpressionNode::Constant(value))
                }
            }
            Token::OpenParen => {
                // TODO: fix the fact that this doesn't match closing and opening parens properly
                let expression = ExpressionNode::parse(&mut steal_until_paren(tokens)?)?;
                expect(Token::CloseParen, steal_last(tokens)?)?;
                Ok(expression)
            }
            _ => {
                let operator = UnaryOperatorNode::parse(&mut vec![token].into())?;
                let expression = ExpressionNode::parse(tokens)?;
                Ok(ExpressionNode::Unary(operator, Box::new(expression)))
            }
        }
    }
}

impl IndentDisplay for UnaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Self::Complement => "complement".to_string(),
            Self::Negate => "negate".to_string(),
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
