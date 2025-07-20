use super::{
    lexer::{Keyword, Token, Type},
    IndentDisplay,
};
use std::{error::Error, fmt::Display, mem::discriminant};

pub struct ProgramNode {
    pub function: FunctionNode,
    has_comments: bool,
}

pub struct FunctionNode {
    pub name: IdentifierNode,
    pub body: StatementNode,
}

pub struct IdentifierNode {
    pub value: String,
}

pub enum StatementNode {
    Return(ExpressionNode),
}

pub enum ExpressionNode {
    Constant(Type),
}

pub fn parse(lexed: Vec<Token>) -> Result<ProgramNode, Box<dyn Error>> {
    ProgramNode::parse(lexed)
}

fn expect(expected: &Token, input: &Token) -> Result<(), Box<dyn Error>> {
    if discriminant(expected) != discriminant(input) {
        return Err(format!(
            "Unexpected token, got: {:?}, expecting {:?}",
            input, expected
        )
        .into());
    }
    Ok(())
}

trait Parse
where
    Self: Sized,
{
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>>;
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
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>> {
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
            self.name.fmt_indent(indent + 4, comments),
            "",
            self.body.fmt_indent(indent + 4, comments),
            "",
            indent = indent,
        )
    }
}

impl Parse for FunctionNode {
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>> {
        expect(&Token::Keyword(Keyword::Int), &tokens[0])?;
        let name = IdentifierNode::parse(vec![tokens[1].clone()])?;
        expect(&Token::OpenParen, &tokens[2])?;
        expect(&Token::Keyword(Keyword::Void), &tokens[3])?;
        expect(&Token::CloseParen, &tokens[4])?;
        expect(&Token::OpenBrace, &tokens[5])?;
        let mut i = 6;
        let mut body_tokens: Vec<Token> = Vec::new();
        while i < tokens.len() && tokens[i] != Token::CloseBrace {
            body_tokens.push(tokens[i].clone());
            i += 1;
        }
        let body = StatementNode::parse(body_tokens)?;
        expect(&Token::CloseBrace, &tokens[i])?;
        if i != tokens.len() - 1 {
            return Err(format!(
                "Unexpected extra tokens in function definition, got: {:?}",
                tokens[i..].to_vec()
            )
            .into());
        }

        Ok(FunctionNode { name, body })
    }
}

impl IndentDisplay for IdentifierNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        format!("Identifier(\"{}\")", self.value,)
    }
}

impl Parse for IdentifierNode {
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>> {
        if tokens.len() > 1 {
            Err(format!(
                "Unexpected extra tokens in identifier definition, got: {:?}",
                tokens[1..].to_vec()
            )
            .into())
        } else if let Token::Identifier(value) = &tokens[0] {
            Ok(IdentifierNode {
                value: value.to_string(),
            })
        } else {
            Err(format!(
                "Unexpected token, got: {:?}, expecting an identifier token",
                tokens[0],
            )
            .into())
        }
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
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>> {
        expect(&Token::Keyword(Keyword::Return), &tokens[0])?;
        let expression = ExpressionNode::parse(vec![tokens[1].clone()])?;
        expect(&Token::SemiColon, &tokens[2])?;

        Ok(StatementNode::Return(expression))
    }
}

impl IndentDisplay for ExpressionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            Self::Constant(value) => {
                format!("Constant({})", value.fmt_indent(indent + 4, comments),)
            }
        }
    }
}

impl Parse for ExpressionNode {
    fn parse(tokens: Vec<Token>) -> Result<Self, Box<dyn Error>> {
        if tokens.len() > 1 {
            Err(format!(
                "Unexpected extra tokens in expression definition, got: {:?}",
                tokens[1..].to_vec()
            )
            .into())
        } else if let Token::Constant(value) = &tokens[0] {
            Ok(ExpressionNode::Constant(value.clone()))
        } else {
            Err(format!(
                "Unexpected token, got: {:?}, expecting an identifier token",
                tokens[0],
            )
            .into())
        }
    }
}
