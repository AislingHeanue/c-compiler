use super::{
    BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode, FunctionNode, Parse,
    ProgramNode, StatementNode, Type, UnaryOperatorNode,
};
use crate::compiler::{lexer::Token, parser::ValidateContext};
use std::{
    collections::VecDeque,
    error::Error,
    mem::{discriminant, Discriminant},
};

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
    expected: Vec<Token>,
) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut output = VecDeque::new();
    let discriminants: Vec<Discriminant<Token>> = expected.iter().map(discriminant).collect();

    while !tokens.is_empty() {
        if discriminants.contains(&discriminant(tokens.front().unwrap())) {
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

fn read_until_match(
    tokens: &mut VecDeque<Token>,
    open: Token,
    close: Token,
    allow_empty: bool,
) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut output = VecDeque::new();
    let mut nesting_level = 0;
    while !tokens.is_empty() {
        let d = discriminant(tokens.front().unwrap());
        if d == discriminant(&close) {
            if output.is_empty() && !allow_empty {
                return Err(format!(
                    "Didn't find any tokens between current token and matching {:?}",
                    &Token::CloseParen
                )
                .into());
            } else if nesting_level == 0 {
                return Ok(output);
            } else {
                nesting_level -= 1;
            }
        } else if d == discriminant(&open) {
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

impl Parse for ProgramNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        let function = FunctionNode::parse(tokens, context)?;

        Ok(ProgramNode {
            function,
            has_comments: false,
        })
    }
}

impl Parse for FunctionNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        expect(Token::KeywordInt, read(tokens)?)?;
        let name = identifier_to_string(read(tokens)?)?;
        expect(Token::OpenParen, read(tokens)?)?;
        expect(Token::KeywordVoid, read(tokens)?)?;
        expect(Token::CloseParen, read(tokens)?)?;
        expect(Token::OpenBrace, read(tokens)?)?;
        let body = Block::parse(
            &mut read_until_match(tokens, Token::OpenBrace, Token::CloseBrace, true)?,
            context,
        )?;
        expect(Token::CloseBrace, read_last(tokens)?)?;

        Ok(FunctionNode { name, body })
    }
}

impl Parse for Block {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut items: Vec<BlockItemNode> = Vec::new();
        while !tokens.is_empty() {
            items.push(match tokens.front().unwrap() {
                Token::KeywordInt => {
                    BlockItemNode::Declaration(DeclarationNode::parse(tokens, context)?)
                }
                _ => BlockItemNode::Statement(StatementNode::parse(tokens, context)?),
            })
        }
        Ok(items)
    }
}

impl Parse for DeclarationNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        expect(Token::KeywordInt, read(tokens)?)?;
        let t = read(tokens)?;
        let name = match t {
            Token::Identifier(name) => Ok(name),
            _ => Err::<String, Box<dyn Error>>(
                format!(
                    "Expected a string identifier reading a declaration name, got {:?}",
                    t
                )
                .into(),
            ),
        }?;
        // VALIDATION STEP: Make sure this variable has not already been defined 'in this scope'
        if context.variables.contains_key(&name) {
            return Err(format!("Duplicate definition of name: {}", name).into());
        }
        context.num_variables += 1;
        let new_name = format!("{}:{}", name, context.num_variables);
        context.variables.insert(name.to_string(), new_name.clone());

        match read(tokens)? {
            Token::SemiColon => Ok(DeclarationNode::Declaration(Type::Integer, new_name, None)),
            Token::Assignment => {
                let expression = ExpressionNode::parse(
                    &mut read_until_token(tokens, vec![Token::SemiColon])?,
                    context,
                )?;
                expect(Token::SemiColon, read(tokens)?)?;
                Ok(DeclarationNode::Declaration(
                    Type::Integer,
                    new_name,
                    Some(expression),
                ))
            }
            t => Err(format!("Unexpected token in declaration of {}: {:?}", name, t).into()),
        }
    }
}

impl Parse for StatementNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        match tokens.front().ok_or("Statement has no tokens")? {
            Token::KeywordReturn => {
                expect(Token::KeywordReturn, read(tokens)?)?;
                let expression = ExpressionNode::parse(
                    &mut read_until_token(tokens, vec![Token::SemiColon])?,
                    context,
                )?;
                expect(Token::SemiColon, read(tokens)?)?;
                Ok(StatementNode::Return(expression))
            }
            Token::SemiColon => {
                expect(Token::SemiColon, read(tokens)?)?;
                Ok(StatementNode::Pass)
            }
            _ => {
                let expression = ExpressionNode::parse(
                    &mut read_until_token(tokens, vec![Token::SemiColon])?,
                    context,
                )?;
                expect(Token::SemiColon, read(tokens)?)?;
                Ok(StatementNode::Expression(expression))
            }
        }
    }
}

impl Parse for ExpressionNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        let output = ExpressionNode::parse_with_level(tokens, context, 0)?;
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
        context: &mut ValidateContext,
        level: usize,
    ) -> Result<Self, Box<dyn Error>> {
        let mut left = ExpressionNode::parse_factor(tokens, context)?;
        while let Some(precedence) = BinaryOperatorNode::precedence(tokens.front()) {
            if precedence < level {
                break;
            }
            match read(tokens)? {
                Token::Assignment => {
                    // VALIDATION STEP: Check to make sure we are not assigning to any expression
                    // more complex than a single identifier
                    if !matches!(left, ExpressionNode::Var(_)) {
                        return Err(format!("Found a non-variable to the left side of an assignment, can't assign to {:?}", left).into());
                    }
                    let right = ExpressionNode::parse_with_level(tokens, context, precedence)?;
                    left = ExpressionNode::Assignment(Box::new(left), Box::new(right));
                }
                t => {
                    let right = ExpressionNode::parse_with_level(tokens, context, precedence + 1)?;
                    left = ExpressionNode::Binary(
                        BinaryOperatorNode::parse(&mut vec![t].into(), context)?,
                        Box::new(left),
                        Box::new(right),
                    );
                }
            }
        }
        Ok(left)
    }

    fn parse_factor(
        tokens: &mut VecDeque<Token>,
        context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        let token = read(tokens)?;
        match token {
            Token::IntegerConstant(value) => Ok(ExpressionNode::IntegerConstant(value)),
            Token::OpenParen => {
                let expression = ExpressionNode::parse(
                    &mut read_until_match(tokens, Token::OpenParen, Token::CloseParen, false)?,
                    context,
                )?;
                expect(Token::CloseParen, read(tokens)?)?;
                Ok(expression)
            }
            Token::Identifier(name) => {
                // VALIDATION STEP: Check the variable has been declared
                match context.variables.get(&name) {
                    Some(s) => Ok(ExpressionNode::Var(s.to_string())),
                    None => Err(format!("Variable used before declaration: {}", name).into()),
                }
            }
            Token::Hyphen | Token::Tilde | Token::Not => {
                let operator = UnaryOperatorNode::parse(&mut vec![token].into(), context)?;
                // don't evaluate other binary expressions here, unary operations take precedence
                // over everything
                let expression = ExpressionNode::parse_factor(tokens, context)?;
                Ok(ExpressionNode::Unary(operator, Box::new(expression)))
            }
            t => Err(format!("Invalid token at start of expression: {:?}", t).into()),
        }
    }
}

impl Parse for UnaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read_last(tokens)? {
            Token::Hyphen => Ok(UnaryOperatorNode::Negate),
            Token::Tilde => Ok(UnaryOperatorNode::Complement),
            Token::Not => Ok(UnaryOperatorNode::Not),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }
}

impl Parse for BinaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(match read_last(tokens)? {
            Token::Star => BinaryOperatorNode::Multiply,
            Token::Slash => BinaryOperatorNode::Divide,
            Token::Percent => BinaryOperatorNode::Mod,

            Token::Plus => BinaryOperatorNode::Add,
            Token::Hyphen => BinaryOperatorNode::Subtract,

            Token::Less => BinaryOperatorNode::Less,
            Token::LessEqual => BinaryOperatorNode::LessEqual,
            Token::Greater => BinaryOperatorNode::Greater,
            Token::GreaterEqual => BinaryOperatorNode::GreaterEqual,

            Token::Equal => BinaryOperatorNode::Equal,
            Token::NotEqual => BinaryOperatorNode::NotEqual,

            Token::And => BinaryOperatorNode::And,

            Token::Or => BinaryOperatorNode::Or,

            Token::Assignment => panic!("Assignment can't be translated to a binary op"),

            a => return Err(format!("token is not a binary operation: {:?}", a).into()),
        })
    }
}
impl BinaryOperatorNode {
    fn precedence(maybe_token: Option<&Token>) -> Option<usize> {
        if let Some(token) = maybe_token {
            Some(match token {
                Token::Star => 50,
                Token::Slash => 50,
                Token::Percent => 50,

                Token::Plus => 45,
                Token::Hyphen => 45,

                Token::Less => 35,
                Token::LessEqual => 35,
                Token::Greater => 35,
                Token::GreaterEqual => 35,

                Token::Equal => 30,
                Token::NotEqual => 30,

                Token::And => 10,

                Token::Or => 5,

                Token::Assignment => 1,

                _ => return None,
            })
        } else {
            None
        }
    }
}
