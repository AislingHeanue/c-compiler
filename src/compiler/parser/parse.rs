use super::{
    BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode, ForInitialiserNode,
    FunctionDeclaration, Parse, ProgramNode, StatementNode, Type, UnaryOperatorNode,
    VariableDeclaration,
};
use crate::compiler::{lexer::Token, parser::ParseContext};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    mem::discriminant,
};

fn expect(tokens: &mut VecDeque<Token>, expected: Token) -> Result<(), Box<dyn Error>> {
    // println!("{:?}", expected);
    let token = read(tokens)?;
    if discriminant(&expected) != discriminant(&token) {
        return Err(format!(
            "Unexpected token, got: {:?}, expecting {:?}",
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

fn peek(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    // println!("peeking {:?}", tokens.front());
    Ok(tokens
        .front()
        .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())?
        .clone())
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

fn enter_scope(context: &mut ParseContext) -> HashMap<String, String> {
    let original_outer_scope_variables = context.outer_scope_variables.clone();
    context
        .outer_scope_variables
        .extend(context.current_scope_variables.clone());
    context.current_scope_variables = HashMap::new();

    original_outer_scope_variables
}

fn leave_scope(previous_scope: HashMap<String, String>, context: &mut ParseContext) {
    context.current_scope_variables = context.outer_scope_variables.clone();
    context.outer_scope_variables = previous_scope;
}

impl Parse for ProgramNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut functions: Vec<FunctionDeclaration> = Vec::new();
        while !tokens.is_empty() {
            functions.push(FunctionDeclaration::parse(tokens, context)?)
        }

        Ok(ProgramNode { functions })
    }
}

impl Parse for FunctionDeclaration {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        expect(tokens, Token::KeywordInt)?;
        let name = identifier_to_string(read(tokens)?)?;
        expect(tokens, Token::OpenParen)?;
        let params = FunctionDeclaration::parse_params(tokens, context)?;
        expect(tokens, Token::CloseParen)?;

        match peek(tokens)? {
            Token::SemiColon => {
                expect(tokens, Token::SemiColon)?;
                Ok(FunctionDeclaration {
                    out_type: Type::Integer,
                    name,
                    params,
                    body: None,
                })
            }
            Token::OpenBrace => {
                let body = Block::parse(tokens, context)?;

                Ok(FunctionDeclaration {
                    out_type: Type::Integer,
                    name,
                    params,
                    body: Some(body),
                })
            }

            t => Err(format!("Unexpected token in function declaration: {:?}", t).into()),
        }
    }
}

impl FunctionDeclaration {
    fn parse_params(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Vec<(Type, String)>, Box<dyn Error>> {
        if matches!(peek(tokens)?, Token::KeywordVoid) {
            expect(tokens, Token::KeywordVoid)?;
            return Ok(Vec::new());
        }

        let mut params: Vec<(Type, String)> =
            vec![FunctionDeclaration::parse_param(tokens, context)?];

        while matches!(peek(tokens)?, Token::Comma) {
            expect(tokens, Token::Comma)?;
            params.push(FunctionDeclaration::parse_param(tokens, context)?)
        }

        Ok(Vec::new())
    }

    fn parse_param(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<(Type, String), Box<dyn Error>> {
        let out_type = match read(tokens)? {
            Token::KeywordInt => Type::Integer,
            t => return Err(format!("Invalid type in parameter: {:?}", t).into()),
        };

        let name = match read(tokens)? {
            Token::Identifier(s) => s.to_string(),
            t => return Err(format!("Invalid name in parameter: {:?}", t).into()),
        };

        Ok((out_type, name))
    }
}

impl Parse for Block {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        expect(tokens, Token::OpenBrace)?;

        // it's a new block it's a new scope
        // (and I'm feeling... good)
        let original_outer_scope_variables = enter_scope(context);
        let mut items: Vec<BlockItemNode> = Vec::new();
        while !matches!(peek(tokens)?, Token::CloseBrace) {
            items.push(BlockItemNode::parse(tokens, context)?)
        }
        leave_scope(original_outer_scope_variables, context);

        expect(tokens, Token::CloseBrace)?;

        Ok(items)
    }
}

impl Parse for BlockItemNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        if tokens.is_empty() {
            return Err("Block item has no tokens".into());
        }
        match peek(tokens)? {
            Token::KeywordInt => Ok(BlockItemNode::Declaration(DeclarationNode::parse(
                tokens, context,
            )?)),
            _ => Ok(BlockItemNode::Statement(StatementNode::parse(
                tokens, context,
            )?)),
        }
    }
}

impl Parse for ForInitialiserNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::KeywordInt => Ok(ForInitialiserNode::Declaration(VariableDeclaration::parse(
                tokens, context,
            )?)),
            _ => {
                let expression = ForInitialiserNode::Expression(Option::<ExpressionNode>::parse(
                    tokens, context,
                )?);
                // pop the extra semi-colon off of tokens, since expressions themselves don't
                // expect semi-colons
                expect(tokens, Token::SemiColon)?;
                Ok(expression)
            }
        }
    }
}

impl Parse for DeclarationNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        //read first 3 tokens to determine if it's a function()
        if tokens.len() < 3 {
            return Err(format!("Declaration found with fewer than 3 tokens: {:?}", tokens).into());
        }
        if !matches!(tokens[1], Token::Identifier(_)) {
            return Err(format!(
                "Second token in a declaration is not an identifier: {:?}",
                tokens[1]
            )
            .into());
        }
        match tokens[2] {
            Token::OpenParen => Ok(DeclarationNode::Function(FunctionDeclaration::parse(
                tokens, context,
            )?)),
            _ => Ok(DeclarationNode::Variable(VariableDeclaration::parse(
                tokens, context,
            )?)),
        }
    }
}

impl Parse for VariableDeclaration {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let variable_type = Type::Integer;
        expect(tokens, Token::KeywordInt)?;

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
        if !context.do_not_validate && context.current_scope_variables.contains_key(&name) {
            return Err(format!("Duplicate definition of name: {}", name).into());
        }

        context.num_variables += 1;
        let new_name;
        if context.do_not_validate {
            new_name = name.clone();
        } else {
            new_name = format!("{}:{}", name, context.num_variables);
            context
                .current_scope_variables
                .insert(name, new_name.clone());
        }

        match read(tokens)? {
            Token::SemiColon => Ok(VariableDeclaration {
                variable_type,
                name: new_name.clone(),
                init: None,
            }),
            Token::Assignment => {
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                Ok(VariableDeclaration {
                    variable_type,
                    name: new_name.clone(),
                    init: Some(expression),
                })
            }
            t => Err(format!("Unexpected token in declaration of {}: {:?}", new_name, t).into()),
        }
    }
}

impl Parse for StatementNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::KeywordReturn => {
                expect(tokens, Token::KeywordReturn)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Return(expression))
            }
            Token::KeywordIf => {
                expect(tokens, Token::KeywordIf)?;
                expect(tokens, Token::OpenParen)?;
                let condition = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let then = StatementNode::parse(tokens, context)?;
                let otherwise = match peek(tokens)? {
                    Token::KeywordElse => {
                        expect(tokens, Token::KeywordElse)?;
                        Some(StatementNode::parse(tokens, context)?)
                    }
                    _ => None,
                };
                Ok(StatementNode::If(
                    condition,
                    Box::new(then),
                    Box::new(otherwise),
                ))
            }
            Token::SemiColon => {
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Pass)
            }
            Token::KeywordGoto => {
                expect(tokens, Token::KeywordGoto)?;
                let s = identifier_to_string(read(tokens)?)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Goto(s))
            }
            Token::OpenBrace => {
                let block = Block::parse(tokens, context)?;
                Ok(StatementNode::Compound(block))
            }
            Token::KeywordFor => {
                expect(tokens, Token::KeywordFor)?;
                expect(tokens, Token::OpenParen)?;

                // create a new scope just for the first line of the 'for' declaration
                let outer_scope = enter_scope(context);

                let init = ForInitialiserNode::parse(tokens, context)?;
                let cond = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                let post = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;

                leave_scope(outer_scope, context);
                Ok(StatementNode::For(init, cond, post, Box::new(body), None))
            }
            Token::KeywordDo => {
                expect(tokens, Token::KeywordDo)?;
                let body = StatementNode::parse(tokens, context)?;
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::DoWhile(Box::new(body), expression, None))
            }
            Token::KeywordWhile => {
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;
                Ok(StatementNode::While(expression, Box::new(body), None))
            }
            Token::KeywordBreak => {
                expect(tokens, Token::KeywordBreak)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Break(None))
            }
            Token::KeywordContinue => {
                expect(tokens, Token::KeywordContinue)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Continue(None))
            }
            Token::KeywordSwitch => {
                expect(tokens, Token::KeywordSwitch)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Ok(StatementNode::Switch(
                    expression,
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                    None,
                ))
            }
            Token::KeywordCase => {
                expect(tokens, Token::KeywordCase)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::Colon)?;
                Ok(StatementNode::Case(
                    expression,
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                ))
            }
            Token::KeywordDefault => {
                expect(tokens, Token::KeywordDefault)?;
                expect(tokens, Token::Colon)?;
                Ok(StatementNode::Default(
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                ))
            }
            _ => match (
                peek(tokens)?,
                tokens
                    .get(1)
                    .ok_or::<Box<dyn Error>>("Statement node only has one token".into())?,
            ) {
                (Token::Identifier(s), Token::Colon) => {
                    expect(tokens, Token::Identifier("".to_string()))?;
                    expect(tokens, Token::Colon)?;
                    Ok(StatementNode::Label(
                        s.to_string(),
                        Box::new(StatementNode::parse(tokens, context)?),
                    ))
                }
                _ => {
                    let expression = ExpressionNode::parse(tokens, context)?;
                    expect(tokens, Token::SemiColon)?;
                    Ok(StatementNode::Expression(expression))
                }
            },
        }
    }
}

impl Parse for Option<ExpressionNode> {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        if tokens.is_empty() {
            Ok(None)
        } else {
            Ok(ExpressionNode::parse_with_level(tokens, context, 0, true)?)
        }
    }
}

impl Parse for ExpressionNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(ExpressionNode::parse_with_level(tokens, context, 0, false)?.unwrap())
    }
}

impl ExpressionNode {
    fn parse_with_level(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        level: usize,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let maybe_left = ExpressionNode::parse_factor(tokens, context, allow_empty)?;
        if maybe_left.is_none() {
            // if allow_empty is false, parse_factor will throw the relevant error for here
            return Ok(None);
        }
        let mut left = maybe_left.unwrap();
        while let Some(precedence) = BinaryOperatorNode::precedence(tokens)? {
            if precedence < level {
                break;
            }
            match peek(tokens)? {
                Token::Assignment
                | Token::AddAssign
                | Token::SubtractAssign
                | Token::MultiplyAssign
                | Token::DivideAssign
                | Token::ModAssign
                | Token::AndAssign
                | Token::XorAssign
                | Token::OrAssign
                | Token::ShiftLeftAssign
                | Token::ShiftRightAssign => {
                    let operator_token = read(tokens)?;
                    let right =
                        ExpressionNode::parse_with_level(tokens, context, precedence, false)?;
                    left = if let Token::Assignment = operator_token {
                        ExpressionNode::Assignment(Box::new(left), Box::new(right.unwrap()))
                    } else {
                        let operator = match operator_token {
                            Token::AddAssign => BinaryOperatorNode::Add,
                            Token::SubtractAssign => BinaryOperatorNode::Subtract,
                            Token::MultiplyAssign => BinaryOperatorNode::Multiply,
                            Token::DivideAssign => BinaryOperatorNode::Divide,
                            Token::ModAssign => BinaryOperatorNode::Mod,
                            Token::AndAssign => BinaryOperatorNode::BitwiseAnd,
                            Token::XorAssign => BinaryOperatorNode::BitwiseXor,
                            Token::OrAssign => BinaryOperatorNode::BitwiseOr,
                            Token::ShiftLeftAssign => BinaryOperatorNode::ShiftLeft,
                            Token::ShiftRightAssign => BinaryOperatorNode::ShiftRight,
                            _ => unreachable!(
                                "Can't use {:?} as an assignment operator",
                                operator_token
                            ),
                        };
                        ExpressionNode::Assignment(
                            Box::new(left.clone()),
                            Box::new(ExpressionNode::Binary(
                                operator,
                                Box::new(left),
                                Box::new(right.unwrap()),
                            )),
                        )
                    };
                }
                Token::Increment | Token::Decrement => {
                    left = ExpressionNode::Unary(
                        UnaryOperatorNode::parse_as_suffix(tokens, context)?,
                        Box::new(left),
                    );
                }
                Token::Question => {
                    expect(tokens, Token::Question)?;
                    // parse this expression with precedence level reset
                    let middle = ExpressionNode::parse(tokens, context)?;
                    expect(tokens, Token::Colon)?;
                    let end = ExpressionNode::parse_with_level(tokens, context, precedence, false)?;
                    left = ExpressionNode::Ternary(
                        Box::new(left),
                        Box::new(middle),
                        Box::new(end.unwrap()),
                    )
                }
                _ => {
                    left = ExpressionNode::Binary(
                        BinaryOperatorNode::parse(tokens, context)?,
                        Box::new(left),
                        Box::new(
                            ExpressionNode::parse_with_level(
                                tokens,
                                context,
                                precedence + 1,
                                false,
                            )?
                            .unwrap(),
                        ),
                    );
                }
            }
        }

        Ok(Some(left))
    }

    fn parse_factor(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let expression = match peek(tokens)? {
            Token::IntegerConstant(value) => {
                expect(tokens, Token::IntegerConstant(0))?;
                Some(ExpressionNode::IntegerConstant(value))
            }
            Token::OpenParen => {
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Some(expression)
            }
            Token::Identifier(name) => {
                expect(tokens, Token::Identifier("".to_string()))?;
                match peek(tokens)? {
                    // this is a function call !!
                    Token::OpenParen => {
                        expect(tokens, Token::OpenParen)?;
                        let mut arguments: Vec<ExpressionNode> = Vec::new();
                        if !matches!(peek(tokens)?, Token::CloseParen) {
                            arguments.push(ExpressionNode::parse(tokens, context)?);
                        }
                        while matches!(peek(tokens)?, Token::Comma) {
                            expect(tokens, Token::Comma)?;
                            arguments.push(ExpressionNode::parse(tokens, context)?);
                        }
                        expect(tokens, Token::CloseParen)?;

                        Some(ExpressionNode::FunctionCall(name, arguments))
                    }
                    _ => {
                        if context.do_not_validate {
                            Some(ExpressionNode::Var(name.to_string()))
                        } else {
                            // VALIDATION STEP: Check the variable has been declared
                            Some(ExpressionNode::Var(if let Some(new_name) =
                                context.current_scope_variables.get(&name)
                            {
                                Ok::<String, Box<dyn Error>>(new_name.to_string())
                            } else if let Some(new_name) = context.outer_scope_variables.get(&name)
                            {
                                Ok(new_name.to_string())
                            } else {
                                Err(format!("Variable used before declaration: {}", name).into())
                            }?))
                        }
                    }
                }
            }
            Token::Hyphen | Token::Tilde | Token::Not | Token::Increment | Token::Decrement => {
                let operator = UnaryOperatorNode::parse(tokens, context)?;
                let precedence = UnaryOperatorNode::precedence(); // all unary operators have the
                                                                  // same precedence
                let expression =
                    ExpressionNode::parse_with_level(tokens, context, precedence, false)?;
                Some(ExpressionNode::Unary(
                    operator,
                    Box::new(expression.unwrap()),
                ))
            }
            t => {
                if allow_empty {
                    None
                } else {
                    return Err(format!("Invalid token at start of expression: {:?}", t).into());
                }
            }
        };

        Ok(expression)
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(self, ExpressionNode::Var(_))
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
            Token::AndAssign => 1,
            Token::XorAssign => 1,
            Token::OrAssign => 1,
            Token::ShiftLeftAssign => 1,
            Token::ShiftRightAssign => 1,

            _ => return Ok(None),
        }))
    }
}
