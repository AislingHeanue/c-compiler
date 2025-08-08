use super::{
    expect, peek, read, BlockItemNode, ExpressionNode, ExpressionWithoutType, ForInitialiserNode,
    Parse, ParseContext, StatementNode,
};
use crate::compiler::lexer::Token;
use std::{collections::VecDeque, error::Error};

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
                let s = match read(tokens)? {
                    Token::Identifier(s) => Ok::<String, Box<dyn Error>>(s),
                    t => Err(format!("unexpected token in goto: {:?}", t).into()),
                }?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Goto(s))
            }
            Token::OpenBrace => {
                let block = <Vec<BlockItemNode>>::parse(tokens, context)?;
                Ok(StatementNode::Compound(block))
            }
            Token::KeywordFor => {
                expect(tokens, Token::KeywordFor)?;
                expect(tokens, Token::OpenParen)?;

                // create a new scope just for the first line of the 'for' declaration
                let outer_scope = BlockItemNode::enter_scope(context);

                let init = ForInitialiserNode::parse(tokens, context)?;
                let cond = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                let post = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;

                BlockItemNode::leave_scope(outer_scope, context);
                Ok(StatementNode::For(init, cond, post, Box::new(body), None))
            }
            Token::KeywordDo => {
                expect(tokens, Token::KeywordDo)?;
                let body = StatementNode::parse(tokens, context)?;
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::DoWhile(
                    Box::new(body),
                    expression.into(),
                    None,
                ))
            }
            Token::KeywordWhile => {
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;
                Ok(StatementNode::While(
                    expression.into(),
                    Box::new(body),
                    None,
                ))
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
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Ok(StatementNode::Switch(
                    expression.into(),
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                    None,
                ))
            }
            Token::KeywordCase => {
                expect(tokens, Token::KeywordCase)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::Colon)?;
                Ok(StatementNode::Case(
                    expression.into(),
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
