use super::{
    BlockItemNode, ExpressionNode, ExpressionWithoutType, Parse, ParseContext, StatementNode,
};
use crate::compiler::lexer::{Token, TokenVector};
use std::{collections::VecDeque, error::Error};

impl Parse<StatementNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<StatementNode, Box<dyn Error>> {
        match self.peek()? {
            Token::KeywordReturn => {
                self.expect(Token::KeywordReturn)?;
                let expression = self.parse(context)?;
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::Return(expression))
            }
            Token::KeywordIf => {
                self.expect(Token::KeywordIf)?;
                self.expect(Token::OpenParen)?;
                let condition = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                let then = self.parse(context)?;
                let otherwise = match self.peek()? {
                    Token::KeywordElse => {
                        self.expect(Token::KeywordElse)?;
                        Some(self.parse(context)?)
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
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::Pass)
            }
            Token::KeywordGoto => {
                self.expect(Token::KeywordGoto)?;
                let s = match self.read()? {
                    Token::Identifier(s) => Ok::<String, Box<dyn Error>>(s),
                    t => Err(format!("unexpected token in goto: {:?}", t).into()),
                }?;
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::Goto(s))
            }
            Token::OpenBrace => {
                let block = self.parse(context)?;
                Ok(StatementNode::Compound(block))
            }
            Token::KeywordFor => {
                self.expect(Token::KeywordFor)?;
                self.expect(Token::OpenParen)?;

                // create a new scope just for the first line of the 'for' declaration
                let outer_scope = BlockItemNode::enter_scope(context);

                let init = self.parse(context)?;
                let cond = self.parse(context)?;
                self.expect(Token::SemiColon)?;
                let post = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                let body = self.parse(context)?;

                BlockItemNode::leave_scope(outer_scope, context);
                Ok(StatementNode::For(init, cond, post, Box::new(body), None))
            }
            Token::KeywordDo => {
                self.expect(Token::KeywordDo)?;
                let body = self.parse(context)?;
                self.expect(Token::KeywordWhile)?;
                self.expect(Token::OpenParen)?;
                let expression: ExpressionWithoutType = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::DoWhile(
                    Box::new(body),
                    expression.into(),
                    None,
                ))
            }
            Token::KeywordWhile => {
                self.expect(Token::KeywordWhile)?;
                self.expect(Token::OpenParen)?;
                let expression: ExpressionWithoutType = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                Ok(StatementNode::While(
                    expression.into(),
                    Box::new(self.parse(context)?),
                    None,
                ))
            }
            Token::KeywordBreak => {
                self.expect(Token::KeywordBreak)?;
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::Break(None))
            }
            Token::KeywordContinue => {
                self.expect(Token::KeywordContinue)?;
                self.expect(Token::SemiColon)?;
                Ok(StatementNode::Continue(None))
            }
            Token::KeywordSwitch => {
                self.expect(Token::KeywordSwitch)?;
                self.expect(Token::OpenParen)?;
                let expression: ExpressionWithoutType = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                Ok(StatementNode::Switch(
                    expression.into(),
                    Box::new(self.parse(context)?),
                    None,
                    None,
                ))
            }
            Token::KeywordCase => {
                self.expect(Token::KeywordCase)?;
                let expression: ExpressionWithoutType = self.parse(context)?;
                self.expect(Token::Colon)?;
                Ok(StatementNode::Case(
                    expression.into(),
                    Box::new(self.parse(context)?),
                    None,
                ))
            }
            Token::KeywordDefault => {
                self.expect(Token::KeywordDefault)?;
                self.expect(Token::Colon)?;
                Ok(StatementNode::Default(Box::new(self.parse(context)?), None))
            }
            _ => match (
                self.peek()?,
                self.get(1)
                    .ok_or::<Box<dyn Error>>("Statement node only has one token".into())?,
            ) {
                (Token::Identifier(s), Token::Colon) => {
                    self.expect(Token::Identifier("".to_string()))?;
                    self.expect(Token::Colon)?;
                    Ok(StatementNode::Label(
                        s.to_string(),
                        Box::new(self.parse(context)?),
                    ))
                }
                _ => {
                    let expression: ExpressionNode = self.parse(context)?;
                    self.expect(Token::SemiColon)?;
                    Ok(StatementNode::Expression(expression))
                }
            },
        }
    }
}
