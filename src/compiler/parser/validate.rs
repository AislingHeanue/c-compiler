use itertools::process_results;
use std::error::Error;

use super::{
    Block, BlockItemNode, DeclarationNode, ExpressionNode, ProgramNode, StatementNode,
    UnaryOperatorNode, Validate, ValidateContext,
};

impl Validate for ProgramNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        self.function.body = self.function.body.validate(context)?;
        Ok(self)
    }
}

impl Validate for Block {
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|i| match i {
                BlockItemNode::Declaration(d) => {
                    Ok(BlockItemNode::Declaration(d.validate(context)?))
                }
                BlockItemNode::Statement(s) => Ok(BlockItemNode::Statement(s.validate(context)?)),
            }),
            |iter| iter.collect(),
        )
    }
}

impl Validate for DeclarationNode {
    fn validate(self, _context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        Ok(self)
    }
}

impl Validate for StatementNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            StatementNode::Expression(e) => {
                self = StatementNode::Expression(e.validate(context)?);
            }
            StatementNode::Pass => {}
            StatementNode::Return(e) => {
                self = StatementNode::Return(e.validate(context)?);
            }
            StatementNode::If(condition, then, otherwise) => {
                let new_other = match *otherwise {
                    Some(other) => Some(other.validate(context)?),
                    None => None,
                };
                self = StatementNode::If(
                    condition.validate(context)?,
                    Box::new(then.validate(context)?),
                    Box::new(new_other),
                )
            }
            StatementNode::Label(s, statement) => {
                self = StatementNode::Label(s, Box::new(statement.validate(context)?))
            }
            StatementNode::Goto(_) => {}
        }
        Ok(self)
    }
}

impl Validate for ExpressionNode {
    fn validate(mut self, _context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            ExpressionNode::IntegerConstant(_) => {}
            ExpressionNode::Unary(op, src) => {
                // VALIDATION: Make sure ++ and -- only operate on variables, not constants or
                // other expressions
                match op {
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => {
                        if !src.is_lvalue() {
                            return Err(format!(
                                "Can't perform suffix operation on non-variable: {:?}",
                                src,
                            )
                            .into());
                        }
                    }
                    _ => {}
                }
                self = ExpressionNode::Unary(op, Box::new(src.validate(_context)?))
            }
            ExpressionNode::Binary(op, left, right) => {
                self = ExpressionNode::Binary(
                    op,
                    Box::new(left.validate(_context)?),
                    Box::new(right.validate(_context)?),
                )
            }
            ExpressionNode::Var(_) => {}
            ExpressionNode::Assignment(dst, src) => {
                self = ExpressionNode::Assignment(
                    Box::new(dst.validate(_context)?),
                    Box::new(src.validate(_context)?),
                )
            }
            ExpressionNode::Ternary(condition, then, otherwise) => {
                self = ExpressionNode::Ternary(
                    Box::new(condition.validate(_context)?),
                    Box::new(then.validate(_context)?),
                    Box::new(otherwise.validate(_context)?),
                )
            }
        }
        Ok(self)
    }
}
